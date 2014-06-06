{-# LANGUAGE TemplateHaskell #-}

module Network.IRC.Bot
  ( Line
  , sendCommand
  , sendMessage
  , sendEvent
  , readLine
  , sendCommandLoop
  , readLineLoop
  , messageProcessLoop
  , eventProcessLoop )
where

import qualified Data.Text.Format  as TF
import qualified System.Log.Logger as HSL

import ClassyPrelude
import Control.Concurrent.Lifted (fork, Chan, readChan, writeChan, threadDelay)
import Control.Exception.Lifted  (mask_, mask)
import Control.Monad.Reader      (ask)
import Control.Monad.State       (get, put)
import Data.Time                 (addUTCTime)
import System.IO                 (hIsEOF)
import System.Timeout            (timeout)
import System.Log.Logger.TH      (deriveLoggers)

import Network.IRC.Internal.Types
import Network.IRC.Protocol
import Network.IRC.Util

$(deriveLoggers "HSL" [HSL.DEBUG, HSL.INFO, HSL.ERROR])

data Line = Timeout | EOF | Line !UTCTime !Text | Msg Message deriving (Show, Eq)

sendCommand :: Chan Command -> Command -> IO ()
sendCommand = writeChan

sendMessage :: Chan Line -> Message -> IO ()
sendMessage = (. Msg) . writeChan

sendEvent :: Chan SomeEvent -> SomeEvent -> IO ()
sendEvent = writeChan

readLine :: Chan Line -> IO Line
readLine = readChan

sendCommandLoop :: Channel Command -> Bot -> IO ()
sendCommandLoop (commandChan, latch) bot@Bot { .. } = do
  cmd       <- readChan commandChan
  let mline = lineFromCommand botConfig cmd
  handle (\(e :: SomeException) ->
            errorM ("Error while writing to connection: " ++ show e) >> latchIt latch) $ do
    whenJust mline $ \line -> do
      TF.hprint botSocket "{}\r\n" $ TF.Only line
      infoM . unpack $ "> " ++ line
    case cmd of
      QuitCmd -> latchIt latch
      _       -> sendCommandLoop (commandChan, latch) bot

readLineLoop :: MVar BotStatus -> Channel Line -> Bot -> Int -> IO ()
readLineLoop = go []
  where
    msgPartTimeout = 10

    go !msgParts mvBotStatus (lineChan, latch) bot@Bot { .. } timeoutDelay = do
      botStatus <- readMVar mvBotStatus
      case botStatus of
        Disconnected -> latchIt latch
        _            -> do
          mLine     <- try $ timeout timeoutDelay readLine'
          msgParts' <- case mLine of
            Left (e :: SomeException)     -> do
              errorM $ "Error while reading from connection: " ++ show e
              writeChan lineChan EOF >> return msgParts
            Right Nothing                 -> writeChan lineChan Timeout >> return msgParts
            Right (Just (Line time line)) -> do
              let (mmsg, msgParts') = parseLine botConfig time line msgParts
              whenJust mmsg $ writeChan lineChan . Msg
              return msgParts'
            Right (Just l)                -> writeChan lineChan l >> return msgParts

          limit <- map (addUTCTime (- msgPartTimeout)) getCurrentTime
          let msgParts'' = concat
                           . filter ((> limit) . msgPartTime . headEx . sortBy (flip $ comparing msgPartTime))
                           . groupAllOn (msgParserType &&& msgPartTarget) $ msgParts'
          go msgParts'' mvBotStatus (lineChan, latch) bot timeoutDelay
      where
        readLine' = do
          eof <- hIsEOF botSocket
          if eof
            then return EOF
            else mask $ \unmask -> do
              line <- map initEx . unmask $ hGetLine botSocket
              infoM . unpack $ "< " ++ line
              now <- getCurrentTime
              return $ Line now line

messageProcessLoop :: Chan Line -> Chan Command -> IRC ()
messageProcessLoop = go 0
  where
    go !idleFor lineChan commandChan = do
      status         <- get
      bot@Bot { .. } <- ask
      let nick       = botNick botConfig

      nStatus <- io . mask_ $
        if idleFor >= (oneSec * botTimeout botConfig)
          then infoM "Timeout" >> return Disconnected
          else do
            when (status == Kicked) $
              threadDelay (5 * oneSec) >> sendCommand commandChan JoinCmd

            mLine <- readLine lineChan
            case mLine of
              Timeout      ->
                getCurrentTime >>= \t -> dispatchHandlers bot (Message t "" IdleMsg) >> return Idle
              EOF          -> infoM "Connection closed" >> return Disconnected
              Line _ _     -> error "This should never happen"
              Msg (message@Message { .. }) -> do
                nStatus <- case msgDetails of
                  JoinMsg { .. } | userNick user == nick -> infoM "Joined" >> return Joined
                  KickMsg { .. } | kickedNick == nick    -> infoM "Kicked" >> return Kicked
                  NickInUseMsg { .. }                    ->
                    infoM "Nick already in use"     >> return NickNotAvailable
                  ModeMsg { user = Self, .. }            ->
                    sendCommand commandChan JoinCmd >> return Connected
                  _                                      -> return Connected

                dispatchHandlers bot message
                return nStatus

      put nStatus
      case nStatus of
        Idle             -> go (idleFor + oneSec) lineChan commandChan
        Disconnected     -> return ()
        NickNotAvailable -> return ()
        _                -> go 0 lineChan commandChan

      where
        dispatchHandlers Bot { .. } message =
          forM_ (mapValues msgHandlers) $ \msgHandler -> void . fork $
            handle (\(e :: SomeException) ->
                      errorM $ "Exception while processing message: " ++ show e) $ do
              cmds <- handleMessage msgHandler botConfig message
              forM_ cmds (sendCommand commandChan)

eventProcessLoop :: Channel SomeEvent -> Chan Line -> Chan Command -> Bot -> IO ()
eventProcessLoop (eventChan, latch) lineChan commandChan bot@Bot {.. } = do
  event <- readChan eventChan
  case fromEvent event of
    Just (QuitEvent, _) -> latchIt latch
    _                   -> do
      debugM $ "Event: " ++ show event
      forM_ (mapValues msgHandlers) $ \msgHandler -> void . fork $
        handle (\(ex :: SomeException) ->
                  errorM $ "Exception while processing event: " ++ show ex) $ do
          resp <- handleEvent msgHandler botConfig event
          case resp of
            RespMessage messages -> forM_ messages $ sendMessage lineChan
            RespCommand commands -> forM_ commands $ sendCommand commandChan
            RespEvent events     -> forM_ events $ sendEvent eventChan
            _                    -> return ()
      eventProcessLoop (eventChan, latch) lineChan commandChan bot
