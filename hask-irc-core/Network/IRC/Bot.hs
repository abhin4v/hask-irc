{-# LANGUAGE TemplateHaskell #-}

module Network.IRC.Bot
  ( In
  , sendCommandLoop
  , readMessageLoop
  , messageProcessLoop )
where

import qualified Data.Configurator as CF
import qualified Data.Text.Format  as TF
import qualified System.Log.Logger as HSL

import ClassyPrelude
import Control.Concurrent.Lifted  (threadDelay)
import Control.Exception.Lifted   (mask_, mask)
import Control.Monad.State.Strict (get, put, evalStateT)
import Data.Time                  (addUTCTime)
import System.IO                  (hIsEOF)
import System.Timeout             (timeout)
import System.Log.Logger.TH       (deriveLoggers)

import Network.IRC.MessageBus
import Network.IRC.Internal.Types
import Network.IRC.Protocol
import Network.IRC.Types
import Network.IRC.Util

$(deriveLoggers "HSL" [HSL.INFO, HSL.ERROR])

data RawIn = Line !UTCTime !Text | EOS deriving (Show, Eq)
data In    = Timeout | EOD | Msg !Message deriving (Show, Eq)

sendCommandLoop :: MessageChannel Message -> Bot -> IO ()
sendCommandLoop commandChan bot@Bot { .. } = do
  msg@(Message _ _ cmd) <- receiveMessage commandChan
  let mline = formatCommand botConfig msg
  handle (\(e :: SomeException) ->
            errorM ("Error while writing to connection: " ++ show e) >> closeMessageChannel commandChan) $ do
    whenJust mline $ \line -> do
      TF.hprint botSocket "{}\r\n" $ TF.Only line
      infoM . unpack $ "> " ++ line
    case fromMessage cmd of
      Just QuitCmd -> closeMessageChannel commandChan
      _            -> sendCommandLoop commandChan bot

readMessageLoop :: MVar BotStatus -> MessageChannel In -> Bot -> Int -> IO ()
readMessageLoop mvBotStatus inChan Bot { .. } timeoutDelay = evalStateT loop mempty
  where
    msgPartTimeout = 10

    loop = do
      msgParts  <- get
      botStatus <- readMVar mvBotStatus
      case botStatus of
        Disconnected -> io $ closeMessageChannel inChan
        _            -> do
          msgParts' <- io $ do
            mLine <- try $ timeout timeoutDelay readLine
            case mLine of
              Left (e :: SomeException)     -> do
                errorM $ "Error while reading from connection: " ++ show e
                sendMessage inChan EOD >> return msgParts
              Right Nothing                 -> sendMessage inChan Timeout >> return msgParts
              Right (Just (Line time line)) -> do
                let (msgs, msgParts') = parseLine botConfig time line msgParts
                forM_ msgs $ sendMessage inChan . Msg
                return msgParts'
              Right (Just EOS)              -> sendMessage inChan EOD >> return msgParts

          limit <- io $ map (addUTCTime (- msgPartTimeout)) getCurrentTime
          put $ validMsgParts limit msgParts'
          loop
      where
        validMsgParts limit =
          foldl' (\m (k, v) -> insertWith (++) k [v] m) mempty
          . concat
          . filter ((> limit) . msgPartTime . snd . headEx . sortBy (flip $ comparing (msgPartTime . snd)))
          . groupAllOn (fst &&& msgPartTarget . snd)
          . asList . concatMap (uncurry (map . (,))) . mapToList

        readLine = do
          eof <- hIsEOF botSocket
          if eof
            then return EOS
            else mask $ \unmask -> do
              line <- map initEx . unmask $ hGetLine botSocket
              infoM . unpack $ "< " ++ line
              now <- getCurrentTime
              return $ Line now line

messageProcessLoop :: MessageChannel In -> MessageChannel Message -> IRC ()
messageProcessLoop inChan messageChan = loop 0
  where
    loop !idleFor = do
      status     <- get
      Bot { .. } <- ask
      let nick   = botNick botConfig
      mpass      <- io $ CF.lookup (config botConfig) "password"

      nStatus <- io . mask_ $
        if idleFor >= (oneSec * botTimeout botConfig)
          then infoM "Timeout" >> return Disconnected
          else do
            when (status == Kicked) $
              threadDelay (5 * oneSec) >> newMessage JoinCmd >>= sendMessage messageChan

            mIn <- receiveMessage inChan
            case mIn of
              Timeout -> newMessage IdleMsg >>= sendMessage messageChan >> return Idle
              EOD     -> infoM "Connection closed" >> return Disconnected
              Msg (msg@Message { .. }) -> do
                nStatus <- handleMsg nick message mpass
                sendMessage messageChan msg
                return nStatus

      put nStatus
      case nStatus of
        Idle             -> loop (idleFor + oneSec)
        Disconnected     -> return ()
        NickNotAvailable -> return ()
        _                -> loop 0

      where
        handleMsg nick message mpass
          | Just (JoinMsg user)   <- fromMessage message, userNick user == nick =
              infoM "Joined" >> return Joined
          | Just (KickMsg { .. }) <- fromMessage message, kickedNick == nick    =
              infoM "Kicked" >> return Kicked
          | Just NickInUseMsg     <- fromMessage message                        =
              infoM "Nick already in use" >> return NickNotAvailable
          | Just (ModeMsg { .. }) <- fromMessage message, modeUser == Self      = do
              whenJust mpass $ \pass -> do
                msg <- newMessage $ PrivMsgReply (User (Nick "NickServ") "") $ "IDENTIFY " ++ pass
                sendMessage messageChan msg
              newMessage JoinCmd >>= sendMessage messageChan
              return Connected
          | otherwise                                                           =
              return Connected
