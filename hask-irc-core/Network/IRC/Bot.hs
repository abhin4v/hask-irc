{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}

module Network.IRC.Bot
  ( In
  , sendCommandLoop
  , readMessageLoop
  , messageProcessLoop )
where

import qualified Data.Text.Format  as TF
import qualified System.Log.Logger as HSL

import ClassyPrelude
import Control.Concurrent.Lifted  (threadDelay)
import Control.Exception.Lifted   (evaluate)
import Control.Monad.State.Strict (get, put)
import Data.Time                  (addUTCTime)
import System.IO                  (hIsEOF)
import System.Timeout             (timeout)
import System.Log.Logger.TH       (deriveLoggers)

import qualified Network.IRC.Configuration as CF
import Network.IRC.MessageBus
import Network.IRC.Internal.Types
import Network.IRC.Protocol
import Network.IRC.Types
import Network.IRC.Util

$(deriveLoggers "HSL" [HSL.INFO, HSL.ERROR])

data RawIn = Line !UTCTime !Text | EOS deriving (Show, Eq)
data In    = Timeout | EOD | Msg !Message deriving (Show, Eq)

formatCommand :: (Exception e) => BotConfig -> Message -> IO ([e], [Text])
formatCommand botConfig@BotConfig { .. } message =
  map (second catMaybes . partitionEithers)
  . forM (defaultCommandFormatter : cmdFormatters) $ \formatter ->
      try . evaluate $ formatter botConfig message

parseLine :: (Exception e)
          => BotConfig -> UTCTime -> Text -> Map MessageParserId [MessagePart]
          -> IO ([e], [Message], Map MessageParserId [MessagePart])
parseLine botConfig@BotConfig { .. } time line msgParts =
  map mconcat . forM parsers $ \MessageParser { .. } -> do
    let parserMsgParts    = concat . maybeToList $ lookup msgParserId msgParts
    let parserMsgPartsMap = singletonMap msgParserId parserMsgParts
    eresult <- try . evaluate $ msgParser botConfig time line parserMsgParts
    return $ case eresult of
      Left e                              -> ([e], []       , parserMsgPartsMap)
      Right ParseReject                   -> ([] , []       , parserMsgPartsMap)
      Right (ParsePartial msgParts')      -> ([] , []       , singletonMap msgParserId msgParts')
      Right (ParseDone message msgParts') -> ([] , [message], singletonMap msgParserId msgParts')
  where
    parsers = defaultParsers ++ msgParsers

sendCommandLoop :: MessageChannel Message -> Bot -> IO ()
sendCommandLoop commandChan bot@Bot { .. } = do
  msg@(Message _ _ cmd) <- receiveMessage commandChan
  (exs, lines_)         <- formatCommand botConfig msg

  forM_ exs $ \(ex :: SomeException) ->
    errorM ("Error while formatting command: " ++ show cmd ++ "\nError: " ++ show ex)

  forM_ lines_ $ \line ->
    handle (\(e :: SomeException) -> do
              errorM ("Error while writing to connection: " ++ show e)
              closeMessageChannel commandChan) $ do
      TF.hprint botSocket "{}\r\n" $ TF.Only line
      infoM . unpack $ "> " ++ line

  commandChanClosed <- isClosedMessageChannel commandChan
  unless commandChanClosed $
    case fromMessage cmd of
      Just QuitCmd -> closeMessageChannel commandChan
      _            -> sendCommandLoop commandChan bot

readMessageLoop :: MVar BotStatus -> MessageChannel In -> Bot -> Int -> IO ()
readMessageLoop mvBotStatus inChan Bot { .. } timeoutDelay = loop mempty
  where
    msgPartTimeout = 10

    loop msgParts = do
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
                (exs, msgs, msgParts') <- parseLine botConfig time line msgParts

                forM_ exs $ \(ex :: SomeException) ->
                  errorM ("Error while parsing line: " ++ unpack line ++ "\nError: " ++ show ex)
                forM_ msgs $ sendMessage inChan . Msg

                return msgParts'
              Right (Just EOS)              -> sendMessage inChan EOD >> return msgParts

          limit <- io $ map (addUTCTime (- msgPartTimeout)) getCurrentTime
          loop $ validMsgParts limit msgParts'

    validMsgParts limit =
      foldl' (\m (k, v) -> insertWith (++) k [v] m) mempty
      . concat
      . filter ((> limit) . msgPartTime . snd . headEx . sortBy (flip $ comparing (msgPartTime . snd)))
      . groupAllOn (fst &&& msgPartTarget . snd)
      . asList
      . concatMap (uncurry (map . (,)))
      . mapToList

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
      status       <- get
      Bot { .. }   <- ask
      let nick     = botNick botConfig
      let origNick = botOrigNick botConfig
      let mpass    = CF.lookup "password" (config botConfig)

      nStatus <- io . mask_ $
        if idleFor >= (oneSec * botTimeout botConfig)
          then infoM "Timeout" >> return Disconnected
          else do
            when (status == Kicked) $
              threadDelay (5 * oneSec) >> (sendMessage messageChan =<< newMessage JoinCmd)

            mIn <- receiveMessage inChan
            case mIn of
              Timeout                  -> do
                sendMessage messageChan =<< newMessage IdleMsg
                sendWhoisMessage nick origNick idleFor
                return Idle
              EOD                      -> infoM "Connection closed" >> return Disconnected
              Msg (msg@Message { .. }) -> do
                nStatus <- handleMsg nick origNick message mpass
                sendMessage messageChan msg
                return nStatus

      put nStatus
      case nStatus of
        Idle             -> loop (idleFor + oneSec)
        Disconnected     -> return ()
        NickNotAvailable -> return ()
        NickAvailable    -> return ()
        _                -> loop 0

    sendWhoisMessage nick origNick idleFor =
      when (nick /= origNick && idleFor /= 0 && idleFor `mod` (10 * oneSec) == 0) $
        sendMessage messageChan =<< (newMessage . WhoisCmd . nickToText $ origNick)

    handleMsg nick origNick message mpass
      | Just (JoinMsg user)     <- fromMessage message, userNick user == nick =
          infoM "Joined" >> return Joined
      | Just (KickMsg { .. })   <- fromMessage message, kickedNick == nick    =
          infoM "Kicked" >> return Kicked
      | Just NickInUseMsg       <- fromMessage message                        =
          infoM "Nick already in use" >> return NickNotAvailable
      | Just (ModeMsg { .. })   <- fromMessage message, modeUser == Self      = do
          whenJust mpass $ \pass -> do
            msg <- newMessage $ PrivMsgReply (User (Nick "NickServ") "") $ "IDENTIFY " ++ pass
            sendMessage messageChan msg
          sendMessage messageChan =<< newMessage JoinCmd
          return Connected
      | Just (WhoisNoSuchNickMsg n) <- fromMessage message, n == origNick     =
          infoM "Original nick available" >> return NickAvailable
      | otherwise                                                             =
          return Connected
