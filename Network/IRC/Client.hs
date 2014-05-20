{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.IRC.Client (run) where

import qualified Data.Text.Format as TF
import qualified Data.Text.Format.Params as TF

import ClassyPrelude
import Control.Exception.Lifted
import Control.Concurrent.Lifted
import Control.Monad.Reader hiding (forM_, foldM)
import Control.Monad.State hiding (forM_, foldM)
import Network
import System.IO (hIsEOF, hSetBuffering, BufferMode(..))
import System.Timeout

import Network.IRC.Handlers
import Network.IRC.Protocol
import Network.IRC.Types

oneSec :: Int
oneSec = 1000000

debug :: Text -> IO ()
debug msg = do
  time <- getCurrentTime
  TF.print "[{}] {}\n" $ TF.buildParams (formatTime defaultTimeLocale "%F %T" time, msg)

type Latch = MVar ()

latchIt :: Latch -> IO ()
latchIt latch = putMVar latch ()

awaitLatch :: Latch -> IO ()
awaitLatch latch = void $ takeMVar latch

type EChannel a = (Chan a, Latch)

data Line = Timeout | EOF | Line !Message deriving (Show, Eq)

sendCommandLoop :: EChannel Command -> Bot -> IO ()
sendCommandLoop (commandChan, latch) bot@Bot { .. } = do
  cmd       <- readChan commandChan
  time      <- getCurrentTime
  let mline = lineFromCommand botConfig cmd
  case mline of
    Nothing   -> return ()
    Just line -> do
      TF.hprint socket "{}\r\n" $ TF.Only line
      TF.print "[{}] > {}\n" $ TF.buildParams (formatTime defaultTimeLocale "%F %T" time, line)
  case cmd of
    QuitCmd -> latchIt latch
    _       -> sendCommandLoop (commandChan, latch) bot

sendCommand :: Chan Command -> Command -> IO ()
sendCommand = writeChan

readLineLoop :: MVar BotStatus -> EChannel Line -> Bot -> Int -> IO ()
readLineLoop mvBotStatus (lineChan, latch) bot@Bot { .. } timeoutDelay = do
  botStatus <- readMVar mvBotStatus
  case botStatus of
    Disconnected -> latchIt latch
    _            -> do
      mLine <- timeout timeoutDelay readLine'
      case mLine of
        Nothing   -> writeChan lineChan Timeout
        Just line -> writeChan lineChan line
      readLineLoop mvBotStatus (lineChan, latch) bot timeoutDelay
      where
        readLine' = do
          eof <- hIsEOF socket
          if eof
            then return EOF
            else do
              line <- map initEx $ hGetLine socket
              debug $ "< " ++ line
              now <- getCurrentTime
              return . Line $ msgFromLine botConfig now line

readLine :: Chan Line -> IO Line
readLine = readChan

sendMessage :: Chan Line -> Message -> IO ()
sendMessage = (. Line) . writeChan

listenerLoop :: Chan Line -> Chan Command -> Int -> IRC ()
listenerLoop lineChan commandChan !idleFor = do
  status         <- get
  bot@Bot { .. } <- ask
  let nick       = botNick botConfig

  nStatus <- liftIO . mask_ $
    if idleFor >= (oneSec * botTimeout botConfig)
      then debug "Timeout" >> return Disconnected
      else do
        when (status == Kicked) $
          threadDelay (5 * oneSec) >> sendCommand commandChan JoinCmd

        mLine <- readLine lineChan
        case mLine of
          Timeout      -> getCurrentTime >>= dispatchHandlers bot . IdleMsg  >> return Idle
          EOF          -> debug "Connection closed" >> return Disconnected
          Line message -> do
            nStatus <- case message of
              JoinMsg { .. } | userNick user == nick -> debug "Joined" >> return Joined
              KickMsg { .. } | kickedNick == nick    -> debug "Kicked" >> return Kicked
              NickInUseMsg { .. }                    ->
                debug "Nick already in use"     >> return NickNotAvailable
              ModeMsg { user = Self, .. }            ->
                sendCommand commandChan JoinCmd >> return Connected
              _                                      -> return Connected

            dispatchHandlers bot message
            return nStatus

  put nStatus
  case nStatus of
    Idle             -> listenerLoop lineChan commandChan (idleFor + oneSec)
    Disconnected     -> return ()
    NickNotAvailable -> return ()
    _                -> listenerLoop lineChan commandChan 0

  where
    dispatchHandlers Bot { .. } message =
      forM_ (mapToList msgHandlers) $ \(_, msgHandler) -> fork $
        handle (\(e :: SomeException) ->
                  debug $ "Exception while processing message: " ++ pack (show e)) $ do
          mCmd <- handleMessage msgHandler botConfig message
          case mCmd of
            Nothing  -> return ()
            Just cmd -> sendCommand commandChan cmd

sendEvent :: Chan SomeEvent -> SomeEvent -> IO ()
sendEvent = writeChan

eventProcessLoop :: EChannel SomeEvent -> Chan Line -> Chan Command -> Bot -> IO ()
eventProcessLoop (eventChan, latch) lineChan commandChan bot@Bot {.. } = do
  event <- readChan eventChan
  case fromEvent event of
    Just (QuitEvent, _) -> latchIt latch
    _                   -> do
      debug $ "** Event: " ++ pack (show event)
      forM_ (mapToList msgHandlers) $ \(_, msgHandler) -> fork $
        handle (\(ex :: SomeException) ->
                  debug $ "Exception while processing event: " ++ pack (show ex)) $ do
          resp <- handleEvent msgHandler botConfig event
          case resp of
            RespMessage message -> sendMessage lineChan message
            RespCommand command -> sendCommand commandChan command
            RespEvent event'    -> sendEvent eventChan event'
            _                   -> return ()
      eventProcessLoop (eventChan, latch) lineChan commandChan bot

loadMsgHandlers :: BotConfig -> Chan SomeEvent -> IO (Map MsgHandlerName MsgHandler)
loadMsgHandlers botConfig@BotConfig { .. } eventChan =
  flip (`foldM` mempty) msgHandlerNames $ \hMap msgHandlerName -> do
    debug $ "Loading msg handler: " ++ msgHandlerName
    mMsgHandler <- mkMsgHandler botConfig eventChan msgHandlerName
    case mMsgHandler of
      Nothing         -> debug ("No msg handler found with name: " ++ msgHandlerName) >> return hMap
      Just msgHandler -> return $ insertMap msgHandlerName msgHandler hMap

unloadMsgHandlers :: Bot -> IO ()
unloadMsgHandlers Bot { .. } =
  forM_ (mapToList msgHandlers) $ \(msgHandlerName, msgHandler) -> do
    debug $ "Unloading msg handler: " ++ msgHandlerName
    stopMsgHandler msgHandler botConfig

connect :: BotConfig -> IO (Bot, MVar BotStatus, EChannel Line, EChannel Command, EChannel SomeEvent)
connect botConfig@BotConfig { .. } = do
  debug "Connecting ..."
  socket <- connectToWithRetry
  hSetBuffering socket LineBuffering
  debug "Connected"

  lineChan    <- newChannel
  commandChan <- newChannel
  eventChan   <- newChannel
  mvBotStatus <- newMVar Connected

  msgHandlers <- loadMsgHandlers botConfig (fst eventChan)

  return (Bot botConfig socket msgHandlers, mvBotStatus, lineChan, commandChan, eventChan)
  where
    connectToWithRetry = connectTo (unpack server) (PortNumber (fromIntegral port))
                           `catch` (\(e :: SomeException) -> do
                                      debug ("Error while connecting: " ++ pack (show e) ++ ". Waiting.")
                                      threadDelay (5 * oneSec)
                                      connectToWithRetry)

    newChannel = (,) <$> newChan <*> newEmptyMVar

disconnect :: (Bot, MVar BotStatus, EChannel Line, EChannel Command, EChannel SomeEvent) -> IO ()
disconnect (bot@Bot { .. }, mvBotStatus, (_, readLatch), (commandChan, sendLatch), (eventChan, eventLatch)) = do
  debug "Disconnecting ..."
  sendCommand commandChan QuitCmd
  awaitLatch sendLatch
  swapMVar mvBotStatus Disconnected
  awaitLatch readLatch
  sendEvent eventChan =<< toEvent QuitEvent
  awaitLatch eventLatch

  unloadMsgHandlers bot
  hClose socket
  debug "Disconnected"

addCoreMsgHandlers :: BotConfig -> BotConfig
addCoreMsgHandlers botConfig =
  botConfig { msgHandlerNames = hashNub $ msgHandlerNames botConfig ++ coreMsgHandlerNames }

run :: BotConfig -> IO ()
run botConfig' = withSocketsDo $ do
  hSetBuffering stdout LineBuffering
  debug "Running with config:"
  print botConfig
  status <- run_
  case status of
    Disconnected     -> debug "Restarting .." >> run botConfig
    Errored          -> debug "Restarting .." >> run botConfig
    Interrupted      -> return ()
    NickNotAvailable -> return ()
    _                -> error "Unsupported status"
  where
    botConfig = addCoreMsgHandlers botConfig'

    handleErrors :: SomeException -> IO BotStatus
    handleErrors e = case fromException e of
        Just UserInterrupt -> debug "User interrupt"                 >> return Interrupted
        _                  -> debug ("Exception! " ++ pack (show e)) >> return Errored

    run_ = bracket (connect botConfig) disconnect $
      \(bot, mvBotStatus, (lineChan, readLatch), (commandChan, sendLatch), eventChannel) ->
        handle handleErrors $ do
          sendCommand commandChan NickCmd
          sendCommand commandChan UserCmd

          fork $ sendCommandLoop (commandChan, sendLatch) bot
          fork $ readLineLoop mvBotStatus (lineChan, readLatch) bot oneSec
          fork $ eventProcessLoop eventChannel lineChan commandChan bot
          runIRC bot Connected (listenerLoop lineChan commandChan 0)
