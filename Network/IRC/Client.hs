{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.IRC.Client (run) where

import qualified Data.Text.Format as TF
import qualified Data.Text.Format.Params as TF

import ClassyPrelude
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

data Cmd  = CmdQuit | Cmd !Command deriving (Show, Eq)
data Line = Timeout | EOF | Line !Message deriving (Show, Eq)

sendCommandLoop :: EChannel Cmd -> Bot -> IO ()
sendCommandLoop (commandChan, latch) bot@Bot { .. } = do
  cmd <- readChan commandChan
  case cmd of
    CmdQuit     -> latchIt latch
    Cmd command -> do
      time <- getCurrentTime
      let line = lineFromCommand botConfig command
      TF.hprint socket "{}\r\n" $ TF.Only line
      TF.print "[{}] > {}\n" $ TF.buildParams (formatTime defaultTimeLocale "%F %T" time, line)
      sendCommandLoop (commandChan, latch) bot

sendCommand :: Chan Cmd -> Cmd -> IO ()
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
              now <- getCurrentTime
              return . Line $ msgFromLine botConfig now line

readLine :: Chan Line -> IO Line
readLine = readChan

sendMessage :: Chan Line -> Message -> IO ()
sendMessage = (. Line) . writeChan

listenerLoop :: Chan Line -> Chan Cmd -> Int -> IRC ()
listenerLoop lineChan commandChan idleFor = do
  status <- get
  bot@Bot { .. } <- ask
  let nick  = botNick botConfig

  nStatus <- liftIO $
    if idleFor >= (oneSec * botTimeout botConfig)
      then return Disconnected
      else do
        when (status == Kicked) $
          threadDelay (5 * oneSec) >> sendCommand commandChan (Cmd JoinCmd)

        mLine <- readLine lineChan
        case mLine of
          Timeout      -> dispatchHandlers bot IdleMsg >> return Idle
          EOF          -> return Disconnected
          Line message -> do
            debug $ "< " ++ msgLine message
            nStatus <- case message of
              JoinMsg { .. } | userNick user == nick -> debug "Joined" >> return Joined
              KickMsg { .. } | kickedNick == nick    -> debug "Kicked" >> return Kicked
              ModeMsg { user = Self, .. }            ->
                sendCommand commandChan (Cmd JoinCmd) >> return Connected
              _                                      -> return Connected

            dispatchHandlers bot message
            return nStatus

  put nStatus
  case nStatus of
    Idle         -> listenerLoop lineChan commandChan (idleFor + oneSec)
    Disconnected -> return ()
    _            -> listenerLoop lineChan commandChan 0

  where
    dispatchHandlers Bot { .. } message =
      forM_ (mapToList msgHandlers) $ \(_, msgHandler) -> fork $
        handle (\(e :: SomeException) -> debug $ "Exception! " ++ pack (show e)) $ do
          mCmd <- runMsgHandler msgHandler botConfig message
          case mCmd of
            Nothing  -> return ()
            Just cmd -> case cmd of
              MessageCmd msg -> sendMessage lineChan msg
              _              -> sendCommand commandChan (Cmd cmd)

loadMsgHandlers :: BotConfig -> IO (Map MsgHandlerName MsgHandler)
loadMsgHandlers botConfig@BotConfig { .. } =
  flip (`foldM` mempty) msgHandlerNames $ \hMap msgHandlerName -> do
    debug $ "Loading msg handler: " ++ msgHandlerName
    mMsgHandler <- mkMsgHandler botConfig msgHandlerName
    case mMsgHandler of
      Nothing         -> debug ("No msg handler found with name: " ++ msgHandlerName) >> return hMap
      Just msgHandler -> return $ insertMap msgHandlerName msgHandler hMap

unloadMsgHandlers :: Bot -> IO ()
unloadMsgHandlers Bot { .. } =
  forM_ (mapToList msgHandlers) $ \(msgHandlerName, msgHandler) -> do
    debug $ "Unloading msg handler: " ++ msgHandlerName
    stopMsgHandler msgHandler botConfig

connect :: BotConfig -> IO (Bot, MVar BotStatus, EChannel Line, EChannel Cmd)
connect botConfig@BotConfig { .. } = do
  debug "Connecting ..."
  socket <- connectToWithRetry
  hSetBuffering socket LineBuffering
  msgHandlers <- loadMsgHandlers botConfig
  debug "Connected"

  lineChan    <- newChan
  commandChan <- newChan
  sendLatch   <- newEmptyMVar
  readLatch   <- newEmptyMVar
  mvBotStatus <- newMVar Connected

  return (Bot botConfig socket msgHandlers, mvBotStatus, (lineChan, readLatch), (commandChan, sendLatch))
  where
    connectToWithRetry = connectTo (unpack server) (PortNumber (fromIntegral port))
                           `catch` (\(e :: SomeException) -> do
                                      debug ("Error while connecting: " ++ pack (show e) ++ ". Waiting.")
                                      threadDelay (5 * oneSec)
                                      connectToWithRetry)

disconnect :: (Bot, MVar BotStatus, EChannel Line, EChannel Cmd) -> IO ()
disconnect (bot@Bot { .. }, mvBotStatus, (_, readLatch), (commandChan, sendLatch)) = do
  debug "Disconnecting ..."
  sendCommand commandChan CmdQuit
  awaitLatch sendLatch
  swapMVar mvBotStatus Disconnected
  awaitLatch readLatch

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
    Disconnected -> debug "Connection timed out" >> run botConfig
    Errored      -> return ()
    _            -> error "Unsupported status"
  where
    botConfig = addCoreMsgHandlers botConfig'

    handleErrors :: SomeException -> IO BotStatus
    handleErrors e = do
      debug $ "Exception! " ++ pack (show e)
      return Errored

    run_ = bracket (connect botConfig) disconnect $
      \(bot, mvBotStatus, (lineChan, readLatch), (commandChan, sendLatch)) ->
        handle handleErrors $ do
          sendCommand commandChan (Cmd NickCmd)
          sendCommand commandChan (Cmd UserCmd)

          fork $ sendCommandLoop (commandChan, sendLatch) bot
          fork $ readLineLoop mvBotStatus (lineChan, readLatch) bot oneSec
          runIRC bot Connected (listenerLoop lineChan commandChan 0)
