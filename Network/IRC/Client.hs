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

sendCommand :: Bot -> Command -> IO ()
sendCommand Bot { .. } reply = do
  time <- getCurrentTime
  let line = lineFromCommand botConfig reply
  TF.hprint socket "{}\r\n" $ TF.Only line
  TF.print "[{}] > {}\n" $ TF.buildParams (formatTime defaultTimeLocale "%F %T" time, line)

data Line = Timeout | EOF | Line !Text deriving (Show, Eq)

readLine :: Handle -> Int -> IO Line
readLine socket timeoutDelay = do
  mLine <- timeout timeoutDelay readLine'
  case mLine of
    Nothing   -> return Timeout
    Just line -> return line
  where
    readLine' = do
      eof <- hIsEOF socket
      if eof
        then return EOF
        else map Line $ hGetLine socket

listenerLoop :: Int -> IRC ()
listenerLoop idleFor = do
  status <- get
  bot@Bot { .. } <- ask
  let nick  = botNick botConfig

  nStatus <- liftIO $
    if idleFor >= (oneSec * botTimeout botConfig)
      then return Disconnected
      else do
        when (status == Kicked) $
          threadDelay (5 * oneSec) >> sendCommand bot JoinCmd

        mLine <- readLine socket oneSec
        case mLine of
          Timeout  -> dispatchHandlers bot IdleMsg >> return Idle
          EOF -> return Disconnected
          Line line' -> do
            let line = initEx line'
            now <- getCurrentTime
            debug $ "< " ++ line

            let message = msgFromLine botConfig now line
            nStatus <- case message of
              JoinMsg { .. } | userNick user == nick -> debug "Joined"          >> return Joined
              KickMsg { .. } | kickedNick == nick    -> debug "Kicked"          >> return Kicked
              ModeMsg { user = Self, .. }            -> sendCommand bot JoinCmd >> return Connected
              _                                      -> return Connected

            dispatchHandlers bot message
            return nStatus

  put nStatus
  case nStatus of
    Idle         -> listenerLoop (idleFor + oneSec)
    Disconnected -> return ()
    _            -> listenerLoop 0

  where
    dispatchHandlers bot@Bot { .. } message =
      forM_ (mapToList msgHandlers) $ \(_, msgHandler) -> fork $
        handle (\(e :: SomeException) -> debug $ "Exception! " ++ pack (show e)) $ do
          mCmd <- runMsgHandler msgHandler botConfig message
          case mCmd of
            Nothing  -> return ()
            Just cmd -> sendCommand bot cmd

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

connect :: BotConfig -> IO Bot
connect botConfig@BotConfig { .. } = do
  debug "Connecting ..."
  socket <- connectToWithRetry
  hSetBuffering socket LineBuffering
  msgHandlers <- loadMsgHandlers botConfig
  debug "Connected"
  return $ Bot botConfig socket msgHandlers
  where
    connectToWithRetry = connectTo (unpack server) (PortNumber (fromIntegral port))
                           `catch` (\(e :: SomeException) -> do
                                      debug ("Error while connecting: " ++ pack (show e) ++ ". Waiting.")
                                      threadDelay (5 * oneSec)
                                      connectToWithRetry)

disconnect :: Bot -> IO ()
disconnect bot@Bot { .. } = do
  debug "Disconnecting ..."
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
    run_ = bracket (connect botConfig) disconnect $ \bot ->
      go bot `catch` \(e :: SomeException) -> do
        debug $ "Exception! " ++ pack (show e)
        return Errored

    go bot = do
      sendCommand bot NickCmd
      sendCommand bot UserCmd
      runIRC bot Connected (listenerLoop 0)
