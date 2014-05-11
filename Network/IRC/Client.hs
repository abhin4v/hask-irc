{-# LANGUAGE RecordWildCards, ScopedTypeVariables, NoImplicitPrelude, OverloadedStrings #-}

module Network.IRC.Client (run) where

import qualified Data.Text.Format as TF
import qualified Data.Text.Format.Params as TF

import ClassyPrelude
import Control.Concurrent
import Control.Monad.Reader hiding (forM_, foldM)
import Control.Monad.State hiding (forM_, foldM)
import Data.Maybe (fromJust)
import Network
import System.IO (hSetBuffering, BufferMode(..))
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
  TF.print "[{}} > {}\n" $ TF.buildParams (formatTime defaultTimeLocale "%F %T" time, line)

listen :: IRC ()
listen = do
  status <- get
  bot@Bot { .. } <- ask
  let nick  = botNick botConfig

  nStatus <- liftIO $ do
    when (status == Kicked) $
      threadDelay (5 * oneSec) >> sendCommand bot JoinCmd

    mLine <- map (map initEx) . timeout (oneSec * botTimeout botConfig) . hGetLine $ socket
    case mLine of
      Nothing -> return Disconnected
      Just line -> do
        now <- getCurrentTime
        debug $ "< " ++ line

        let message = msgFromLine botConfig now line
        nStatus <- case message of
          JoinMsg { .. } | userNick user == nick -> debug "Joined"          >> return Joined
          KickMsg { .. } | kickedNick == nick    -> debug "Kicked"          >> return Kicked
          ModeMsg { user = Self, .. }            -> sendCommand bot JoinCmd >> return status
          _                                      -> return status

        forM_ (msgHandlers botConfig) $ \msgHandlerName -> forkIO $ do
          let mMsgHandler = getMsgHandler msgHandlerName
          case mMsgHandler of
            Nothing         -> debug $ "No msg handler found with name: " ++ msgHandlerName
            Just msgHandler -> do
              let msgHandlerState = fromJust . lookup msgHandlerName $ msgHandlerStates
              mCmd <- runMsgHandler msgHandler botConfig msgHandlerState message
              case mCmd of
                Nothing  -> return ()
                Just cmd -> sendCommand bot cmd

        return nStatus

  put nStatus
  when (nStatus /= Disconnected) listen

connect :: BotConfig -> IO Bot
connect botConfig@BotConfig { .. } = do
  debug "Connecting ..."
  socket <- connectToWithRetry
  hSetBuffering socket LineBuffering
  msgHandlerStates <- loadMsgHandlers botConfig
  debug "Connected"
  return $ Bot botConfig socket msgHandlerStates
  where
    connectToWithRetry = connectTo (unpack server) (PortNumber (fromIntegral port))
                           `catch` (\(e :: SomeException) -> do
                                      debug ("Error while connecting: " ++ pack (show e) ++ ". Waiting.")
                                      threadDelay (5 * oneSec)
                                      connectToWithRetry)

loadMsgHandlers :: BotConfig -> IO MsgHandlerStates
loadMsgHandlers botConfig@BotConfig { .. } =
  flip (`foldM` mapFromList []) msgHandlers $ \hMap msgHandlerName -> do
    debug $ "Loading msg handler: " ++ msgHandlerName
    let mMsgHandler = getMsgHandler msgHandlerName
    case mMsgHandler of
      Nothing         -> debug ("No msg handler found with name: " ++ msgHandlerName) >> return hMap
      Just msgHandler -> do
        msgHandlerState <- initMsgHandler msgHandler botConfig
        return $ insertMap msgHandlerName msgHandlerState hMap

unloadMsgHandlers :: Bot -> IO ()
unloadMsgHandlers Bot { .. } =
  forM_ (mapToList msgHandlerStates) $ \(msgHandlerName, msgHandlerState) -> do
    debug $ "Unloading msg handler: " ++ msgHandlerName
    let mMsgHandler = getMsgHandler msgHandlerName
    case mMsgHandler of
      Nothing         -> debug ("No msg handler found with name: " ++ msgHandlerName)
      Just msgHandler -> exitMsgHandler msgHandler botConfig msgHandlerState


disconnect :: Bot -> IO ()
disconnect bot@Bot { .. } = do
  debug "Disconnecting ..."
  unloadMsgHandlers bot
  hClose socket
  debug "Disconnected"

addCoreMsgHandlers :: BotConfig -> BotConfig
addCoreMsgHandlers botConfig =
  botConfig { msgHandlers = hashNub $ msgHandlers botConfig ++ coreMsgHandlerNames }

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
      runIRC bot Connected listen
