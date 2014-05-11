{-# LANGUAGE RecordWildCards, NoImplicitPrelude, OverloadedStrings, FlexibleContexts #-}

module Network.IRC.Handlers.Core (getMsgHandler) where

import qualified Data.Configurator as C
import qualified Data.Text.Format as TF
import qualified Data.Text.Format.Params as TF

import ClassyPrelude hiding (try, (</>), (<.>))
import Control.Monad.Reader
import Control.Monad.State
import Data.Dynamic
import System.Directory
import System.FilePath
import System.IO

import Network.IRC.Types

getMsgHandler :: MsgHandlerName -> Maybe MsgHandler
getMsgHandler "pingpong"      = Just $ newMsgHandler { msgHandlerRun = pingPong }
getMsgHandler "messagelogger" = Just $ newMsgHandler { msgHandlerInit = initMessageLogger
                                                     , msgHandlerRun  = messageLogger
                                                     , msgHandlerExit = exitMessageLogger }
getMsgHandler _               = Nothing

pingPong :: MonadMsgHandler m => Message -> m (Maybe Command)
pingPong Ping { .. } = return . Just $ Pong msg
pingPong _           = return Nothing

initMessageLogger :: MonadMsgHandler m => m ()
initMessageLogger = do
  BotConfig { .. } <- ask
  logFileHandle <- liftIO $ do
    logFileDir <- C.require config "messagelogger.logdir"
    createDirectoryIfMissing True logFileDir
    let logFilePath = logFileDir </> unpack botNick <.> "log"
    logFileHandle <- openFile logFilePath AppendMode
    hSetBuffering logFileHandle LineBuffering
    return logFileHandle
  put $ toDyn logFileHandle

exitMessageLogger :: MonadMsgHandler m => m ()
exitMessageLogger = do
  mHandle <- map fromDynamic get
  case mHandle of
    Nothing -> return ()
    Just logFileHandle -> liftIO $ hClose logFileHandle

withLogFile :: MonadMsgHandler m => (Handle -> IO ()) -> m (Maybe Command)
withLogFile action = do
  logFileHandle <- map (`fromDyn` error "No log file set") get
  liftIO $ action logFileHandle
  return Nothing

fmtTime :: UTCTime -> String
fmtTime = formatTime defaultTimeLocale "%F %T"

messageLogger :: MonadMsgHandler m => Message -> m (Maybe Command)
messageLogger ChannelMsg { .. } = withLogFile $ \logFileHandle ->
  TF.hprint logFileHandle "[{}] {}: {}\n" $ TF.buildParams (fmtTime msgTime, userNick user, msg)

messageLogger KickMsg { .. } = withLogFile $ \logFileHandle ->
  TF.hprint logFileHandle "[{}] ** {} KICKED {} :{}\n" $
    TF.buildParams (fmtTime msgTime, userNick user, kickedNick, msg)

messageLogger JoinMsg { .. } = withLogFile $ \logFileHandle ->
  TF.hprint logFileHandle "[{}] ** {} JOINED\n" $
    TF.buildParams (fmtTime msgTime, userNick user)

messageLogger PartMsg { .. } = withLogFile $ \logFileHandle ->
  TF.hprint logFileHandle "[{}] ** {} PARTED :{}\n" $
    TF.buildParams (fmtTime msgTime, userNick user, msg)

messageLogger QuitMsg { .. } = withLogFile $ \logFileHandle ->
  TF.hprint logFileHandle "[{}] ** {} QUIT :{}\n" $
    TF.buildParams (fmtTime msgTime, userNick user, msg)

messageLogger NickMsg { .. } = withLogFile $ \logFileHandle ->
  TF.hprint logFileHandle "[{}] ** {} CHANGED NICK TO {}\n" $
    TF.buildParams (fmtTime msgTime, userNick user, nick)

messageLogger _ = return Nothing
