{-# LANGUAGE RecordWildCards, NoImplicitPrelude, OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}

module Network.IRC.Handlers.Core (getMsgHandler) where

import qualified Data.Configurator as C
import qualified Data.Text.Format as TF
import qualified Data.Text.Format.Params as TF

import ClassyPrelude hiding (try, (</>), (<.>), FilePath)
import Control.Monad.Reader
import Control.Monad.State
import Data.Dynamic
import Data.Time (diffDays)
import System.Directory
import System.FilePath
import System.IO (openFile, IOMode(..), hSetBuffering, BufferMode(..))

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

getLogFilePath :: BotConfig -> IO FilePath
getLogFilePath BotConfig { .. } = do
  logFileDir <- C.require config "messagelogger.logdir"
  createDirectoryIfMissing True logFileDir
  return $ logFileDir </> unpack botNick <.> "log"

openLogFile :: FilePath -> IO Handle
openLogFile logFilePath = do
  logFileHandle <- openFile logFilePath AppendMode
  hSetBuffering logFileHandle LineBuffering
  return logFileHandle

initMessageLogger :: MonadMsgHandler m => m ()
initMessageLogger = do
  botConfig <- ask
  (logFileHandle, curDay) <- liftIO $ do
    logFilePath   <- getLogFilePath botConfig
    logFileHandle <- openLogFile logFilePath
    time <- getModificationTime logFilePath
    return (logFileHandle, utctDay time)
  put $ toDyn (logFileHandle, curDay)

exitMessageLogger :: MonadMsgHandler m => m ()
exitMessageLogger = do
  mHandle <- map fromDynamic get
  case mHandle of
    Nothing                            -> return ()
    Just (logFileHandle, _ :: UTCTime) -> liftIO $ hClose logFileHandle

withLogFile :: MonadMsgHandler m => (Handle -> IO ()) -> m (Maybe Command)
withLogFile action = do
  botConfig <- ask

  (logFileHandle, prevDay) <- map (`fromDyn` error "No log file set") get

  (logFileHandle', curDay) <- liftIO $ do
    curDay <- map utctDay getCurrentTime
    let diff = diffDays curDay prevDay
    logFileHandle'' <- if diff >= 1
      then do
        hClose logFileHandle
        logFilePath <- getLogFilePath botConfig
        copyFile logFilePath (logFilePath <.> show prevDay)
        removeFile logFilePath
        openLogFile logFilePath
      else return logFileHandle

    action logFileHandle''
    return (logFileHandle'', curDay)

  put $ toDyn (logFileHandle', curDay)
  return Nothing

fmtTime :: UTCTime -> String
fmtTime = formatTime defaultTimeLocale "%F %T"

messageLogger :: MonadMsgHandler m => Message -> m (Maybe Command)
messageLogger ChannelMsg { .. } = withLogFile $ \logFile ->
  TF.hprint logFile "[{}] {}: {}\n" $ TF.buildParams (fmtTime msgTime, userNick user, msg)

messageLogger ActionMsg { .. } = withLogFile $ \logFile ->
  TF.hprint logFile "[{}] {}: {} {}\n" $
    TF.buildParams (fmtTime msgTime, userNick user, userNick user, msg)

messageLogger KickMsg { .. } = withLogFile $ \logFile ->
  TF.hprint logFile "[{}] ** {} KICKED {} :{}\n" $
    TF.buildParams (fmtTime msgTime, userNick user, kickedNick, msg)

messageLogger JoinMsg { .. } = withLogFile $ \logFile ->
  TF.hprint logFile "[{}] ** {} JOINED\n" $
    TF.buildParams (fmtTime msgTime, userNick user)

messageLogger PartMsg { .. } = withLogFile $ \logFile ->
  TF.hprint logFile "[{}] ** {} PARTED :{}\n" $
    TF.buildParams (fmtTime msgTime, userNick user, msg)

messageLogger QuitMsg { .. } = withLogFile $ \logFile ->
  TF.hprint logFile "[{}] ** {} QUIT :{}\n" $
    TF.buildParams (fmtTime msgTime, userNick user, msg)

messageLogger NickMsg { .. } = withLogFile $ \logFile ->
  TF.hprint logFile "[{}] ** {} CHANGED NICK TO {}\n" $
    TF.buildParams (fmtTime msgTime, userNick user, nick)

messageLogger _ = return Nothing
