{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.IRC.Handlers.MessageLogger (mkMsgHandler) where

import qualified Data.Configurator as C
import qualified Data.Text.Format as TF
import qualified Data.Text.Format.Params as TF

import ClassyPrelude hiding (try, (</>), (<.>), FilePath)
import Control.Monad.Reader
import Data.Time (diffDays)
import System.Directory
import System.FilePath
import System.IO (openFile, IOMode(..), hSetBuffering, BufferMode(..))

import Network.IRC.Types

type LoggerState = Maybe (Handle, Day)

mkMsgHandler :: BotConfig -> MsgHandlerName -> IO (Maybe MsgHandler)
mkMsgHandler botConfig "messagelogger" = do
  state <- liftIO $ newIORef Nothing
  initMessageLogger botConfig state
  return . Just $ newMsgHandler { msgHandlerRun  = flip messageLogger state
                                , msgHandlerStop = exitMessageLogger state }
mkMsgHandler _  _            = return Nothing

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

initMessageLogger :: BotConfig -> IORef LoggerState -> IO ()
initMessageLogger botConfig state = do
  logFilePath   <- getLogFilePath botConfig
  logFileHandle <- openLogFile logFilePath
  time <- getModificationTime logFilePath
  atomicWriteIORef state $ Just (logFileHandle, utctDay time)

exitMessageLogger :: MonadMsgHandler m => IORef LoggerState -> m ()
exitMessageLogger state = liftIO $ do
  mHandle <- readIORef state
  case mHandle of
    Nothing                        -> return ()
    Just (logFileHandle, _ :: Day) -> hClose logFileHandle

withLogFile :: MonadMsgHandler m => (Handle -> IO ()) -> IORef LoggerState -> m (Maybe Command)
withLogFile action state = do
  botConfig <- ask

  liftIO $ do
    Just (logFileHandle, prevDay) <- readIORef state
    curDay <- map utctDay getCurrentTime
    let diff = diffDays curDay prevDay
    logFileHandle' <- if diff >= 1
      then do
        hClose logFileHandle
        logFilePath <- getLogFilePath botConfig
        copyFile logFilePath (logFilePath <.> show prevDay)
        removeFile logFilePath
        openLogFile logFilePath
      else return logFileHandle

    action logFileHandle'
    atomicWriteIORef state $ Just (logFileHandle', curDay)

  return Nothing

fmtTime :: UTCTime -> String
fmtTime = formatTime defaultTimeLocale "%F %T"

messageLogger :: MonadMsgHandler m => Message -> IORef LoggerState -> m (Maybe Command)
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

messageLogger _ = const $ return Nothing
