{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.IRC.Handlers.MessageLogger (mkMsgHandler) where

import qualified Data.Configurator as C
import qualified Data.Text.Format as TF
import qualified Data.Text.Format.Params as TF

import ClassyPrelude hiding      ((</>), (<.>), FilePath, log)
import Control.Concurrent.Lifted (Chan)
import Control.Exception.Lifted  (mask_)
import Control.Monad.Reader      (ask)
import Data.Time                 (diffDays)
import System.Directory          (createDirectoryIfMissing, getModificationTime, copyFile, removeFile)
import System.FilePath           (FilePath, (</>), (<.>))
import System.IO                 (openFile, IOMode(..), hSetBuffering, BufferMode(..))

import Network.IRC.Types

type LoggerState = Maybe (Handle, Day)

mkMsgHandler :: BotConfig -> Chan SomeEvent -> MsgHandlerName -> IO (Maybe MsgHandler)
mkMsgHandler botConfig _ "messagelogger" = do
  state <- liftIO $ newIORef Nothing
  initMessageLogger botConfig state
  return . Just $ newMsgHandler { onMessage = flip messageLogger state
                                , onStop    = exitMessageLogger state }
mkMsgHandler _ _ _                       = return Nothing

getLogFilePath :: BotConfig -> IO FilePath
getLogFilePath BotConfig { .. } = do
  logFileDir <- C.require config "messagelogger.logdir"
  createDirectoryIfMissing True logFileDir
  return $ logFileDir </> unpack (channel ++ "-" ++ botNick) <.> "log"

openLogFile :: FilePath -> IO Handle
openLogFile logFilePath = do
  logFileHandle <- openFile logFilePath AppendMode
  hSetBuffering logFileHandle LineBuffering
  return logFileHandle

initMessageLogger :: BotConfig -> IORef LoggerState -> IO ()
initMessageLogger botConfig state = do
  logFilePath   <- getLogFilePath botConfig
  logFileHandle <- openLogFile logFilePath
  time          <- getModificationTime logFilePath
  atomicWriteIORef state $ Just (logFileHandle, utctDay time)

exitMessageLogger :: MonadMsgHandler m => IORef LoggerState -> m ()
exitMessageLogger state = liftIO $ do
  mHandle <- readIORef state
  case mHandle of
    Nothing                 -> return ()
    Just (logFileHandle, _) -> hClose logFileHandle

withLogFile :: MonadMsgHandler m => (Handle -> IO ()) -> IORef LoggerState -> m (Maybe Command)
withLogFile action state = do
  botConfig <- ask
  liftIO $ do
    Just (logFileHandle, prevDay) <- readIORef state
    curDay                        <- map utctDay getCurrentTime
    let diff                      = diffDays curDay prevDay
    logFileHandle'                <- if diff >= 1
      then do
        hClose logFileHandle
        logFilePath <- getLogFilePath botConfig
        mask_ $ do
          copyFile logFilePath (logFilePath <.> show prevDay)
          removeFile logFilePath
        openLogFile logFilePath
      else return logFileHandle

    action logFileHandle'
    atomicWriteIORef state $ Just (logFileHandle', curDay)

  return Nothing

messageLogger :: MonadMsgHandler m => Message -> IORef LoggerState -> m (Maybe Command)
messageLogger message = case message of
  ChannelMsg { .. } -> log "<{}> {}"                  [userNick user, msg]
  ActionMsg { .. }  -> log "<{}> {} {}"               [userNick user, userNick user, msg]
  KickMsg { .. }    -> log "** {} KICKED {} :{}"      [userNick user, kickedNick, msg]
  JoinMsg { .. }    -> log "** {} JOINED"             [userNick user]
  PartMsg { .. }    -> log "** {} PARTED :{}"         [userNick user, msg]
  QuitMsg { .. }    -> log "** {} QUIT :{}"           [userNick user, msg]
  NickMsg { .. }    -> log "** {} CHANGED NICK TO {}" [userNick user, nick]
  NamesMsg { .. }   -> log "** USERS {}"              [unwords nicks]
  _                 -> const $ return Nothing
  where
    log format args = withLogFile $ \logFile ->
      TF.hprint logFile ("[{}] " ++ format ++ "\n") $ TF.buildParams (fmtTime (msgTime message) : args)

    fmtTime = pack . formatTime defaultTimeLocale "%F %T"

--messageLogger IdleMsg = const . liftIO $ do
--  now <- getCurrentTime
--  return . Just . MessageCmd $
