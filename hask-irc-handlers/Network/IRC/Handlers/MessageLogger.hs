{-# LANGUAGE FlexibleContexts #-}

module Network.IRC.Handlers.MessageLogger (messageLoggerMsgHandlerMaker) where

import qualified Data.Configurator       as CF
import qualified Data.Text.Format        as TF
import qualified Data.Text.Format.Params as TF

import ClassyPrelude hiding     ((</>), (<.>), FilePath, log)
import Data.Time                (diffDays)
import System.Directory         (createDirectoryIfMissing, getModificationTime, copyFile, removeFile)
import System.FilePath          (FilePath, (</>), (<.>))
import System.IO                (openFile, IOMode(..), hSetBuffering, BufferMode(..))

import Network.IRC
import Network.IRC.Util

type LoggerState = Maybe (Handle, Day)

messageLoggerMsgHandlerMaker :: MsgHandlerMaker
messageLoggerMsgHandlerMaker = MsgHandlerMaker "messagelogger" go
  where
    go botConfig _ = do
      state <- io $ newIORef Nothing
      initMessageLogger botConfig state
      return $ newMsgHandler { onMessage = flip messageLogger state
                             , onStop    = exitMessageLogger state }

getLogFilePath :: BotConfig -> IO FilePath
getLogFilePath BotConfig { .. } = do
  logFileDir <- CF.require config "messagelogger.logdir"
  createDirectoryIfMissing True logFileDir
  return $ logFileDir </> unpack (botChannel ++ "-" ++ nickToText botNick) <.> "log"

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
exitMessageLogger state = io $ readIORef state >>= flip whenJust (hClose . fst)

withLogFile :: MonadMsgHandler m => (Handle -> IO ()) -> IORef LoggerState -> m [Message]
withLogFile action state = do
  botConfig <- ask
  io $ do
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

  return []

messageLogger :: MonadMsgHandler m => Message -> IORef LoggerState -> m [Message]
messageLogger Message { .. }
  | Just (ChannelMsg user msg)         <- fromMessage message =
      log "<{}> {}" [nick user, msg]
  | Just (ActionMsg user msg)          <- fromMessage message =
      log "<{}> {} {}" [nick user, nick user, msg]
  | Just (KickMsg user kickedNick msg) <- fromMessage message =
      log "** {} KICKED {} :{}" [nick user, nickToText kickedNick, msg]
  | Just (JoinMsg user)                <- fromMessage message =
      log "** {} JOINED" [nick user]
  | Just (PartMsg user msg)            <- fromMessage message =
      log "** {} PARTED :{}" [nick user, msg]
  | Just (QuitMsg user msg)            <- fromMessage message =
      log "** {} QUIT :{}" [nick user, msg]
  | Just (NickMsg user newNick)        <- fromMessage message =
      log "** {} CHANGED NICK TO {}" [nick user, nickToText newNick]
  | Just (NamesMsg nicks)              <- fromMessage message =
      log "** USERS {}" [unwords . map nickToText $ nicks]
  | otherwise                                                 =
      const $ return []
  where
    nick = nickToText . userNick

    log format args = withLogFile $ \logFile ->
      TF.hprint logFile ("[{}] " ++ format ++ "\n") $ TF.buildParams (fmtTime msgTime : args)

    fmtTime = pack . formatTime defaultTimeLocale "%F %T"
