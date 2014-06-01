{-# LANGUAGE FlexibleContexts #-}

module Network.IRC.Handlers.MessageLogger (mkMsgHandler) where

import qualified Data.Configurator       as CF
import qualified Data.Text.Format        as TF
import qualified Data.Text.Format.Params as TF

import ClassyPrelude hiding     ((</>), (<.>), FilePath, log)
import Control.Exception.Lifted (mask_)
import Control.Monad.Reader     (ask)
import Data.Time                (diffDays)
import System.Directory         (createDirectoryIfMissing, getModificationTime, copyFile, removeFile)
import System.FilePath          (FilePath, (</>), (<.>))
import System.IO                (openFile, IOMode(..), hSetBuffering, BufferMode(..))

import Network.IRC.Types
import Network.IRC.Util

type LoggerState = Maybe (Handle, Day)

mkMsgHandler :: MsgHandlerMaker
mkMsgHandler botConfig _ "messagelogger" = do
  state <- io $ newIORef Nothing
  initMessageLogger botConfig state
  return . Just $ newMsgHandler { onMessage = flip messageLogger state
                                , onStop    = exitMessageLogger state }
mkMsgHandler _ _ _                       = return Nothing

getLogFilePath :: BotConfig -> IO FilePath
getLogFilePath BotConfig { .. } = do
  logFileDir <- CF.require config "messagelogger.logdir"
  createDirectoryIfMissing True logFileDir
  return $ logFileDir </> unpack (channel ++ "-" ++ nickToText botNick) <.> "log"

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

withLogFile :: MonadMsgHandler m => (Handle -> IO ()) -> IORef LoggerState -> m [Command]
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

messageLogger :: MonadMsgHandler m => Message -> IORef LoggerState -> m [Command]
messageLogger Message { .. } = case msgDetails of
  ChannelMsg { .. } -> log "<{}> {}"                  [nick user, msg]
  ActionMsg { .. }  -> log "<{}> {} {}"               [nick user, nick user, msg]
  KickMsg { .. }    -> log "** {} KICKED {} :{}"      [nick user, nickToText kickedNick, msg]
  JoinMsg { .. }    -> log "** {} JOINED"             [nick user]
  PartMsg { .. }    -> log "** {} PARTED :{}"         [nick user, msg]
  QuitMsg { .. }    -> log "** {} QUIT :{}"           [nick user, msg]
  NickMsg { .. }    -> log "** {} CHANGED NICK TO {}" [nick user, nickToText newNick]
  NamesMsg { .. }   -> log "** USERS {}"              [unwords . map nickToText $ nicks]
  _                 -> const $ return []
  where
    nick = nickToText . userNick

    log format args = withLogFile $ \logFile ->
      TF.hprint logFile ("[{}] " ++ format ++ "\n") $ TF.buildParams (fmtTime msgTime : args)

    fmtTime = pack . formatTime defaultTimeLocale "%F %T"
