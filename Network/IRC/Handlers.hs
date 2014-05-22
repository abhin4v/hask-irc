{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.IRC.Handlers (coreMsgHandlerNames, mkMsgHandler) where

import qualified Network.IRC.Handlers.MessageLogger as Logger
import qualified Network.IRC.Handlers.SongSearch    as SongSearch
import qualified Network.IRC.Handlers.Auth          as Auth

import ClassyPrelude
import Control.Concurrent.Lifted  (Chan)
import Control.Monad.Reader       (ask)
import Data.Convertible           (convert)
import Data.Text                  (strip)
import Data.Time                  (addUTCTime)

import Network.IRC.Types
import Network.IRC.Util

clean :: Text -> Text
clean = toLower . strip

coreMsgHandlerNames :: [Text]
coreMsgHandlerNames = ["pingpong", "messagelogger", "help"]

mkMsgHandler :: BotConfig -> Chan SomeEvent -> MsgHandlerName -> IO (Maybe MsgHandler)
mkMsgHandler _ _ "greeter"  = return . Just $ newMsgHandler { onMessage = greeter }
mkMsgHandler _ _ "welcomer" = return . Just $ newMsgHandler { onMessage = welcomer }
mkMsgHandler _ _ "pingpong" = do
  state <- getCurrentTime >>= newIORef
  return . Just $ newMsgHandler { onMessage = pingPong state }
mkMsgHandler _ _ "help"     =
    return . Just $ newMsgHandler { onMessage = help, onHelp = return $ singletonMap "!help" helpMsg}
  where
    helpMsg = "Get help. !help or !help <command>"

mkMsgHandler botConfig eventChan name =
  flip (`foldM` Nothing) [Logger.mkMsgHandler, SongSearch.mkMsgHandler, Auth.mkMsgHandler] $ \acc h ->
    case acc of
      Just _  -> return acc
      Nothing -> h botConfig eventChan name

pingPong :: MonadMsgHandler m => IORef UTCTime -> Message -> m (Maybe Command)
pingPong state PingMsg { .. } = do
  liftIO $ atomicWriteIORef state msgTime
  return . Just $ PongCmd msg
pingPong state PongMsg { .. } = do
  liftIO $ atomicWriteIORef state msgTime
  return Nothing
pingPong state IdleMsg { .. } | even (convert msgTime :: Int) = do
  BotConfig { .. } <- ask
  let limit = fromIntegral $ botTimeout `div` 2
  liftIO $ do
    lastComm <- readIORef state
    if addUTCTime limit lastComm < msgTime
      then return . Just . PingCmd . pack . formatTime defaultTimeLocale "%s" $ msgTime
      else return Nothing
pingPong _ _          = return Nothing

greeter ::  MonadMsgHandler m => Message -> m (Maybe Command)
greeter ChannelMsg { .. } = case find (== clean msg) greetings of
    Nothing       -> return Nothing
    Just greeting -> return . Just . ChannelMsgReply $ greeting ++ " " ++ userNick user
  where
    greetings = ["hi", "hello", "hey", "sup", "bye"
                , "good morning", "good evening", "good night"
                , "ohayo", "oyasumi"]
greeter _ = return Nothing

welcomer :: MonadMsgHandler m => Message -> m (Maybe Command)
welcomer JoinMsg { .. } = do
  BotConfig { .. } <- ask
  if userNick user /= botNick
    then return . Just . ChannelMsgReply $ "welcome back " ++ userNick user
    else return Nothing

welcomer _ = return Nothing

help :: MonadMsgHandler m => Message -> m (Maybe Command)
help ChannelMsg { .. }
  | "!help" == clean msg = do
      BotConfig { .. } <- ask
      let commands = concatMap mapKeys . mapValues $ msgHandlerInfo
      return . Just . ChannelMsgReply $ "I know these commands: " ++ unwords commands
  | "!help" `isPrefixOf` msg = do
      BotConfig { .. } <- ask
      let command = clean . unwords . drop 1 . words $ msg
      let mHelp   = find ((== command) . fst) . concatMap mapToList . mapValues $ msgHandlerInfo
      return . Just . ChannelMsgReply $ maybe ("No such command found: " ++ command) snd mHelp

help _ = return Nothing

