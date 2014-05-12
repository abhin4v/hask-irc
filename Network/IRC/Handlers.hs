{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.IRC.Handlers (coreMsgHandlerNames, mkMsgHandler) where

import qualified Network.IRC.Handlers.MessageLogger as L
import qualified Network.IRC.Handlers.SongSearch as SS

import ClassyPrelude
import Control.Monad.Reader.Class
import Data.Text (strip)

import Network.IRC.Types

clean :: Text -> Text
clean = toLower . strip

coreMsgHandlerNames :: [Text]
coreMsgHandlerNames = ["pingpong", "messagelogger"]

mkMsgHandler :: BotConfig -> MsgHandlerName -> IO (Maybe MsgHandler)
mkMsgHandler _ "greeter"  = return . Just $ newMsgHandler { msgHandlerRun = greeter }
mkMsgHandler _ "welcomer" = return . Just $ newMsgHandler { msgHandlerRun = welcomer }
mkMsgHandler _ "pingpong" = return . Just $ newMsgHandler { msgHandlerRun = pingPong }
mkMsgHandler botConfig name       =
  flip (`foldM` Nothing) [L.mkMsgHandler, SS.mkMsgHandler] $ \acc h ->
    case acc of
      Just _  -> return acc
      Nothing -> h botConfig name

pingPong :: MonadMsgHandler m => Message -> m (Maybe Command)
pingPong Ping { .. } = return . Just $ Pong msg
pingPong _           = return Nothing

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
