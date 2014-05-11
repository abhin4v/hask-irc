{-# LANGUAGE RecordWildCards, OverloadedStrings, NoImplicitPrelude, FlexibleContexts #-}

module Network.IRC.Handlers (coreMsgHandlerNames, getMsgHandler) where

import qualified Network.IRC.Handlers.MessageLogger as L
import qualified Network.IRC.Handlers.SongSearch as SS

import ClassyPrelude
import Control.Monad.Reader
import Data.Text (strip)

import Network.IRC.Types

clean :: Text -> Text
clean = toLower . strip

coreMsgHandlerNames :: [Text]
coreMsgHandlerNames = ["pingpong", "messagelogger"]

getMsgHandler :: MsgHandlerName -> Maybe MsgHandler
getMsgHandler "greeter"  = Just $ newMsgHandler { msgHandlerRun = greeter }
getMsgHandler "welcomer" = Just $ newMsgHandler { msgHandlerRun = welcomer }
getMsgHandler "pingpong" = Just $ newMsgHandler { msgHandlerRun = pingPong }
getMsgHandler name       = listToMaybe $ mapMaybe (\f -> f name)
                           [L.getMsgHandler, SS.getMsgHandler]

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
