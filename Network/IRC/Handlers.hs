{-# LANGUAGE RecordWildCards, OverloadedStrings, NoImplicitPrelude, FlexibleContexts #-}

module Network.IRC.Handlers (coreMsgHandlerNames, getMsgHandler) where

import qualified Network.IRC.Handlers.Core as C
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
getMsgHandler name       = listToMaybe $ mapMaybe (\f -> f name)
                           [C.getMsgHandler, SS.getMsgHandler]


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
