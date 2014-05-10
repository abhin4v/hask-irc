{-# LANGUAGE RecordWildCards, OverloadedStrings, NoImplicitPrelude #-}

module Network.IRC.Handlers (getHandler) where

import ClassyPrelude
import Data.Text (strip)

import Network.IRC.Handlers.SongSearch
import Network.IRC.Types

clean :: Text -> Text
clean = toLower . strip

getHandler :: HandlerName -> Maybe Handler
getHandler "greeter"    = Just $ Handler greeter
getHandler "welcomer"   = Just $ Handler welcomer
getHandler "songsearch" = Just $ Handler songSearch
getHandler _            = Nothing

greeter :: Monad m => BotConfig -> Message -> m (Maybe Command)
greeter _ ChannelMsg { .. } = case find (== clean msg) greetings of
    Nothing       -> return Nothing
    Just greeting -> return . Just . ChannelMsgReply $ greeting ++ " " ++ userNick user
  where
    greetings = ["hi", "hello", "hey", "sup", "bye"
                , "good morning", "good evening", "good night"
                , "ohayo", "oyasumi"]
greeter _ _ = return Nothing

welcomer :: Monad m => BotConfig -> Message -> m (Maybe Command)
welcomer BotConfig { .. } JoinMsg { .. }
  | userNick user /= botNick = return . Just . ChannelMsgReply $ "welcome back " ++ userNick user
welcomer _ _ = return Nothing
