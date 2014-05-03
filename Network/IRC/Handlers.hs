{-# LANGUAGE RecordWildCards #-}

module Network.IRC.Handlers(handleMessage) where

import Data.List

import Network.IRC.Protocol
import Network.IRC.Types

handleMessage :: String -> Handler
handleMessage "greeter" = greeter
handleMessage "welcomer" = welcomer

greeter bot ChannelMsg { .. } = case find (`isPrefixOf` msg) greetings of
    Nothing       -> return Nothing
    Just greeting -> return . Just . ChannelMsgReply $ greeting ++ " " ++ userNick user
  where
    greetings = ["hi", "hello", "hey", "sup", "bye"
                , "good morning", "good evening", "good night"
                , "ohayo", "oyasumi"]
greeter _ _ = return Nothing

welcomer bot@BotConfig { .. } JoinMsg { .. }
  | userNick user /= botNick = return . Just . ChannelMsgReply $ "welcome back " ++ userNick user
welcomer _ _ = return Nothing
