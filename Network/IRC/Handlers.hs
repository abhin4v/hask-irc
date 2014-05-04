{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Network.IRC.Handlers (handleMessage) where

import qualified Data.List as L

import Data.Text
import Prelude hiding ((++))

import Network.IRC.Protocol
import Network.IRC.Types

(++) = append

handleMessage :: HandlerName -> Handler
handleMessage "greeter" = greeter
handleMessage "welcomer" = welcomer

greeter bot ChannelMsg { .. } = case L.find (== clean msg) greetings of
    Nothing       -> return Nothing
    Just greeting -> return . Just . ChannelMsgReply $ greeting ++ " " ++ userNick user
  where
    greetings = ["hi", "hello", "hey", "sup", "bye"
                , "good morning", "good evening", "good night"
                , "ohayo", "oyasumi"]

    clean = toLower . strip
greeter _ _ = return Nothing

welcomer bot@BotConfig { .. } JoinMsg { .. }
  | userNick user /= botNick = return . Just . ChannelMsgReply $ "welcome back " ++ userNick user
welcomer _ _ = return Nothing
