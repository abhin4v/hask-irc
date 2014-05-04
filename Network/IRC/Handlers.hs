{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Network.IRC.Handlers (handleMessage) where

import qualified Data.List as L

import Data.Text
import Prelude hiding ((++))

import Network.IRC.Handlers.SongSearch
import Network.IRC.Types

clean = toLower . strip
(++) = append

handleMessage :: HandlerName -> Handler
handleMessage "greeter"    = greeter
handleMessage "welcomer"   = welcomer
handleMessage "songsearch" = songSearch

greeter bot ChannelMsg { .. } = case L.find (== clean msg) greetings of
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
