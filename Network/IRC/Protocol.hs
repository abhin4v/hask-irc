{-# LANGUAGE RecordWildCards, OverloadedStrings, NoImplicitPrelude #-}

module Network.IRC.Protocol (msgFromLine, lineFromCommand) where

import qualified Data.List as L
import qualified Data.Text as T

import BasicPrelude
import System.Time

import Network.IRC.Types

msgFromLine :: BotConfig -> ClockTime -> Text -> Message
msgFromLine (BotConfig { .. }) time line
  | "PING :" `T.isPrefixOf` line = Ping time . T.drop 6 $ line
  | otherwise = case command of
      "JOIN"    -> JoinMsg time user
      "QUIT"    -> QuitMsg time user message
      "PART"    -> PartMsg time user message
      "KICK"    -> KickMsg time user kicked kickReason
      "MODE"    -> if source == botNick
        then ModeMsg time Self target message []
        else ModeMsg time user target mode modeArgs
      "NICK"    -> NickMsg time user (T.drop 1 target)
      "PRIVMSG" -> if target == channel
        then ChannelMsg time user message
        else PrivMsg time user message
      _         -> OtherMsg time source command target message
  where
    isSpc      = (== ' ')
    isNotSpc   = not . isSpc
    splits     = T.split isSpc line
    source     = T.drop 1 . T.takeWhile isNotSpc $ line
    target     = splits !! 2
    command    = splits !! 1
    message    = T.drop 1 . unwords . L.drop 3 $ splits
    user       = let u = T.split (== '!') source in User (u !! 0) (u !! 1)
    mode       = splits !! 3
    modeArgs   = L.drop 4 splits
    kicked     = splits !! 3
    kickReason = T.drop 1 . unwords . L.drop 4 $ splits

lineFromCommand :: BotConfig -> Command -> Text
lineFromCommand (BotConfig { .. }) reply = case reply of
  Pong { .. }                     -> "PONG :" ++ rmsg
  NickCmd                         -> "NICK " ++ botNick
  UserCmd                         -> "USER " ++ botNick ++ " 0 * :" ++ botNick
  JoinCmd                         -> "JOIN " ++ channel
  ChannelMsgReply { .. }          -> "PRIVMSG " ++ channel ++ " :" ++ rmsg
  PrivMsgReply (User { .. }) rmsg -> "PRIVMSG " ++ botNick ++ " :" ++ rmsg

