{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Network.IRC.Protocol (msgFromLine, lineFromCommand) where

import qualified Data.List as L

import Data.Text
import Prelude hiding (drop, unwords, takeWhile, (++))
import System.Time

import Network.IRC.Types

msgFromLine :: BotConfig -> ClockTime -> Text -> Message
msgFromLine (BotConfig { .. }) time line
  | "PING :" `isPrefixOf` line = Ping time . drop 6 $ line
  | otherwise = case command of
      "JOIN"    -> JoinMsg time user
      "QUIT"    -> QuitMsg time user message
      "PART"    -> PartMsg time user message
      "KICK"    -> KickMsg time user kicked kickReason
      "MODE"    -> if source == botNick
        then ModeMsg time Self target message []
        else ModeMsg time user target mode modeArgs
      "NICK"    -> NickMsg time user (drop 1 target)
      "PRIVMSG" -> if target == channel
        then ChannelMsg time user message
        else PrivMsg time user message
      _         -> OtherMsg time source command target message
  where
    isSpc      = (== ' ')
    isNotSpc   = not . isSpc
    splits     = split isSpc line
    source     = drop 1 . takeWhile isNotSpc $ line
    target     = splits !! 2
    command    = splits !! 1
    message    = drop 1 . unwords . L.drop 3 $ splits
    user       = let u = split (== '!') source in User (u !! 0) (u !! 1)
    mode       = splits !! 3
    modeArgs   = L.drop 4 splits
    kicked     = splits !! 3
    kickReason = drop 1 . unwords . L.drop 4 $ splits

lineFromCommand :: BotConfig -> Command -> Text
lineFromCommand (BotConfig { .. }) reply = case reply of
  Pong { .. }                     -> "PONG :" ++ rmsg
  NickCmd                         -> "NICK " ++ botNick
  UserCmd                         -> "USER " ++ botNick ++ " 0 * :" ++ botNick
  JoinCmd                         -> "JOIN " ++ channel
  ChannelMsgReply { .. }          -> "PRIVMSG " ++ channel ++ " :" ++ rmsg
  PrivMsgReply (User { .. }) rmsg -> "PRIVMSG " ++ botNick ++ " :" ++ rmsg
  where
    (++) = append
