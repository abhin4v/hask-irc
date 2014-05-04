{-# LANGUAGE RecordWildCards #-}

module Network.IRC.Protocol where

import Data.List
import Data.List.Split
import System.Time

import Network.IRC.Types

msgFromLine :: BotConfig -> ClockTime -> String -> Message
msgFromLine (BotConfig { .. }) time line
  | "PING :" `isPrefixOf` line = Ping time . drop 6 $ line
  | otherwise = case command of
      "JOIN"    -> JoinMsg time user
      "QUIT"    -> QuitMsg time user message
      "PART"    -> PartMsg time user message
      "KICK"    -> KickMsg time user kickReason
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
    splits     = splitWhen isSpc line
    source     = drop 1 . takeWhile isNotSpc $ line
    target     = splits !! 2
    command    = splits !! 1
    message    = drop 1 . unwords . drop 3 $ splits
    user       = let u = splitWhen (== '!') source in User (u !! 0) (u !! 1)
    mode       = splits !! 3
    modeArgs   = drop 4 splits
    kickReason = drop 1 . unwords . drop 4 $ splits

lineFromCommand :: BotConfig -> Command -> String
lineFromCommand (BotConfig { .. }) reply = case reply of
  Pong { .. }                     -> "PONG :" ++ rmsg
  NickCmd                         -> "NICK " ++ botNick
  UserCmd                         -> "USER " ++ botNick ++ " 0 * :" ++ botNick
  JoinCmd                         -> "JOIN " ++ channel
  ChannelMsgReply { .. }          -> "PRIVMSG " ++ channel ++ " :" ++ rmsg
  PrivMsgReply (User { .. }) rmsg -> "PRIVMSG " ++ botNick ++ " :" ++ rmsg
