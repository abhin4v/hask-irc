{-# LANGUAGE RecordWildCards, OverloadedStrings, NoImplicitPrelude #-}

module Network.IRC.Protocol (msgFromLine, lineFromCommand) where

import ClassyPrelude
import Data.List ((!!))
import Data.Text (split)

import Network.IRC.Types

msgFromLine :: BotConfig -> UTCTime -> Text -> Message
msgFromLine (BotConfig { .. }) time line
  | "PING :" `isPrefixOf` line = Ping time (drop 6 line) line
  | otherwise = case command of
      "JOIN"    -> JoinMsg time user line
      "QUIT"    -> QuitMsg time user quitMessage line
      "PART"    -> PartMsg time user message line
      "KICK"    -> KickMsg time user kicked kickReason line
      "MODE"    -> if source == botNick
        then ModeMsg time Self target message [] line
        else ModeMsg time user target mode modeArgs line
      "NICK"    -> NickMsg time user (drop 1 target) line
      "PRIVMSG" -> if target == channel
        then if "\x01" `isPrefixOf` message && "ACTION" `isPrefixOf` drop 1 message
          then ActionMsg time user (initDef . drop 8 $ message) line
          else ChannelMsg time user message line
        else PrivMsg time user message line
      _         -> OtherMsg time source command target message line
  where
    isSpc          = (== ' ')
    isNotSpc       = not . isSpc
    splits         = split isSpc line
    source         = drop 1 . takeWhile isNotSpc $ line
    target         = splits !! 2
    command        = splits !! 1
    message        = drop 1 . unwords . drop 3 $ splits
    quitMessage    = drop 1 . unwords . drop 2 $ splits
    user           = let u = split (== '!') source in User (u !! 0) (u !! 1)
    mode           = splits !! 3
    modeArgs       = drop 4 splits
    kicked         = splits !! 3
    kickReason     = drop 1 . unwords . drop 4 $ splits

lineFromCommand :: BotConfig -> Command -> Text
lineFromCommand (BotConfig { .. }) reply = case reply of
  Pong { .. }                     -> "PONG :" ++ rmsg
  NickCmd                         -> "NICK " ++ botNick
  UserCmd                         -> "USER " ++ botNick ++ " 0 * :" ++ botNick
  JoinCmd                         -> "JOIN " ++ channel
  ChannelMsgReply { .. }          -> "PRIVMSG " ++ channel ++ " :" ++ rmsg
  PrivMsgReply (User { .. }) rmsg -> "PRIVMSG " ++ botNick ++ " :" ++ rmsg
