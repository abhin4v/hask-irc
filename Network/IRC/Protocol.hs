{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.IRC.Protocol (msgFromLine, lineFromCommand) where

import ClassyPrelude
import Data.List ((!!))
import Data.Text (split, strip)

import Network.IRC.Types

msgFromLine :: BotConfig -> UTCTime -> Text -> Message
msgFromLine (BotConfig { .. }) time line
  | "PING :" `isPrefixOf` line = PingMsg time (drop 6 line) line
  | otherwise = case command of
      "PONG"    -> PongMsg time message line
      "JOIN"    -> JoinMsg time user line
      "QUIT"    -> QuitMsg time user quitMessage line
      "PART"    -> PartMsg time user message line
      "KICK"    -> KickMsg time user kicked kickReason line
      "MODE"    -> if source == botNick
                     then ModeMsg time Self target message [] line
                     else ModeMsg time user target mode modeArgs line
      "NICK"    -> NickMsg time user (drop 1 target) line
      "353"     -> NamesMsg time namesNicks
      "433"     -> NickInUseMsg time line
      "PRIVMSG" | target /= channel -> PrivMsg time user message line
                | isActionMsg       -> ActionMsg time user (initDef . drop 8 $ message) line
                | otherwise         -> ChannelMsg time user message line
      _         -> OtherMsg time source command target message line
  where
    isSpc           = (== ' ')
    isNotSpc        = not . isSpc
    splits          = split isSpc line
    source          = drop 1 . takeWhile isNotSpc $ line
    target          = splits !! 2
    command         = splits !! 1
    message         = strip . drop 1 . unwords . drop 3 $ splits
    quitMessage     = strip . drop 1 . unwords . drop 2 $ splits
    user            = uncurry User . break (== '!') $ source
    mode            = splits !! 3
    modeArgs        = drop 4 splits
    kicked          = splits !! 3
    kickReason      = drop 1 . unwords . drop 4 $ splits

    nickPrefixes :: String
    nickPrefixes    = "~&@%+"
    namesNicks      = map stripNickPrefix . words . drop 1 . unwords . drop 5 $ splits
    stripNickPrefix = pack . dropWhile (`elem` nickPrefixes) . unpack

    isActionMsg     = "\SOH" `isPrefixOf` message && "ACTION" `isPrefixOf` drop 1 message

lineFromCommand :: BotConfig -> Command -> Maybe Text
lineFromCommand BotConfig { .. } command = case command of
  PongCmd { .. }                  -> Just $ "PONG :" ++ rmsg
  PingCmd { .. }                  -> Just $ "PING :" ++ rmsg
  NickCmd                         -> Just $ "NICK " ++ botNick
  UserCmd                         -> Just $ "USER " ++ botNick ++ " 0 * :" ++ botNick
  JoinCmd                         -> Just $ "JOIN " ++ channel
  QuitCmd                         -> Just "QUIT"
  ChannelMsgReply { .. }          -> Just $ "PRIVMSG " ++ channel ++ " :" ++ rmsg
  PrivMsgReply (User { .. }) rmsg -> Just $ "PRIVMSG " ++ userNick ++ " :" ++ rmsg
  NamesCmd                        -> Just $ "NAMES " ++ channel
  _                               -> Nothing
