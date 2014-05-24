module Network.IRC.Protocol (MessageParser, msgFromLine, lineFromCommand) where

import ClassyPrelude
import Data.List ((!!))
import Data.Text (split, strip)

import Network.IRC.Types

type MessageParser = BotConfig -> UTCTime -> Text -> Message

msgFromLine :: MessageParser
msgFromLine (BotConfig { .. }) time line
  | "PING :" `isPrefixOf` line = Message time line $ PingMsg (drop 6 line)
  | otherwise = case command of
      "PONG"    -> Message time line $ PongMsg message
      "JOIN"    -> Message time line $ JoinMsg user
      "QUIT"    -> Message time line $ QuitMsg user quitMessage
      "PART"    -> Message time line $ PartMsg user message
      "KICK"    -> Message time line $ KickMsg user kicked kickReason
      "MODE"    -> if source == botNick
                     then Message time line $ ModeMsg Self target message []
                     else Message time line $ ModeMsg user target mode modeArgs
      "NICK"    -> Message time line $ NickMsg user (drop 1 target)
      "353"     -> Message time line $ NamesMsg namesNicks
      "433"     -> Message time line NickInUseMsg
      "PRIVMSG" | target /= channel -> Message time line $ PrivMsg user message
                | isActionMsg       -> Message time line $ ActionMsg user (initDef . drop 8 $ message)
                | otherwise         -> Message time line $ ChannelMsg user message
      _         -> Message time line $ OtherMsg source command target message
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

    nickPrefixes    = "~&@%+" :: String
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
