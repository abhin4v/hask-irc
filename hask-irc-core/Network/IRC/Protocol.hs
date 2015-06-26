module Network.IRC.Protocol
  ( defaultParsers
  , defaultCommandFormatter
  ) where

import ClassyPrelude
import Data.Maybe (fromJust)
import Data.List  ((!!))
import Data.Text  (strip)

import Network.IRC.Types

pingParser :: MessageParser
pingParser = MessageParser "ping" go
  where
    go _ time line _
      | "PING :" `isPrefixOf` line =
          flip ParseDone [] . Message time line . toMessage . PingMsg . drop 6 $ line
      | otherwise                  = ParseReject

parseMsgLine :: Text -> ([Text], Text, Text, Text, Text)
parseMsgLine line = (splits, command, source, target, message)
  where
    splits  = words line
    command = splits !! 1
    source  = drop 1 $ splits !! 0
    target  = splits !! 2
    message = strip . drop 1 . unwords . drop 3 $ splits

lineParser :: MessageParser
lineParser = MessageParser "line" go
  where
    go BotConfig { .. } time line _
      | "PING :" `isPrefixOf` line = ParseReject
      | otherwise                  = case command of
          "PONG"    -> done $ toMessage $ PongMsg message
          "JOIN"    -> done $ toMessage $ JoinMsg user
          "QUIT"    -> done $ toMessage $ QuitMsg user quitMessage
          "PART"    -> done $ toMessage $ PartMsg user message
          "KICK"    -> done $ toMessage $ KickMsg user (Nick kicked) kickReason
          "MODE"    -> done $ toMessage $ if Nick target == botNick
                         then ModeMsg Self target message []
                         else ModeMsg user target mode modeArgs
          "NICK"    -> done $ toMessage $ NickMsg user $ Nick (drop 1 target)
          "433"     -> done $ toMessage NickInUseMsg
          "PRIVMSG"
            | target /= botChannel -> done $ toMessage $ PrivMsg user message
            | isActionMsg          -> done $ toMessage $ ActionMsg user (initDef . drop 8 $ message)
            | otherwise            -> done $ toMessage $ ChannelMsg user message
          _         -> ParseReject
      where
        done = flip ParseDone [] . Message time line

        (splits, command, source, target, message) = parseMsgLine line
        quitMessage = strip . drop 1 . unwords . drop 2 $ splits
        user        = uncurry User . (Nick *** drop 1) . break (== '!') $ source
        mode        = splits !! 3
        modeArgs    = drop 4 splits
        kicked      = splits !! 3
        kickReason  = drop 1 . unwords . drop 4 $ splits
        isActionMsg = "\SOH" `isPrefixOf` message && "ACTION" `isPrefixOf` drop 1 message

defaultParser :: MessageParser
defaultParser = MessageParser "default" go
  where
    go _ time line _
      | "PING :" `isPrefixOf` line = ParseReject
      | otherwise                  =
          flip ParseDone [] . Message time line . toMessage . OtherMsg source command target $ message
      where
        (_, command, source, target, message) = parseMsgLine line

namesParser :: MessageParser
namesParser = MessageParser "names" go
  where
    go BotConfig { .. } time line msgParts
      | "PING :" `isPrefixOf` line = ParseReject
      | otherwise                  = case command of
          "353" -> ParsePartial $ MessagePart target time line : msgParts
          "366" -> let
            (myMsgParts, otherMsgParts) = partition ((target ==) . msgPartTarget) msgParts
            (nicks, allLines) = concat *** intercalate "\r\n" . (++ [line])
              $ unzip $ map (\MessagePart { .. } -> (namesNicks msgPartLine, msgPartLine)) myMsgParts
            in ParseDone (Message time allLines . toMessage $ NamesMsg nicks) otherMsgParts
          _     -> ParseReject
      where
        (_, command, _ , target, _) = parseMsgLine line
        stripNickPrefix  = pack . dropWhile (`elem` ("~&@%+" :: String)) . unpack
        namesNicks line' =
          map (Nick . stripNickPrefix) . words . drop 1 . unwords . drop 5 . words $ line'

whoisParser :: MessageParser
whoisParser = MessageParser "whois" go
  where
    go BotConfig { .. } time line msgParts
      | "PING :" `isPrefixOf` line = ParseReject
      | command `elem` ["401", "311", "319", "312", "317"] =
          ParsePartial $ MessagePart target time line : msgParts
      | command == "318" = let
          (myMsgParts, otherMsgParts) = partition ((target ==) . msgPartTarget) msgParts
          allLines = intercalate "\r\n" . reverse . (line :) . map msgPartLine $ myMsgParts
          in ParseDone (Message time allLines . toMessage $ parse myMsgParts) otherMsgParts
      | otherwise = ParseReject
      where
        (_, command, _, target, _) = parseMsgLine line

    parse :: [MessagePart] -> WhoisReplyMsg
    parse myMsgParts =
      let partMap = asMap $ flip (`foldl'` mempty) myMsgParts $ \m MessagePart { .. } ->
                              insertMap (words msgPartLine !! 1) msgPartLine m
      in case lookup "401" partMap of
           Just line -> WhoisNoSuchNickMsg . Nick $ words line !! 3
           Nothing   -> let
               splits311   = words . fromJust . lookup "311" $ partMap
               nick        = Nick (splits311 !! 3)
               user        = splits311 !! 4
               host        = splits311 !! 5
               realName    = drop 1 $ splits311 !! 7
               channels    = mconcat
                            . maybeToList
                            . map (words . drop 1 . unwords . drop 4 . words)
                            . lookup "319"
                            $ partMap
               splits312   = words . fromJust . lookup "312" $ partMap
               server      = splits312 !! 4
               serverInfo  = drop 1 . unwords . drop 5 $ splits312
             in WhoisNickInfoMsg nick user host realName channels server serverInfo

defaultParsers :: [MessageParser]
defaultParsers = [pingParser, namesParser, whoisParser, lineParser, defaultParser]

defaultCommandFormatter :: CommandFormatter
defaultCommandFormatter BotConfig { .. } Message { .. }
  | Just (PongCmd msg)   <- fromMessage message = Just $ "PONG :" ++ msg
  | Just (PingCmd msg)   <- fromMessage message = Just $ "PING :" ++ msg
  | Just NickCmd         <- fromMessage message = Just $ "NICK " ++ botNick'
  | Just UserCmd         <- fromMessage message = Just $ "USER " ++ botNick' ++ " 0 * :" ++ botNick'
  | Just JoinCmd         <- fromMessage message = Just $ "JOIN " ++ botChannel
  | Just QuitCmd         <- fromMessage message = Just "QUIT"
  | Just NamesCmd        <- fromMessage message = Just $ "NAMES " ++ botChannel
  | Just (WhoisCmd nick) <- fromMessage message = Just $ "WHOIS " ++ nick
  | Just (ChannelMsgReply msg) <- fromMessage message            =
      Just $ "PRIVMSG " ++ botChannel ++ " :" ++ msg
  | Just (PrivMsgReply (User { .. }) msg) <- fromMessage message =
      Just $ "PRIVMSG " ++ nickToText userNick ++ " :" ++ msg
  | otherwise                                                    = Nothing
  where
    botNick' = nickToText botNick
