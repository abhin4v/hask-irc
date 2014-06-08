module Network.IRC.Protocol
  ( MessagePart (..)
  , parseLine
  , formatCommand) where

import ClassyPrelude
import Data.Foldable (msum)
import Data.List     ((!!))
import Data.Text     (strip)

import Network.IRC.Types

parseLine :: BotConfig -> UTCTime -> Text -> [MessagePart] -> (Maybe FullMessage, [MessagePart])
parseLine botConfig@BotConfig { .. } time line msgParts =
  fromMaybe (Nothing, msgParts) . msum . flip map parsers $ \MessageParser { .. } -> let
    (parserMsgParts, otherParserMsgParts) = partition ((msgParserId ==) . msgPartParserId) msgParts
    in case msgParser botConfig time line parserMsgParts of
         Reject                  -> Nothing
         Partial msgParts'       -> Just (Nothing, msgParts' ++ otherParserMsgParts)
         Done message' msgParts' -> Just (Just message', msgParts' ++ otherParserMsgParts)
  where
    parsers = [pingParser, namesParser, lineParser] ++ msgParsers ++ [defaultParser]

pingParser :: MessageParser
pingParser = MessageParser "ping" go
  where
    go _ time line _
      | "PING :" `isPrefixOf` line = Done (FullMessage time line . toMessage . PingMsg . drop 6 $ line) []
      | otherwise                  = Reject

parseMsgLine :: Text -> ([Text], Text, Text, Text, Text)
parseMsgLine line = (splits, command, source, target, message)
  where
    splits      = words line
    command     = splits !! 1
    source      = drop 1 $ splits !! 0
    target      = splits !! 2
    message     = strip . drop 1 . unwords . drop 3 $ splits

lineParser :: MessageParser
lineParser = MessageParser "line" go
  where
    go BotConfig { .. } time line _ =
      case command of
        "PONG"    -> done $ toMessage $ PongMsg message
        "JOIN"    -> done $ toMessage $ JoinMsg user
        "QUIT"    -> done $ toMessage $ QuitMsg user quitMessage
        "PART"    -> done $ toMessage $ PartMsg user message
        "KICK"    -> done $ toMessage $ KickMsg user (Nick kicked) kickReason
        "MODE"    -> done $ toMessage $ if Nick source == botNick
                       then ModeMsg Self target message []
                       else ModeMsg user target mode modeArgs
        "NICK"    -> done $ toMessage $ NickMsg user $ Nick (drop 1 target)
        "433"     -> done $ toMessage NickInUseMsg
        "PRIVMSG" | target /= channel -> done $ toMessage $ PrivMsg user message
                  | isActionMsg       -> done $ toMessage $ ActionMsg user (initDef . drop 8 $ message)
                  | otherwise         -> done $ toMessage $ ChannelMsg user message
        _         -> Reject
      where
        done = flip Done [] . FullMessage time line

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
    go _ time line _ = flip Done [] . FullMessage time line $
      toMessage $ OtherMsg source command target message
      where
        (_, command, source, target, message) = parseMsgLine line

namesParser :: MessageParser
namesParser = MessageParser "names" go
  where
    go BotConfig { .. } time line msgParts = case command of
      "353" -> Partial $ MessagePart "names" target time line : msgParts
      "366" -> let
        (myMsgParts, otherMsgParts) = partition ((target ==) . msgPartTarget) msgParts
        (nicks, allLines) =  concat *** intercalate "\r\n" . (++ [line])
          $ unzip $ map (\MessagePart { .. } -> (namesNicks msgPartLine, msgPartLine)) myMsgParts
        in Done (FullMessage time allLines . toMessage $ NamesMsg nicks) otherMsgParts
      _     -> Reject
      where
        (_ : command : target : _) = words line
        stripNickPrefix  = pack . dropWhile (`elem` ("~&@%+" :: String)) . unpack
        namesNicks line' =
          map (Nick . stripNickPrefix) . words . drop 1 . unwords . drop 5 . words $ line'

formatCommand :: CommandFormatter
formatCommand botConfig@BotConfig { .. } command =
  msum . map (\formatter -> formatter botConfig command) $ defaultCommandFormatter : cmdFormatters

defaultCommandFormatter :: CommandFormatter
defaultCommandFormatter BotConfig { .. } command
  | Just (PongCmd msg)                    <- fromCommand command = Just $ "PONG :" ++ msg
  | Just (PingCmd msg)                    <- fromCommand command = Just $ "PING :" ++ msg
  | Just NickCmd                          <- fromCommand command = Just $ "NICK " ++ botNick'
  | Just UserCmd                          <- fromCommand command =
      Just $ "USER " ++ botNick' ++ " 0 * :" ++ botNick'
  | Just JoinCmd                          <- fromCommand command = Just $ "JOIN " ++ channel
  | Just QuitCmd                          <- fromCommand command = Just "QUIT"
  | Just (ChannelMsgReply msg)            <- fromCommand command =
      Just $ "PRIVMSG " ++ channel ++ " :" ++ msg
  | Just (PrivMsgReply (User { .. }) msg) <- fromCommand command =
      Just $ "PRIVMSG " ++ nickToText userNick ++ " :" ++ msg
  | Just NamesCmd                         <- fromCommand command = Just $ "NAMES " ++ channel
  | otherwise = Nothing
  where
    botNick' = nickToText botNick
