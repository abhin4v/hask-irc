module Network.IRC.Protocol
  ( MessagePart (..)
  , parseLine
  , lineFromCommand) where

import ClassyPrelude
import Data.List ((!!))
import Data.Text (strip)

import Network.IRC.Types

data MessageParseType =   Names
                        | Whois
                        deriving (Show, Eq)

data MessagePart = MessagePart { msgParserType :: MessageParseType
                               , msgPartTarget :: Text
                               , msgPartTime   :: UTCTime
                               , msgPartLine   :: Text }
                   deriving (Show, Eq)

data MessageParseResult =  Done Message [MessagePart]
                         | Partial [MessagePart]
                         | Reject
                         deriving (Show, Eq)

type MessageParser = BotConfig -> UTCTime -> Text -> [MessagePart] -> MessageParseResult

parseLine :: BotConfig -> UTCTime -> Text -> [MessagePart] -> (Maybe Message, [MessagePart])
parseLine botConfig time line msgParts =
  case lineParser botConfig time line msgParts of
    Done message@(Message { msgDetails = OtherMsg { .. }, .. }) _ ->
      fromMaybe (Just message, msgParts) . flip (`foldl'` Nothing) parsers $ \parseResult parser ->
        case parseResult of
          Just _  -> parseResult
          Nothing -> case parser botConfig time line msgParts of
                       Reject                  -> Nothing
                       Partial msgParts'       -> Just (Nothing, msgParts')
                       Done message' msgParts' -> Just (Just message', msgParts')
    Done message _ -> (Just message, msgParts)
    _              -> error "This should never happen"
  where
    parsers = [namesParser]

lineParser :: MessageParser
lineParser BotConfig { .. } time line msgParts
  | "PING :" `isPrefixOf` line = flip Done msgParts $ Message time line $ PingMsg (drop 6 line)
  | otherwise                  = flip Done msgParts $ case command of
      "PONG"    -> Message time line $ PongMsg message
      "JOIN"    -> Message time line $ JoinMsg user
      "QUIT"    -> Message time line $ QuitMsg user quitMessage
      "PART"    -> Message time line $ PartMsg user message
      "KICK"    -> Message time line $ KickMsg user kicked kickReason
      "MODE"    -> if source == botNick
                     then Message time line $ ModeMsg Self target message []
                     else Message time line $ ModeMsg user target mode modeArgs
      "NICK"    -> Message time line $ NickMsg user (drop 1 target)
      "433"     -> Message time line NickInUseMsg
      "PRIVMSG" | target /= channel -> Message time line $ PrivMsg user message
                | isActionMsg       -> Message time line $ ActionMsg user (initDef . drop 8 $ message)
                | otherwise         -> Message time line $ ChannelMsg user message
      _         -> Message time line $ OtherMsg source command target message
  where
    splits          = words line
    command         = splits !! 1
    source          = drop 1 $ splits !! 0
    target          = splits !! 2
    message         = strip . drop 1 . unwords . drop 3 $ splits
    quitMessage     = strip . drop 1 . unwords . drop 2 $ splits
    user            = uncurry User . second (drop 1) . break (== '!') $ source
    mode            = splits !! 3
    modeArgs        = drop 4 splits
    kicked          = splits !! 3
    kickReason      = drop 1 . unwords . drop 4 $ splits
    isActionMsg     = "\SOH" `isPrefixOf` message && "ACTION" `isPrefixOf` drop 1 message

partitionMsgParts :: MessageParseType -> Text -> [MessagePart] -> ([MessagePart], [MessagePart])
partitionMsgParts parserType target =
  partition (\MessagePart { .. } -> msgParserType == parserType && msgPartTarget == target)

namesParser :: MessageParser
namesParser BotConfig { .. } time line msgParts = case command of
  "353" -> Partial $ MessagePart Names target time line : msgParts
  "366" -> let
    (myMsgParts, otherMsgParts) = partitionMsgParts Names target msgParts
    (nicks, allLines) =  concat *** intercalate "\r\n" . (++ [line])
      $ unzip $ map (\MessagePart { .. } -> (namesNicks msgPartLine, msgPartLine)) myMsgParts
    in Done (Message time allLines $ NamesMsg nicks) otherMsgParts
  _     -> Reject
  where
    (_ : command : target : _) = words line
    stripNickPrefix  = pack . dropWhile (`elem` ("~&@%+" :: String)) . unpack
    namesNicks line' = map stripNickPrefix . words . drop 1 . unwords . drop 5 . words $ line'


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
