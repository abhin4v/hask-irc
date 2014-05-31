module Network.IRC.Protocol
  ( MessagePart (..)
  , parseLine
  , lineFromCommand) where

import ClassyPrelude
import Data.List ((!!))
import Data.Text (strip)

import Network.IRC.Types

data MessageParseType = Names
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
  fromMaybe (Nothing, msgParts) . flip (`foldl'` Nothing) parsers $ \parseResult parser ->
    case parseResult of
      Just _  -> parseResult
      Nothing -> case parser botConfig time line msgParts of
                   Reject                  -> Nothing
                   Partial msgParts'       -> Just (Nothing, msgParts')
                   Done message' msgParts' -> Just (Just message', msgParts')
  where
    parsers = [pingParser, namesParser, lineParser]

pingParser :: MessageParser
pingParser _ time line msgParts
  | "PING :" `isPrefixOf` line = Done (Message time line . PingMsg . drop 6 $ line) msgParts
  | otherwise                  = Reject

lineParser :: MessageParser
lineParser BotConfig { .. } time line msgParts = flip Done msgParts . Message time line $
  case command of
    "PONG"    -> PongMsg message
    "JOIN"    -> JoinMsg user
    "QUIT"    -> QuitMsg user quitMessage
    "PART"    -> PartMsg user message
    "KICK"    -> KickMsg user (Nick kicked) kickReason
    "MODE"    -> if Nick source == botNick
                   then ModeMsg Self target message []
                   else ModeMsg user target mode modeArgs
    "NICK"    -> NickMsg user $ Nick (drop 1 target)
    "433"     -> NickInUseMsg
    "PRIVMSG" | target /= channel -> PrivMsg user message
              | isActionMsg       -> ActionMsg user (initDef . drop 8 $ message)
              | otherwise         -> ChannelMsg user message
    _         -> OtherMsg source command target message
  where
    splits          = words line
    command         = splits !! 1
    source          = drop 1 $ splits !! 0
    target          = splits !! 2
    message         = strip . drop 1 . unwords . drop 3 $ splits
    quitMessage     = strip . drop 1 . unwords . drop 2 $ splits
    user            = uncurry User . (Nick *** drop 1) . break (== '!') $ source
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
    namesNicks line' =
      map (Nick . stripNickPrefix) . words . drop 1 . unwords . drop 5 . words $ line'

lineFromCommand :: BotConfig -> Command -> Maybe Text
lineFromCommand BotConfig { .. } command = case command of
  PongCmd { .. }                  -> Just $ "PONG :" ++ rmsg
  PingCmd { .. }                  -> Just $ "PING :" ++ rmsg
  NickCmd                         -> Just $ "NICK " ++ nickToText botNick
  UserCmd                         -> Just $ "USER " ++ nickToText botNick ++ " 0 * :" ++ nickToText botNick
  JoinCmd                         -> Just $ "JOIN " ++ channel
  QuitCmd                         -> Just "QUIT"
  ChannelMsgReply { .. }          -> Just $ "PRIVMSG " ++ channel ++ " :" ++ rmsg
  PrivMsgReply (User { .. }) rmsg -> Just $ "PRIVMSG " ++ nickToText userNick ++ " :" ++ rmsg
  NamesCmd                        -> Just $ "NAMES " ++ channel
  _                               -> Nothing
