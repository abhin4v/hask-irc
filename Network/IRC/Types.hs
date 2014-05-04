module Network.IRC.Types where

import Control.Monad.Reader
import Data.Text (Text)
import System.IO
import System.Time

type Channel     = Text
type Nick        = Text
type HandlerName = Text
type Handler     = BotConfig -> Message -> IO (Maybe Command)

data User = Self | User { userNick :: Nick, userServer :: Text }
            deriving (Show, Eq)

data Message =
    ChannelMsg { time :: ClockTime, user :: User, msg :: Text }
  | PrivMsg    { time :: ClockTime, user :: User, msg :: Text }
  | Ping       { time :: ClockTime, msg :: Text }
  | JoinMsg    { time :: ClockTime, user :: User }
  | ModeMsg    { time :: ClockTime, user :: User, target :: Text
               , mode :: Text, modeArgs :: [Text] }
  | NickMsg    { time :: ClockTime, user :: User, nick :: Text }
  | QuitMsg    { time :: ClockTime, user :: User, msg :: Text }
  | PartMsg    { time :: ClockTime, user :: User, msg :: Text }
  | KickMsg    { time :: ClockTime, user :: User, msg :: Text }
  | OtherMsg   { time :: ClockTime, source :: Text, command :: Text
               , target :: Text, msg :: Text }
  deriving (Show, Eq)

data Command =
    Pong            { rmsg :: Text }
  | ChannelMsgReply { rmsg :: Text }
  | PrivMsgReply    { ruser :: User, rmsg :: Text }
  | NickCmd
  | UserCmd
  | JoinCmd
  deriving (Show, Eq)

data BotConfig = BotConfig { server :: String
                           , port :: Int
                           , channel :: Text
                           , botNick :: Text
                           , botTimeout :: Int
                           , handlers :: [HandlerName] }
                 deriving (Show, Eq)
data Bot = Bot { botConfig :: BotConfig, socket :: Handle } deriving (Show, Eq)

type IRC = ReaderT Bot IO
