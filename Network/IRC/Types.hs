module Network.IRC.Types where

import Control.Monad.Reader
import System.IO
import System.Time

type Channel = String
type Nick    = String

data User = Self | User { userNick :: Nick, userServer :: String }
            deriving (Show, Eq)

data Message =
    ChannelMsg { time :: ClockTime, user :: User, msg :: String }
  | PrivMsg    { time :: ClockTime, user :: User, msg :: String }
  | Ping       { time :: ClockTime, msg :: String }
  | JoinMsg    { time :: ClockTime, user :: User }
  | ModeMsg    { time :: ClockTime, user :: User, target :: String
               , mode :: String, modeArgs :: [String] }
  | NickMsg    { time :: ClockTime, user :: User, nick :: String }
  | QuitMsg    { time :: ClockTime, user :: User, msg :: String }
  | PartMsg    { time :: ClockTime, user :: User, msg :: String }
  | OtherMsg   { time :: ClockTime, source :: String, command :: String
               , target :: String, msg :: String }
  deriving (Show, Eq)

data Command =
    Pong            { rmsg :: String }
  | ChannelMsgReply { rmsg :: String }
  | PrivMsgReply    { ruser :: User, rmsg :: String }
  | NickCmd
  | UserCmd
  | JoinCmd
  deriving (Show, Eq)

data Bot = Bot { server :: String
               , port :: Int
               , channel :: String
               , botNick :: String
               , socket :: Handle }

type IRC = ReaderT Bot IO
