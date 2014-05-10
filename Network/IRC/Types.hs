{-# LANGUAGE RecordWildCards, RankNTypes, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE  NoImplicitPrelude, OverloadedStrings #-}

module Network.IRC.Types where

import ClassyPrelude
import Control.Monad.Reader
import Control.Monad.State
import Data.Configurator.Types

type Channel     = Text
type Nick        = Text
type HandlerName = Text

newtype Handler = Handler {
  runHandler :: forall m . (MonadIO m) => BotConfig -> Message -> m (Maybe Command)
}

data User = Self | User { userNick :: Nick, userServer :: Text }
            deriving (Show, Eq)

data Message =
    ChannelMsg { time :: UTCTime, user   :: User, msg     :: Text }
  | PrivMsg    { time :: UTCTime, user   :: User, msg     :: Text }
  | Ping       { time :: UTCTime, msg    :: Text }
  | JoinMsg    { time :: UTCTime, user   :: User }
  | ModeMsg    { time :: UTCTime, user   :: User, target  :: Text , mode   :: Text, modeArgs :: [Text] }
  | NickMsg    { time :: UTCTime, user   :: User, nick    :: Text }
  | QuitMsg    { time :: UTCTime, user   :: User, msg     :: Text }
  | PartMsg    { time :: UTCTime, user   :: User, msg     :: Text }
  | KickMsg    { time :: UTCTime, user   :: User, kicked  :: Text , msg    :: Text }
  | OtherMsg   { time :: UTCTime, source :: Text, command :: Text , target :: Text, msg      :: Text }
  deriving (Show, Eq)

data Command =
    Pong            { rmsg  :: Text }
  | ChannelMsgReply { rmsg  :: Text }
  | PrivMsgReply    { ruser :: User, rmsg :: Text }
  | NickCmd
  | UserCmd
  | JoinCmd
  deriving (Show, Eq)

data BotConfig = BotConfig { server     :: String
                           , port       :: Int
                           , channel    :: Text
                           , botNick    :: Text
                           , botTimeout :: Int
                           , handlers   :: [HandlerName]
                           , config     :: Config }

instance Show BotConfig where
  show BotConfig { .. } = "server = " ++ show server ++ "\n" ++
                          "port = " ++ show port ++ "\n" ++
                          "channel = " ++ show channel ++ "\n" ++
                          "nick = " ++ show botNick ++ "\n" ++
                          "timeout = " ++ show botTimeout ++ "\n" ++
                          "handlers = " ++ show handlers ++ "\n"

data Bot = Bot { botConfig :: BotConfig, socket :: Handle } deriving (Show)

data BotStatus = Connected | Disconnected | Joined | Kicked | Errored
                 deriving (Show, Eq)

newtype IRC a = IRC { _runIRC :: StateT BotStatus (ReaderT Bot IO) a }
                deriving (Functor, Monad, MonadIO, MonadReader Bot, MonadState BotStatus)

runIRC :: Bot -> BotStatus -> IRC a -> IO BotStatus
runIRC bot botStatus = flip runReaderT bot . flip execStateT botStatus . _runIRC
