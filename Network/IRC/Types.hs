{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Network.IRC.Types
  ( Nick
  , MsgHandlerName
  , User (..)
  , Message (..)
  , MessageDetails (..)
  , Command (..)
  , Event (..)
  , SomeEvent
  , QuitEvent(..)
  , EventResponse (..)
  , BotConfig (..)
  , BotStatus (..)
  , Bot (..)
  , IRC
  , runIRC
  , MsgHandler (..)
  , MonadMsgHandler
  , newMsgHandler
  , handleMessage
  , handleEvent
  , stopMsgHandler
  , getHelp )
where

import ClassyPrelude
import Control.Monad.Reader    (ReaderT, MonadReader, runReaderT)
import Control.Monad.State     (StateT, MonadState, execStateT)
import Data.Configurator.Types (Config)
import Data.Typeable           (cast)

import Network.IRC.Util

-- IRC related

type Nick           = Text
type MsgHandlerName = Text

data User = Self | User { userNick :: !Nick, userServer :: !Text }
            deriving (Show, Eq)

data Message = Message { msgTime :: !UTCTime, msgLine :: !Text, msgDetails :: MessageDetails}
               deriving (Show, Eq)

data MessageDetails =
    IdleMsg
  | NickInUseMsg
  | PingMsg      { msg       :: !Text }
  | PongMsg      { msg       :: !Text }
  | NamesMsg     { nicks     :: ![Nick] }
  | ChannelMsg   { user      :: !User, msg        :: !Text }
  | PrivMsg      { user      :: !User, msg        :: !Text }
  | ActionMsg    { user      :: !User, msg        :: !Text }
  | JoinMsg      { user      :: !User }
  | QuitMsg      { user      :: !User, msg        :: !Text }
  | PartMsg      { user      :: !User, msg        :: !Text }
  | NickMsg      { user      :: !User, newNick    :: !Nick }
  | KickMsg      { user      :: !User, kickedNick :: !Nick, msg       :: !Text }
  | ModeMsg      { user      :: !User, msgTarget  :: !Text, mode      :: !Text , modeArgs :: ![Text] }
  | OtherMsg     { msgSource :: !Text, msgCommand :: !Text, msgTarget :: !Text , msg      :: !Text }
  deriving (Show, Eq)

data Command =
    PingCmd         { rmsg  :: !Text }
  | PongCmd         { rmsg  :: !Text }
  | ChannelMsgReply { rmsg  :: !Text }
  | PrivMsgReply    { ruser :: !User, rmsg :: !Text }
  | NickCmd
  | UserCmd
  | JoinCmd
  | QuitCmd
  | NamesCmd
  deriving (Show, Eq)

-- Internal events

class (Typeable e, Show e) => Event e where
  toEvent :: e -> IO SomeEvent
  toEvent e = SomeEvent <$> pure e <*> getCurrentTime

  fromEvent :: SomeEvent -> Maybe (e, UTCTime)
  fromEvent (SomeEvent e time) = do
    ev <- cast e
    return (ev, time)

data SomeEvent = forall e. Event e => SomeEvent e UTCTime deriving (Typeable)
instance Show SomeEvent where
  show (SomeEvent e time) = formatTime defaultTimeLocale "[%F %T] " time ++ show e

data QuitEvent = QuitEvent deriving (Show, Typeable)
instance Event QuitEvent

data EventResponse =  RespNothing
                    | RespEvent SomeEvent
                    | RespMessage Message
                    | RespCommand Command
                    deriving (Show)

-- Bot

data BotConfig = BotConfig { server         :: !Text
                           , port           :: !Int
                           , channel        :: !Text
                           , botNick        :: !Text
                           , botTimeout     :: !Int
                           , msgHandlerInfo :: !(Map MsgHandlerName (Map Text Text))
                           , config         :: !Config }

instance Show BotConfig where
  show BotConfig { .. } = "server = "   ++ show server     ++ "\n" ++
                          "port = "     ++ show port       ++ "\n" ++
                          "channel = "  ++ show channel    ++ "\n" ++
                          "nick = "     ++ show botNick    ++ "\n" ++
                          "timeout = "  ++ show botTimeout ++ "\n" ++
                          "handlers = " ++ show (mapKeys msgHandlerInfo)

data Bot = Bot { botConfig   :: !BotConfig
               , socket      :: !Handle
               , msgHandlers :: !(Map MsgHandlerName MsgHandler) }

data BotStatus = Connected
               | Disconnected
               | Joined
               | Kicked
               | Errored
               | Idle
               | Interrupted
               | NickNotAvailable
               deriving (Show, Eq)

newtype IRC a = IRC { _runIRC :: StateT BotStatus (ReaderT Bot IO) a }
                deriving ( Functor
                         , Applicative
                         , Monad
                         , MonadIO
                         , MonadReader Bot
                         , MonadState BotStatus )

runIRC :: Bot -> BotStatus -> IRC a -> IO BotStatus
runIRC bot botStatus = flip runReaderT bot . flip execStateT botStatus . _runIRC

-- Message handlers

newtype MsgHandlerT a = MsgHandlerT { _runMsgHandler :: ReaderT BotConfig IO a }
                        deriving ( Functor
                                 , Applicative
                                 , Monad
                                 , MonadIO
                                 , MonadReader BotConfig )

class (MonadIO m, Applicative m, MonadReader BotConfig m) => MonadMsgHandler m where
  msgHandler :: MsgHandlerT a -> m a

instance MonadMsgHandler MsgHandlerT where
  msgHandler = id

handleMessage :: MsgHandler -> BotConfig -> Message -> IO (Maybe Command)
handleMessage MsgHandler { .. } botConfig =
  flip runReaderT botConfig . _runMsgHandler . onMessage

stopMsgHandler :: MsgHandler -> BotConfig -> IO ()
stopMsgHandler MsgHandler { .. } botConfig =
  flip runReaderT botConfig . _runMsgHandler $ onStop

handleEvent :: MsgHandler -> BotConfig -> SomeEvent -> IO EventResponse
handleEvent MsgHandler { .. } botConfig =
  flip runReaderT botConfig . _runMsgHandler . onEvent

getHelp :: MsgHandler -> BotConfig -> IO (Map Text Text)
getHelp MsgHandler { .. } botConfig =
  flip runReaderT botConfig . _runMsgHandler $ onHelp

data MsgHandler = MsgHandler {
  onMessage :: !(forall m . MonadMsgHandler m => Message -> m (Maybe Command)),
  onStop    :: !(forall m . MonadMsgHandler m => m ()),
  onEvent   :: !(forall m . MonadMsgHandler m => SomeEvent -> m EventResponse),
  onHelp    :: !(forall m . MonadMsgHandler m => m (Map Text Text))
}

newMsgHandler :: MsgHandler
newMsgHandler = MsgHandler {
  onMessage = const $ return Nothing,
  onStop    = return (),
  onEvent   = const $ return RespNothing,
  onHelp    = return mempty
}
