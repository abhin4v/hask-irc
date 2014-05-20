{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Network.IRC.Types
  ( Nick
  , MsgHandlerName
  , User (..)
  , Message (..)
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
  , stopMsgHandler)
where

import ClassyPrelude
import Control.Monad.Reader
import Control.Monad.State
import Data.Configurator.Types
import Data.Typeable (cast)

type Nick           = Text
type MsgHandlerName = Text

data User = Self | User { userNick :: !Nick, userServer :: !Text }
            deriving (Show, Eq)

data Message =
    IdleMsg      { msgTime  :: !UTCTime}
  | PingMsg      { msgTime  :: !UTCTime, msg     :: !Text, msgLine     :: !Text }
  | PongMsg      { msgTime  :: !UTCTime, msg     :: !Text, msgLine     :: !Text }
  | ChannelMsg   { msgTime  :: !UTCTime, user    :: !User, msg         :: !Text, msgLine :: !Text }
  | PrivMsg      { msgTime  :: !UTCTime, user    :: !User, msg         :: !Text, msgLine :: !Text }
  | ActionMsg    { msgTime  :: !UTCTime, user    :: !User, msg         :: !Text, msgLine :: !Text }
  | JoinMsg      { msgTime  :: !UTCTime, user    :: !User, msgLine     :: !Text }
  | QuitMsg      { msgTime  :: !UTCTime, user    :: !User, msg         :: !Text, msgLine :: !Text }
  | PartMsg      { msgTime  :: !UTCTime, user    :: !User, msg         :: !Text, msgLine :: !Text }
  | NickMsg      { msgTime  :: !UTCTime, user    :: !User, nick        :: !Nick, msgLine :: !Text }
  | NickInUseMsg { msgTime  :: !UTCTime, msgLine :: !Text }
  | KickMsg      { msgTime  :: !UTCTime, user    :: !User, kickedNick  :: !Nick, msg     :: !Text
                 , msgLine  :: !Text }
  | ModeMsg      { msgTime  :: !UTCTime, user    :: !User, target      :: !Text, mode    :: !Text
                 , modeArgs :: ![Text], msgLine  :: !Text }
  | NamesMsg     { msgTime  :: !UTCTime, nicks   :: ![Nick] }
  | OtherMsg     { msgTime  :: !UTCTime, source  :: !Text, command     :: !Text, target  :: !Text
                 , msg      :: !Text,   msgLine  :: !Text }
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

data BotConfig = BotConfig { server          :: !Text
                           , port            :: !Int
                           , channel         :: !Text
                           , botNick         :: !Text
                           , botTimeout      :: !Int
                           , msgHandlerNames :: ![MsgHandlerName]
                           , config          :: !Config }

instance Show BotConfig where
  show BotConfig { .. } = "server = "   ++ show server          ++ "\n" ++
                          "port = "     ++ show port            ++ "\n" ++
                          "channel = "  ++ show channel         ++ "\n" ++
                          "nick = "     ++ show botNick         ++ "\n" ++
                          "timeout = "  ++ show botTimeout      ++ "\n" ++
                          "handlers = " ++ show msgHandlerNames

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

data MsgHandler = MsgHandler {
  onMessage :: !(forall m . MonadMsgHandler m => Message -> m (Maybe Command)),
  onStop    :: !(forall m . MonadMsgHandler m => m ()),
  onEvent   :: !(forall m . MonadMsgHandler m => SomeEvent -> m EventResponse)
}

newMsgHandler :: MsgHandler
newMsgHandler = MsgHandler {
  onMessage = const $ return Nothing,
  onStop    = return (),
  onEvent   = const $ return RespNothing
}
