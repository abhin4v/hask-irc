{-# LANGUAGE RecordWildCards, RankNTypes, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleContexts #-}

module Network.IRC.Types
  (Channel, Nick, MsgHandlerName,
   User (..), Message (..), Command (..),
   BotConfig (..), BotStatus (..), Bot (..),
   IRC, runIRC,
   MonadMsgHandler, runMsgHandler, initMsgHandler, exitMsgHandler,
   MsgHandlerState, MsgHandlerStates, MsgHandler (..), newMsgHandler)
where

import ClassyPrelude
import Control.Monad.Reader
import Control.Monad.State
import Data.Configurator.Types
import Data.Dynamic

type Channel        = Text
type Nick           = Text
type MsgHandlerName = Text

data User = Self | User { userNick :: !Nick, userServer :: !Text }
            deriving (Show, Eq)

data Message =
    ChannelMsg { msgTime  :: !UTCTime, user   :: !User, msg         :: !Text, msgLine :: !Text }
  | PrivMsg    { msgTime  :: !UTCTime, user   :: !User, msg         :: !Text, msgLine :: !Text }
  | Ping       { msgTime  :: !UTCTime, msg    :: !Text, msgLine     :: !Text }
  | JoinMsg    { msgTime  :: !UTCTime, user   :: !User, msgLine     :: !Text }
  | QuitMsg    { msgTime  :: !UTCTime, user   :: !User, msg         :: !Text, msgLine :: !Text }
  | PartMsg    { msgTime  :: !UTCTime, user   :: !User, msg         :: !Text, msgLine :: !Text }
  | ModeMsg    { msgTime  :: !UTCTime, user   :: !User, target      :: !Text, mode    :: !Text
               , modeArgs :: ![Text], msgLine :: !Text }
  | NickMsg    { msgTime  :: !UTCTime, user   :: !User, nick        :: !Text, msgLine :: !Text }
  | KickMsg    { msgTime  :: !UTCTime, user   :: !User, kickedNick  :: !Text, msg     :: !Text
               , msgLine  :: !Text }
  | OtherMsg   { msgTime  :: !UTCTime, source :: !Text, command     :: !Text, target  :: !Text
               , msg      :: !Text,   msgLine :: !Text }
  deriving (Show, Eq)

data Command =
    Pong            { rmsg  :: !Text }
  | ChannelMsgReply { rmsg  :: !Text }
  | PrivMsgReply    { ruser :: !User, rmsg :: !Text }
  | NickCmd
  | UserCmd
  | JoinCmd
  deriving (Show, Eq)

data BotConfig = BotConfig { server      :: !Text
                           , port        :: !Int
                           , channel     :: !Text
                           , botNick     :: !Text
                           , botTimeout  :: !Int
                           , msgHandlers :: ![MsgHandlerName]
                           , config      :: !Config }

instance Show BotConfig where
  show BotConfig { .. } = "server = " ++ show server ++ "\n" ++
                          "port = " ++ show port ++ "\n" ++
                          "channel = " ++ show channel ++ "\n" ++
                          "nick = " ++ show botNick ++ "\n" ++
                          "timeout = " ++ show botTimeout ++ "\n" ++
                          "handlers = " ++ show msgHandlers

type MsgHandlerStates = Map MsgHandlerName MsgHandlerState

data Bot = Bot { botConfig        :: !BotConfig
               , socket           :: !Handle
               , msgHandlerStates :: !MsgHandlerStates}

data BotStatus = Connected | Disconnected | Joined | Kicked | Errored
                 deriving (Show, Eq)

newtype IRC a = IRC { _runIRC :: StateT BotStatus (ReaderT Bot IO) a }
                deriving ( Functor
                         , Applicative
                         , Monad
                         , MonadIO
                         , MonadReader Bot
                         , MonadState BotStatus)

runIRC :: Bot -> BotStatus -> IRC a -> IO BotStatus
runIRC bot botStatus = flip runReaderT bot . flip execStateT botStatus . _runIRC

type MsgHandlerState = Dynamic

newtype MsgHandlerT a = MsgHandlerT { _runMsgHandler :: StateT MsgHandlerState (ReaderT BotConfig IO) a }
                     deriving ( Functor
                              , Applicative
                              , Monad
                              , MonadIO
                              , MonadState MsgHandlerState
                              , MonadReader BotConfig)

class ( MonadIO m, Applicative m
      , MonadState MsgHandlerState m, MonadReader BotConfig m) => MonadMsgHandler m where
  msgHandler :: MsgHandlerT a -> m a

instance MonadMsgHandler MsgHandlerT where
  msgHandler = id

runMsgHandler :: MsgHandler -> BotConfig -> MsgHandlerState -> Message -> IO (Maybe Command)
runMsgHandler MsgHandler { .. } botConfig msgHandlerState =
  flip runReaderT botConfig . flip evalStateT msgHandlerState . _runMsgHandler . msgHandlerRun

initMsgHandler :: MsgHandler -> BotConfig -> IO MsgHandlerState
initMsgHandler MsgHandler { .. } botConfig =
  flip runReaderT botConfig . flip execStateT (toDyn ()) . _runMsgHandler $ msgHandlerInit

exitMsgHandler :: MsgHandler -> BotConfig -> MsgHandlerState -> IO ()
exitMsgHandler MsgHandler { .. } botConfig msgHandlerState =
  flip runReaderT botConfig . flip evalStateT msgHandlerState . _runMsgHandler $ msgHandlerExit

data MsgHandler = MsgHandler { msgHandlerInit :: !(forall m . MonadMsgHandler m => m ())
                             , msgHandlerRun  :: !(forall m . MonadMsgHandler m => Message -> m (Maybe Command))
                             , msgHandlerExit :: !(forall m . MonadMsgHandler m => m ()) }

newMsgHandler :: MsgHandler
newMsgHandler = MsgHandler { msgHandlerInit = return ()
                           , msgHandlerRun  = const $ return Nothing
                           , msgHandlerExit = return () }
