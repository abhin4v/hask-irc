{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.IRC.Internal.Types
  (
    -- * Messages and Commands
    Nick (..)
  , User (..)
  , Message (..)
  , MessageDetails (..)
  , Command (..)
  -- * Events
  , Event (..)
  , SomeEvent
  , EventResponse (..)
  , QuitEvent(..)
  -- * Bot
  , BotConfig (..)
  , Bot (..)
  , BotStatus (..)
  , IRC
  , runIRC
  -- * Message handlers
  , MsgHandlerName
  , MonadMsgHandler
  , MsgHandler (..)
  , newMsgHandler
  , MsgHandlerMaker (..)
  , handleMessage
  , handleEvent
  , stopMsgHandler
  , getHelp
  ) where

import ClassyPrelude
import Control.Concurrent.Lifted (Chan)
import Control.Monad.Base        (MonadBase)
import Control.Monad.Reader      (ReaderT, MonadReader, runReaderT)
import Control.Monad.State       (StateT, MonadState, execStateT)
import Data.Configurator.Types   (Config)
import Data.Data                 (Data)
import Data.SafeCopy             (base, deriveSafeCopy)
import Data.Typeable             (cast)

import Network.IRC.Util

-- * Types
-- ** IRC related

-- | An IRC nick.
newtype Nick = Nick { nickToText :: Text }
               deriving (Eq, Ord, Data, Typeable, Hashable)

instance Show Nick where
  show = unpack . nickToText

$(deriveSafeCopy 0 'base ''Nick)

-- | An IRC user.
data User
  -- | The user for the bot itself.
  = Self
  -- | An user other than the bot.
  | User
  { userNick :: !Nick         -- ^ The user's nick.
  , userServer :: !Text       -- ^ The user's server.
  } deriving (Show, Eq, Ord)

-- | An IRC message sent from the server to the bot.
data Message = Message
  { msgTime :: !UTCTime            -- ^ The time when the message was received.
  , msgLine :: !Text               -- ^ The raw message line.
  , msgDetails :: MessageDetails   -- ^ The details of the parsed message.
  } deriving (Show, Eq, Ord)

-- | Different types of IRC messages.
data MessageDetails
  -- | The internal (non-IRC) message received when the bot is idle.
  = IdleMsg
  -- | The message received when the bot's current nick is already in use.
  | NickInUseMsg
  -- | A /PING/ message. Must be replied with a 'PongCmd'.
  | PingMsg      { msg       :: !Text }
  -- | A /PONG/ message. Received in response to a 'PingCmd'.
  | PongMsg      { msg       :: !Text }
  -- | A /NAMES/ message which contains a list of nicks of all users in the channel.
  | NamesMsg     { nicks     :: ![Nick] }
  -- | A /PRIVMSG/ message sent to the channel from a user.
  | ChannelMsg   { user      :: !User, msg        :: !Text }
  -- | A /PRIVMSG/ private message sent to the bot from a user.
  | PrivMsg      { user      :: !User, msg        :: !Text }
  -- | An /PRIVMSG/ action message sent to the channel from a user.
  | ActionMsg    { user      :: !User, msg        :: !Text }
  -- | A /JOIN/ message received when a user joins the channel.
  | JoinMsg      { user      :: !User }
  -- | A /QUIT/ message received when a user quits the server.
  | QuitMsg      { user      :: !User, msg        :: !Text }
  -- | A /PART/ message received when a user leaves the channel.
  | PartMsg      { user      :: !User, msg        :: !Text }
  -- | A /NICK/ message received when a user changes their nick.
  | NickMsg      { user      :: !User, newNick    :: !Nick }
  -- | A /KICK/ message received when a user kicks another user from the channel.
  | KickMsg      { user      :: !User, kickedNick :: !Nick, msg       :: !Text }
  -- | A /MODE/ message received when a user's mode changes.
  | ModeMsg      { user      :: !User, msgTarget  :: !Text, mode      :: !Text , modeArgs :: ![Text] }
  -- | All other messages which are not parsed as any of the above types.
  | OtherMsg     { msgSource :: !Text, msgCommand :: !Text, msgTarget :: !Text , msg      :: !Text }
  deriving (Show, Eq, Ord)

-- | IRC commands sent from the bot to the server.
data Command
  -- | A /PING/ command. A 'PongMsg' is expected as a response to this.
  = PingCmd         { rmsg  :: !Text }
  -- | A /PONG/ command. Sent in response to a 'PingMsg'.
  | PongCmd         { rmsg  :: !Text }
  -- | A /PRIVMSG/ message sent to the channel.
  | ChannelMsgReply { rmsg  :: !Text }
  -- | A /PRIVMSG/ message sent to a user.
  | PrivMsgReply    { ruser :: !User, rmsg :: !Text }
  -- | A /NICK/ command sent to set the bot's nick.
  | NickCmd
  -- | A /USER/ command sent to identify the bot.
  | UserCmd
  -- | A /JOIN/ command sent to join the channel.
  | JoinCmd
  -- | A /QUIT/ command sent to quit the server.
  | QuitCmd
  -- | A /NAMES/ command sent to ask for the nicks of the users in the channel.
  | NamesCmd
  deriving (Show, Eq, Ord)

-- ** Events

-- | Events are used for communication between message handlers. To send events, write them to the
-- event channel provided to the 'MsgHandler' when it is created. To receive events, provide
-- an 'onEvent' function as a part of the message handler.
class (Typeable e, Show e, Eq e) => Event e where
  -- | Creates an event.
  toEvent :: e -> IO SomeEvent
  toEvent e = SomeEvent <$> pure e <*> getCurrentTime

  -- | Extracts a received event.
  fromEvent :: SomeEvent -> Maybe (e, UTCTime)
  fromEvent (SomeEvent e time) = do
    ev <- cast e
    return (ev, time)

-- | A wrapper over all events to allow sending them over channel of same type.
data SomeEvent = forall e. (Event e, Typeable e) => SomeEvent e UTCTime deriving (Typeable)
instance Show SomeEvent where
  show (SomeEvent e time) = formatTime defaultTimeLocale "[%F %T] " time ++ show e
instance Eq SomeEvent where
  SomeEvent e1 t1 == SomeEvent e2 t2 =
    case cast e2 of
      Just e2' -> e1 == e2' && t1 == t2
      Nothing  -> False

-- | Response to an event received by a message handler.
data EventResponse
  = RespNothing            -- ^ No response
  | RespEvent [SomeEvent]  -- ^ Events as the response. They will be sent to all message handlers like usual events.
  | RespMessage [Message]  -- ^ Messages as the response. They will be sent to all message handlers like usual messages.
  | RespCommand [Command]  -- ^ Commands as the response. They will be sent to the server like usual commands.
  deriving (Show, Eq)

-- | An event signifying the bot quitting the server.
data QuitEvent = QuitEvent deriving (Show, Eq, Ord, Typeable)
instance Event QuitEvent

-- ** Bot

-- | Name of a message handler.
type MsgHandlerName = Text

-- | The configuration for running the bot.
data BotConfig = BotConfig
  {
  -- | The server to connect to.
    server           :: !Text
  -- | The port to connect to.
  , port             :: !Int
  -- | The channel to join.
  , channel          :: !Text
  -- | Nick of the bot.
  , botNick          :: !Nick
  -- | The timeout in seconds after which bot automatically disconnects and tries to reconnect.
  -- Should be few seconds more than the ping timeout of the server.
  , botTimeout       :: !Int
  -- | Info about the message handlers. A map of message handler names to a map of all commands supported
  -- by that message handler to the help text of that command.
  , msgHandlerInfo   :: !(Map MsgHandlerName (Map Text Text))
  -- | A list of 'MsgHandlerMaker's which are used to create message handlers for the bot.
  , msgHandlerMakers :: ![MsgHandlerMaker]
  -- | All the bot configuration so that message handlers can lookup their own specific configs.
  , config           :: !Config
  }

instance Show BotConfig where
  show BotConfig { .. } = "server = "   ++ show server     ++ "\n" ++
                          "port = "     ++ show port       ++ "\n" ++
                          "channel = "  ++ show channel    ++ "\n" ++
                          "nick = "     ++ show botNick    ++ "\n" ++
                          "timeout = "  ++ show botTimeout ++ "\n" ++
                          "handlers = " ++ show (mapKeys msgHandlerInfo)

-- | The bot.
data Bot = Bot
  {
  -- | The config for the bot.
    botConfig   :: !BotConfig
  -- | The network socket on which the bot communicates with the server.
  , botSocket   :: !Handle
  -- | The message handlers attached with the bot as a map of message handler names to the message handlers.
  , msgHandlers :: !(Map MsgHandlerName MsgHandler)
  }

-- | The current status of the bot.
data BotStatus = Connected                -- ^ Connected to the server
               | Disconnected             -- ^ Disconnected from the server.
               | Joined                   -- ^ Joined the channel.
               | Kicked                   -- ^ Kicked from the channel.
               | Errored                  -- ^ Some unhandled error happened.
               | Idle                     -- ^ No communication with the server. The bot is idle.
                                          -- If the bot stays idle for 'botTimeout' seconds, it disconnects.
               | Interrupted              -- ^ Interrupted using external signals like SIGINT.
               | NickNotAvailable         -- ^ Bot's nick already taken on the server.
               deriving (Show, Eq, Ord)

-- | An IRC action to be run.
newtype IRC a = IRC { _runIRC :: StateT BotStatus (ReaderT Bot IO) a }
                deriving ( Functor
                         , Applicative
                         , Monad
                         , MonadIO
                         , MonadReader Bot
                         , MonadState BotStatus
                         )

-- | Runs the bot action.
runIRC :: Bot           -- ^ The bot.
       -> BotStatus     -- ^ The bot status.
       -> IRC a         -- ^ The bot action to run.
       -> IO BotStatus  -- ^ IO action which returns the next bot status.
runIRC bot botStatus = flip runReaderT bot . flip execStateT botStatus . _runIRC

-- ** Message handlers

newtype MsgHandlerT a = MsgHandlerT { _runMsgHandler :: ReaderT BotConfig IO a }
                        deriving ( Functor
                                 , Applicative
                                 , Monad
                                 , MonadIO
                                 , MonadBase IO
                                 , MonadReader BotConfig
                                 )

-- | The monad in which message handlers actions run.
class (MonadIO m, Applicative m, MonadReader BotConfig m, MonadBase IO m) => MonadMsgHandler m where
  msgHandler :: MsgHandlerT a -> m a

instance MonadMsgHandler MsgHandlerT where
  msgHandler = id

-- | A message handler containing actions which are invoked by the bot.
data MsgHandler = MsgHandler
  {
  -- | The action invoked when a message is received. It returns a list of commands in response
  -- to the message which the bot sends to the server.
    onMessage :: !(forall m . MonadMsgHandler m => Message -> m [Command])
  -- | The action invoked when an event is triggered. It returns an event resonpse which the bot
  -- handles according to its type.
  , onEvent   :: !(forall m . MonadMsgHandler m => SomeEvent -> m EventResponse)
  -- | The action invoked to stop the message handler.
  , onStop    :: !(forall m . MonadMsgHandler m => m ())
  -- | The action invoked to get the map of the commands supported by the message handler and their help messages.
  , onHelp    :: !(forall m . MonadMsgHandler m => m (Map Text Text))
  }

-- | Creates a new message handler which doesn't do anything.
newMsgHandler :: MsgHandler
newMsgHandler = MsgHandler
  { onMessage = const $ return []
  , onStop    = return ()
  , onEvent   = const $ return RespNothing
  , onHelp    = return mempty
  }

-- | A message handler maker which creates a new message handler.
data MsgHandlerMaker = MsgHandlerMaker
  {
  -- | The name of the message handler.
    msgHandlerName  :: !MsgHandlerName
  -- | The action which is invoked to create a new message handler.
  , msgHandlerMaker :: !(BotConfig -> Chan SomeEvent -> MsgHandlerName -> IO (Maybe MsgHandler))
  }

instance Eq MsgHandlerMaker where
  m1 == m2 = msgHandlerName m1 == msgHandlerName m2
instance Ord MsgHandlerMaker where
  m1 `compare` m2 = msgHandlerName m1 `compare` msgHandlerName m2

-- | Handles a message using a given message handler.
handleMessage :: MsgHandler    -- ^ The message handler.
              -> BotConfig     -- ^ The bot config.
              -> Message       -- ^ The message to handle.
              -> IO [Command]  -- ^ A list of commands to be sent to the server.
handleMessage MsgHandler { .. } botConfig =
  flip runReaderT botConfig . _runMsgHandler . onMessage

-- | Handles an event using a given message handler.
handleEvent :: MsgHandler        -- ^ The message handler.
            -> BotConfig         -- ^ The bot config.
            -> SomeEvent         -- ^ The event to handle.
            -> IO EventResponse  -- ^ The event response which will be dispatched by the bot.
handleEvent MsgHandler { .. } botConfig =
  flip runReaderT botConfig . _runMsgHandler . onEvent

-- | Stops a message handler.
stopMsgHandler :: MsgHandler    -- ^ The message handler.
               -> BotConfig     -- ^ The bot config.
               -> IO ()
stopMsgHandler MsgHandler { .. } botConfig =
  flip runReaderT botConfig . _runMsgHandler $ onStop

-- | Gets the help messages for a given message handler.
getHelp :: MsgHandler          -- ^ The message handler.
        -> BotConfig           -- ^ The bot config.
        -> IO (Map Text Text)  -- ^ A map of the commands supported by this message handler  to their help messages.
getHelp MsgHandler { .. } botConfig =
  flip runReaderT botConfig . _runMsgHandler $ onHelp
