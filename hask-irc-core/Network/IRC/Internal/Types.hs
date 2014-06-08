{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.IRC.Internal.Types where

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
data FullMessage = FullMessage
  { msgTime :: !UTCTime  -- ^ The time when the message was received.
  , msgLine :: !Text     -- ^ The raw message line.
  , message :: Message   -- ^ The details of the parsed message.
  } deriving (Show, Eq)

-- | The typeclass for different types of IRC messages.
class (Typeable msg, Show msg, Eq msg, Ord msg) => MessageC msg where
  toMessage :: msg -> Message
  toMessage = Message

  fromMessage :: Message -> Maybe msg
  fromMessage (Message msg) = cast msg

-- | A wrapper over all types of IRC messages.
data Message = forall m . MessageC m => Message m deriving (Typeable)
instance Show Message where
  show (Message m) = show m
instance Eq Message where
  Message m1 == Message m2 = case cast m1 of
    Just m1' -> m1' == m2
    _        -> False

-- | The internal (non-IRC) message received when the bot is idle.
data IdleMsg      = IdleMsg deriving (Typeable, Show, Eq, Ord)
instance MessageC IdleMsg

-- | The message received when the bot's current nick is already in use.
data NickInUseMsg = NickInUseMsg deriving (Typeable, Show, Eq, Ord)
instance MessageC NickInUseMsg

-- | A /PING/ message. Must be replied with a 'PongCmd'.
data PingMsg      = PingMsg !Text deriving (Typeable, Show, Eq, Ord)
instance MessageC PingMsg

-- | A /PONG/ message. Received in response to a 'PingCmd'.
data PongMsg      = PongMsg !Text deriving (Typeable, Show, Eq, Ord)
instance MessageC PongMsg

-- | A /NAMES/ message which contains a list of nicks of all users in the channel.
data NamesMsg     = NamesMsg ![Nick] deriving (Typeable, Show, Eq, Ord)
instance MessageC NamesMsg

-- | A /PRIVMSG/ message sent to the channel from a user.
data ChannelMsg   = ChannelMsg !User !Text deriving (Typeable, Show, Eq, Ord)
instance MessageC ChannelMsg

-- | A /PRIVMSG/ private message sent to the bot from a user.
data PrivMsg      = PrivMsg !User !Text deriving (Typeable, Show, Eq, Ord)
instance MessageC PrivMsg

-- | An /PRIVMSG/ action message sent to the channel from a user.
data ActionMsg    = ActionMsg !User !Text deriving (Typeable, Show, Eq, Ord)
instance MessageC ActionMsg

-- | A /JOIN/ message received when a user joins the channel.
data JoinMsg      = JoinMsg !User deriving (Typeable, Show, Eq, Ord)
instance MessageC JoinMsg

-- | A /QUIT/ message received when a user quits the server.
data QuitMsg      = QuitMsg !User !Text deriving (Typeable, Show, Eq, Ord)
instance MessageC QuitMsg

-- | A /PART/ message received when a user leaves the channel.
data PartMsg      = PartMsg !User !Text deriving (Typeable, Show, Eq, Ord)
instance MessageC PartMsg

-- | A /NICK/ message received when a user changes their nick.
data NickMsg      = NickMsg !User !Nick deriving (Typeable, Show, Eq, Ord)
instance MessageC NickMsg

-- | A /KICK/ message received when a user kicks another user from the channel.
data KickMsg      = KickMsg { kickUser :: !User, kickedNick :: !Nick, kickMsg :: !Text }
               deriving (Typeable, Show, Eq, Ord)
instance MessageC KickMsg

-- | A /MODE/ message received when a user's mode changes.
data ModeMsg      = ModeMsg { modeUser :: !User, modeTarget :: !Text, mode :: !Text , modeArgs :: ![Text] }
               deriving (Typeable, Show, Eq, Ord)
instance MessageC ModeMsg

-- | All other messages which are not parsed as any of the above types.
data OtherMsg     = OtherMsg { msgSource :: !Text, msgCommand :: !Text, msgTarget :: !Text , msg :: !Text }
                deriving (Typeable, Show, Eq, Ord)
instance MessageC OtherMsg

-- | The typeclass for IRC commands sent from the bot to the server.
class (Typeable cmd, Show cmd, Eq cmd, Ord cmd) => CommandC cmd where
  toCommand :: cmd -> Command
  toCommand = Command

  fromCommand :: Command -> Maybe cmd
  fromCommand (Command cmd) = cast cmd

-- | A wrapper over all types of IRC commands.
data Command = forall m . CommandC m => Command m deriving (Typeable)
instance Show Command where
  show (Command m) = show m
instance Eq Command where
  Command m1 == Command m2 = case cast m1 of
    Just m1' -> m1' == m2
    _        -> False

-- | A /PING/ command. A 'PongMsg' is expected as a response to this.
data PingCmd         = PingCmd !Text deriving (Typeable, Show, Eq, Ord)
instance CommandC PingCmd

-- | A /PONG/ command. Sent in response to a 'PingMsg'.
data PongCmd         = PongCmd !Text deriving (Typeable, Show, Eq, Ord)
instance CommandC PongCmd

-- | A /PRIVMSG/ message sent to the channel.
data ChannelMsgReply = ChannelMsgReply !Text deriving (Typeable, Show, Eq, Ord)
instance CommandC ChannelMsgReply

-- | A /PRIVMSG/ message sent to a user.
data PrivMsgReply    = PrivMsgReply !User !Text deriving (Typeable, Show, Eq, Ord)
instance CommandC PrivMsgReply

-- | A /NICK/ command sent to set the bot's nick.
data NickCmd         = NickCmd deriving (Typeable, Show, Eq, Ord)
instance CommandC NickCmd

-- | A /USER/ command sent to identify the bot.
data UserCmd         = UserCmd deriving (Typeable, Show, Eq, Ord)
instance CommandC UserCmd

-- | A /JOIN/ command sent to join the channel.
data JoinCmd         = JoinCmd deriving (Typeable, Show, Eq, Ord)
instance CommandC JoinCmd

-- | A /QUIT/ command sent to quit the server.
data QuitCmd         = QuitCmd deriving (Typeable, Show, Eq, Ord)
instance CommandC QuitCmd

-- | A /NAMES/ command sent to ask for the nicks of the users in the channel.
data NamesCmd        = NamesCmd deriving (Typeable, Show, Eq, Ord)
instance CommandC NamesCmd

-- ** Message Parsing

-- | Message parser id. Should be unique.
type MessageParserId = Text

-- | A part of a mutlipart message.
data MessagePart = MessagePart { msgPartParserId   :: !MessageParserId
                               , msgPartTarget     :: !Text
                               , msgPartTime       :: !UTCTime
                               , msgPartLine       :: !Text }
                   deriving (Eq, Show)

-- | The result of parsing a message line.
data MessageParseResult =
    Done !FullMessage ![MessagePart] -- ^ A fully parsed message and leftover message parts.
  | Partial ![MessagePart]           -- ^ A partial message with message parts received yet.
  | Reject                           -- ^ Returned if a message line cannot be parsed by a particular parser.
  deriving (Eq, Show)

-- | A message parser used for parsing text lines from the server to 'Message's.
data MessageParser = MessageParser
  { msgParserId :: !MessageParserId
  , msgParser   :: !(BotConfig -> UTCTime -> Text -> [MessagePart] -> MessageParseResult)
  }

-- ** Command Formatting

-- | A command formatter which optinally formats commands to texts which are then send to the server.
type CommandFormatter = BotConfig -> Command -> Maybe Text

-- ** Events

-- | Events are used for communication between message handlers. To send events, write them to the
-- event channel provided to the 'MsgHandler' when it is created. To receive events, provide
-- an 'onEvent' function as a part of the message handler.
class (Typeable e, Show e, Eq e) => EventC e where
  -- | Creates an event.
  toEvent :: e -> IO Event
  toEvent e = Event <$> pure e <*> getCurrentTime

  -- | Extracts a received event.
  fromEvent :: Event -> Maybe (e, UTCTime)
  fromEvent (Event e time) = do
    ev <- cast e
    return (ev, time)

-- | A wrapper over all types of 'Event's to allow sending them over channel of same type.
data Event = forall e. (EventC e, Typeable e) => Event e UTCTime deriving (Typeable)
instance Show Event where
  show (Event e time) = formatTime defaultTimeLocale "[%F %T] " time ++ show e
instance Eq Event where
  Event e1 t1 == Event e2 t2 =
    case cast e2 of
      Just e2' -> e1 == e2' && t1 == t2
      Nothing  -> False

-- | Response to an event received by a message handler.
data EventResponse =
  -- | No response
    RespNothing
  -- | Events as the response. They will be sent to all message handlers like usual events.
  | RespEvent [Event]
  -- | Messages as the response. They will be sent to all message handlers like usual messages.
  | RespMessage [FullMessage]
  -- | Commands as the response. They will be sent to the server like usual commands.
  | RespCommand [Command]
  deriving (Show, Eq)

-- | An event signifying the bot quitting the server.
data QuitEvent = QuitEvent deriving (Show, Eq, Ord, Typeable)
instance EventC QuitEvent

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
  -- | A list of extra message parsers. Note that these parsers will always be called after the built-in ones.
  , msgParsers       :: ![MessageParser]
  -- | A list of extra command formatters. Note that these formatters will always be called after the built-in ones.
  , cmdFormatters    :: ![CommandFormatter]
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

-- | Creates a new bot config with some fields as empty.
newBotConfig :: Text                               -- ^ server
             -> Int                                -- ^ port
             -> Text                               -- ^ channel
             -> Nick                               -- ^ botNick
             -> Int                                -- ^ botTimeout
             -> Map MsgHandlerName (Map Text Text) -- ^ msgHandlerInfo
             -> Config                             -- ^ config
             -> BotConfig
newBotConfig server port channel botNick botTimeout msgHandlerInfo =
  BotConfig server port channel botNick botTimeout msgHandlerInfo [] [] []

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
  fromMsgHandler :: MsgHandlerT a -> m a

instance MonadMsgHandler MsgHandlerT where
  fromMsgHandler = id

-- | A message handler containing actions which are invoked by the bot.
data MsgHandler = MsgHandler
  {
  -- | The action invoked when a message is received. It returns a list of commands in response
  -- to the message which the bot sends to the server.
    onMessage :: !(forall m . MonadMsgHandler m => FullMessage -> m [Command])
  -- | The action invoked when an event is triggered. It returns an event resonpse which the bot
  -- handles according to its type.
  , onEvent   :: !(forall m . MonadMsgHandler m => Event -> m EventResponse)
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
  , msgHandlerMaker :: !(BotConfig -> Chan Event -> MsgHandlerName -> IO (Maybe MsgHandler))
  }

instance Eq MsgHandlerMaker where
  m1 == m2 = msgHandlerName m1 == msgHandlerName m2
instance Ord MsgHandlerMaker where
  m1 `compare` m2 = msgHandlerName m1 `compare` msgHandlerName m2

-- | Handles a message using a given message handler.
handleMessage :: MsgHandler    -- ^ The message handler.
              -> BotConfig     -- ^ The bot config.
              -> FullMessage   -- ^ The message to handle.
              -> IO [Command]  -- ^ A list of commands to be sent to the server.
handleMessage MsgHandler { .. } botConfig =
  flip runReaderT botConfig . _runMsgHandler . onMessage

-- | Handles an event using a given message handler.
handleEvent :: MsgHandler        -- ^ The message handler.
            -> BotConfig         -- ^ The bot config.
            -> Event             -- ^ The event to handle.
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
