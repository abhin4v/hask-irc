{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Network.IRC.Internal.Types where

import qualified Data.Configurator as CF

import ClassyPrelude
import Control.Monad.Base      (MonadBase)
import Control.Monad.State     (StateT, MonadState, execStateT)
import Data.Configurator.Types (Config)

import Network.IRC.Message.Types
import Network.IRC.MessageBus
import Network.IRC.Util

-- ** Message Parsing

-- | Message parser id. Should be unique.
type MessageParserId = Text

-- | A part of a mutlipart message.
data MessagePart = MessagePart { msgPartParserId :: !MessageParserId
                               , msgPartTarget   :: !Text
                               , msgPartTime     :: !UTCTime
                               , msgPartLine     :: !Text
                               } deriving (Eq, Show)

-- | The result of parsing a message line.
data MessageParseResult =
    Done !Message ![MessagePart]  -- ^ A fully parsed message and leftover message parts.
  | Partial ![MessagePart]        -- ^ A partial message with message parts received yet.
  | Reject                        -- ^ Returned if a message line cannot be parsed by a particular parser.
  deriving (Eq, Show)

-- | A message parser used for parsing text lines from the server to 'Message's.
data MessageParser = MessageParser
  { msgParserId :: !MessageParserId
  , msgParser   :: !(BotConfig -> UTCTime -> Text -> [MessagePart] -> MessageParseResult)
  }

-- ** Command Formatting

-- | A command formatter which optinally formats commands to texts which are then send to the server.
type CommandFormatter = BotConfig -> Message -> Maybe Text

-- ** Bot

-- | Name of a message handler.
type MsgHandlerName = Text

-- | The configuration for running the bot.
data BotConfig = BotConfig
  {
  -- | The server to connect to.
    botServer        :: !Text
  -- | The port to connect to.
  , botPort          :: !Int
  -- | The channel to join.
  , botChannel       :: !Text
  -- | Nick of the bot.
  , botNick          :: !Nick
  -- | The timeout in seconds after which bot automatically disconnects and tries to reconnect.
  -- Should be few seconds more than the ping timeout of the server.
  , botTimeout       :: !Int
  -- | Info about the message handlers. A map of message handler names to a map of all commands supported
  -- by that message handler to the help text of that command.
  , msgHandlerInfo   :: !(Map MsgHandlerName (Map Text Text))
  -- | A list of 'MsgHandlerMaker's which are used to create message handlers for the bot.
  , msgHandlerMakers :: !(Map MsgHandlerName MsgHandlerMaker)
  -- | A list of extra message parsers. Note that these parsers will always be called after the built-in ones.
  , msgParsers       :: ![MessageParser]
  -- | A list of extra command formatters. Note that these formatters will always be called after the built-in ones.
  , cmdFormatters    :: ![CommandFormatter]
  -- | All the bot configuration so that message handlers can lookup their own specific configs.
  , config           :: !Config
  }

instance Show BotConfig where
  show BotConfig { .. } = "BotConfig {"                                  ++ "\n" ++
                          "server = "   ++ show botServer                ++ "\n" ++
                          "port = "     ++ show botPort                  ++ "\n" ++
                          "channel = "  ++ show botChannel               ++ "\n" ++
                          "nick = "     ++ show botNick                  ++ "\n" ++
                          "timeout = "  ++ show botTimeout               ++ "\n" ++
                          "handlers = " ++ show (mapKeys msgHandlerInfo) ++ " }"

-- | Creates a new bot config with essential fields leaving rest fields empty.
newBotConfig :: Text       -- ^ server
             -> Int        -- ^ port
             -> Text       -- ^ channel
             -> Nick       -- ^ botNick
             -> Int        -- ^ botTimeout
             -> BotConfig
newBotConfig server port channel botNick botTimeout =
  BotConfig server port channel botNick botTimeout mempty mempty [] [] CF.empty

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
data BotStatus = Connected               -- ^ Connected to the server
               | Disconnected            -- ^ Disconnected from the server.
               | Joined                  -- ^ Joined the channel.
               | Kicked                  -- ^ Kicked from the channel.
               | Errored                 -- ^ Some unhandled error happened.
               | Idle                    -- ^ No communication with the server. The bot is idle.
                                         -- If the bot stays idle for 'botTimeout' seconds, it disconnects.
               | Interrupted             -- ^ Interrupted using external signals like SIGINT.
               | NickNotAvailable        -- ^ Bot's nick already taken on the server.
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
    onMessage   :: !(forall m . MonadMsgHandler m => Message -> m [Message])

  -- | The action invoked to stop the message handler.
  , onStop      :: !(forall m . MonadMsgHandler m => m ())

  -- | The action invoked to get the map of the commands supported by the message handler and their help messages.
  , handlerHelp :: !(forall m . MonadMsgHandler m => m (Map Text Text))
  }

-- | Creates a new message handler which doesn't do anything.
newMsgHandler :: MsgHandler
newMsgHandler = MsgHandler
  { onMessage   = const $ return mempty
  , onStop      = return ()
  , handlerHelp = return mempty
  }

-- | A message handler maker which creates a new message handler.
data MsgHandlerMaker = MsgHandlerMaker
  {
  -- | The name of the message handler.
    msgHandlerName  :: !MsgHandlerName
  -- | The action which is invoked to create a new message handler.
  , msgHandlerMaker :: !(BotConfig -> MessageChannel Message -> IO MsgHandler)
  }

instance Eq MsgHandlerMaker where
  m1 == m2 = msgHandlerName m1 == msgHandlerName m2
instance Ord MsgHandlerMaker where
  m1 `compare` m2 = msgHandlerName m1 `compare` msgHandlerName m2

-- | Handles a message using a given message handler.
handleMessage :: MsgHandler    -- ^ The message handler.
              -> BotConfig     -- ^ The bot config.
              -> Message   -- ^ The message to handle.
              -> IO [Message]  -- ^ A list of commands to be sent to the server.
handleMessage MsgHandler { .. } botConfig =
  flip runReaderT botConfig . _runMsgHandler . onMessage

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
  flip runReaderT botConfig . _runMsgHandler $ handlerHelp
