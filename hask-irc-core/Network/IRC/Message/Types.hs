{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK hide #-}

module Network.IRC.Message.Types where

import ClassyPrelude
import Data.Data     (Data)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Typeable (cast)

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
  { userNick   :: !Nick       -- ^ The user's nick.
  , userServer :: !Text       -- ^ The user's server.
  } deriving (Show, Eq, Ord)

-- | An message sent from the server to the bot or from the bot to the server
-- or from a handler to another handler.
data Message = Message
  { msgTime :: !UTCTime   -- ^ The time when the message was received/sent.
  , msgLine :: !Text      -- ^ The raw message.
  , message :: !MessageW  -- ^ The details of the parsed message.
  } deriving (Show, Eq)

-- | The typeclass for different types of messages.
class (Typeable msg, Show msg, Eq msg, Ord msg) => MessageC msg where
  toMessage :: msg -> MessageW
  toMessage = MessageW

  fromMessage :: MessageW -> Maybe msg
  fromMessage (MessageW msg) = cast msg

-- | A wrapper over all types of messages.
data MessageW = forall m . MessageC m => MessageW !m deriving (Typeable)

instance Show MessageW where
  show (MessageW m) = show m

instance Eq MessageW where
  MessageW m1 == MessageW m2 = case cast m1 of
    Just m1' -> m1' == m2
    _        -> False

-- | Creates a new message with the current time and the given message details.
newMessage :: (MessageC msg, MonadIO m)
           => msg        -- ^ Message details
           -> m Message
newMessage msg = do
  t <- liftIO getCurrentTime
  return $ Message t "" (toMessage msg)

-- | The internal (non-IRC) message received when the bot is idle.
data IdleMsg       = IdleMsg deriving (Typeable, Show, Eq, Ord)
instance MessageC IdleMsg

-- | The message received when the bot's current nick is already in use.
data NickInUseMsg  = NickInUseMsg deriving (Typeable, Show, Eq, Ord)
instance MessageC NickInUseMsg

-- | A /PING/ message. Must be replied with a 'PongCmd'.
data PingMsg       = PingMsg !Text deriving (Typeable, Show, Eq, Ord)
instance MessageC PingMsg

-- | A /PONG/ message. Received in response to a 'PingCmd'.
data PongMsg       = PongMsg !Text deriving (Typeable, Show, Eq, Ord)
instance MessageC PongMsg

-- | A /NAMES/ message which contains a list of nicks of all users in the channel.
data NamesMsg      = NamesMsg ![Nick] deriving (Typeable, Show, Eq, Ord)
instance MessageC NamesMsg

-- | A /PRIVMSG/ message sent to the channel from a user.
data ChannelMsg    = ChannelMsg !User !Text deriving (Typeable, Show, Eq, Ord)
instance MessageC ChannelMsg

-- | A /PRIVMSG/ private message sent to the bot from a user.
data PrivMsg       = PrivMsg !User !Text deriving (Typeable, Show, Eq, Ord)
instance MessageC PrivMsg

-- | An /PRIVMSG/ action message sent to the channel from a user.
data ActionMsg     = ActionMsg !User !Text deriving (Typeable, Show, Eq, Ord)
instance MessageC ActionMsg

-- | A /JOIN/ message received when a user joins the channel.
data JoinMsg       = JoinMsg !User deriving (Typeable, Show, Eq, Ord)
instance MessageC JoinMsg

-- | A /QUIT/ message received when a user quits the server.
data QuitMsg       = QuitMsg !User !Text deriving (Typeable, Show, Eq, Ord)
instance MessageC QuitMsg

-- | A /PART/ message received when a user leaves the channel.
data PartMsg       = PartMsg !User !Text deriving (Typeable, Show, Eq, Ord)
instance MessageC PartMsg

-- | A /NICK/ message received when a user changes their nick.
data NickMsg       = NickMsg !User !Nick deriving (Typeable, Show, Eq, Ord)
instance MessageC NickMsg

-- | A /KICK/ message received when a user kicks another user from the channel.
data KickMsg       = KickMsg { kickUser :: !User, kickedNick :: !Nick, kickMsg :: !Text }
                    deriving (Typeable, Show, Eq, Ord)
instance MessageC KickMsg

-- | A /MODE/ message received when a user's mode changes.
data ModeMsg       = ModeMsg { modeUser   :: !User
                             , modeTarget :: !Text
                             , mode       :: !Text
                             , modeArgs   :: ![Text]
                             } deriving (Typeable, Show, Eq, Ord)
instance MessageC ModeMsg

-- | A message received as a response to a 'WhoisCmd'.
data WhoisReplyMsg = WhoisNoSuchNick { whoisNick :: !Nick }
                   | WhoisReplyMsg {
                       whoisNick        :: !Nick
                     , whoisUser        :: !Text
                     , whoisHost        :: !Text
                     , whoisRealName    :: !Text
                     , whoisChannels    :: ![Text]
                     , whoisServer      :: !Text
                     , whoisServerInfo  :: !Text
                     } deriving (Typeable, Show, Eq, Ord)
instance MessageC WhoisReplyMsg

-- | All other messages which are not parsed as any of the above message types.
data OtherMsg      = OtherMsg { msgSource  :: !Text
                              , msgCommand :: !Text
                              , msgTarget  :: !Text
                              , msg        :: !Text
                              } deriving (Typeable, Show, Eq, Ord)
instance MessageC OtherMsg


-- | A /PING/ command. A 'PongMsg' is expected as a response to this.
data PingCmd         = PingCmd !Text deriving (Typeable, Show, Eq, Ord)
instance MessageC PingCmd

-- | A /PONG/ command. Sent in response to a 'PingMsg'.
data PongCmd         = PongCmd !Text deriving (Typeable, Show, Eq, Ord)
instance MessageC PongCmd

-- | A /PRIVMSG/ message sent to the channel.
data ChannelMsgReply = ChannelMsgReply !Text deriving (Typeable, Show, Eq, Ord)
instance MessageC ChannelMsgReply

-- | A /PRIVMSG/ message sent to a user.
data PrivMsgReply    = PrivMsgReply !User !Text deriving (Typeable, Show, Eq, Ord)
instance MessageC PrivMsgReply

-- | A /NICK/ command sent to set the bot's nick.
data NickCmd         = NickCmd deriving (Typeable, Show, Eq, Ord)
instance MessageC NickCmd

-- | A /USER/ command sent to identify the bot.
data UserCmd         = UserCmd deriving (Typeable, Show, Eq, Ord)
instance MessageC UserCmd

-- | A /JOIN/ command sent to join the channel.
data JoinCmd         = JoinCmd deriving (Typeable, Show, Eq, Ord)
instance MessageC JoinCmd

-- | A /QUIT/ command sent to quit the server.
data QuitCmd         = QuitCmd deriving (Typeable, Show, Eq, Ord)
instance MessageC QuitCmd

-- | A /NAMES/ command sent to ask for the nicks of the users in the channel.
data NamesCmd        = NamesCmd deriving (Typeable, Show, Eq, Ord)
instance MessageC NamesCmd

-- | A /WHOIS/ command sent to ask for the status of a user nick.
data WhoisCmd        = WhoisCmd !Text deriving (Typeable, Show, Eq, Ord)
instance MessageC WhoisCmd
