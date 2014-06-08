{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.IRC.Internal.Message.Types where

import ClassyPrelude
import Data.Data                 (Data)
import Data.SafeCopy             (base, deriveSafeCopy)
import Data.Typeable             (cast)

-- ** IRC Message

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
