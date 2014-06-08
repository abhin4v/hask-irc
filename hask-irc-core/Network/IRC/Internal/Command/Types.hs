{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Network.IRC.Internal.Command.Types where

import ClassyPrelude
import Data.Typeable             (cast)

import Network.IRC.Internal.Message.Types

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
