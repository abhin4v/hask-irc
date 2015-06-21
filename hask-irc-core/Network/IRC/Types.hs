{-|
Module      : Network.IRC.Types
Description : Types for the IRC bot and the message handlers.
Copyright   : (c) Abhinav Sarkar, 2014
License     : Apache-2.0
Maintainer  : abhinav@abhinavsarkar.net
Stability   : experimental
Portability : POSIX
-}

module Network.IRC.Types
  (
  -- * IRC Messages
    Nick (..)
  , User (..)
  , MessageC (..)
  , Message (..)
  , MessageW
  , newMessage
  , IdleMsg (..)
  , NickInUseMsg (..)
  , PingMsg (..)
  , PongMsg (..)
  , NamesMsg (..)
  , ChannelMsg (..)
  , PrivMsg (..)
  , ActionMsg (..)
  , JoinMsg (..)
  , QuitMsg (..)
  , PartMsg (..)
  , NickMsg (..)
  , KickMsg (..)
  , ModeMsg (..)
  , WhoisReplyMsg (..)
  , OtherMsg (..)
  -- * IRC Commands
  , PingCmd (..)
  , PongCmd (..)
  , ChannelMsgReply (..)
  , PrivMsgReply (..)
  , NickCmd (..)
  , UserCmd (..)
  , JoinCmd (..)
  , QuitCmd (..)
  , NamesCmd (..)
  , WhoisCmd (..)
  -- * Message Parsing
  , MessageParserId
  , MessagePart (..)
  , MessageParseResult (..)
  , MessageParser (..)
  -- * Command Formatting
  , CommandFormatter
  -- * Bot
  , BotConfig (..)
  , newBotConfig
  , Bot (..)
  , BotStatus (..)
  -- * Message Handlers
  , MsgHandlerName
  , MonadMsgHandler
  , MsgHandler (..)
  , newMsgHandler
  , MsgHandlerMaker (..)
  -- * Message Channel
  , MessageChannel
  , sendMessage
  ) where

import Network.IRC.Message.Types
import Network.IRC.Internal.Types
import Network.IRC.MessageBus
