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
  , Message
  , FullMessage (..)
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
  , OtherMsg (..)
  -- * IRC Commands
  , CommandC (..)
  , Command
  , PingCmd (..)
  , PongCmd (..)
  , ChannelMsgReply (..)
  , PrivMsgReply (..)
  , NickCmd (..)
  , UserCmd (..)
  , JoinCmd (..)
  , QuitCmd (..)
  , NamesCmd (..)
  -- * Message Parsing
  , MessageParserId
  , MessagePart (..)
  , MessageParseResult (..)
  , MessageParser (..)
  -- * Events
  , EventC (..)
  , Event
  , EventResponse (..)
  , QuitEvent(..)
  -- * Bot
  , BotConfig (..)
  , Bot (..)
  , BotStatus (..)
  -- * Message handlers
  , MsgHandlerName
  , MonadMsgHandler
  , MsgHandler (..)
  , newMsgHandler
  , MsgHandlerMaker (..)
  ) where

import Network.IRC.Internal.Types
