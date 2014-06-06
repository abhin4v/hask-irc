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
    -- * IRC related
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
  -- * Message handlers
  , MsgHandlerName
  , MonadMsgHandler
  , MsgHandler (..)
  , newMsgHandler
  , MsgHandlerMaker (..)
  ) where

import Network.IRC.Internal.Types
