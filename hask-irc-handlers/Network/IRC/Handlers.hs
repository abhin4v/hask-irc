module Network.IRC.Handlers (allMsgHandlerMakers) where

import qualified Network.IRC.Handlers.Auth          as Auth
import qualified Network.IRC.Handlers.Core          as Core
import qualified Network.IRC.Handlers.Greet         as Greet
import qualified Network.IRC.Handlers.MessageLogger as Logger
import qualified Network.IRC.Handlers.NickTracker   as NickTracker
import qualified Network.IRC.Handlers.SongSearch    as SongSearch
import qualified Network.IRC.Handlers.Tell          as Tell

import Network.IRC.Types

allMsgHandlerMakers :: [MsgHandlerMaker]
allMsgHandlerMakers = [
    Auth.mkMsgHandler
  , Core.mkMsgHandler
  , Greet.mkMsgHandler
  , Logger.mkMsgHandler
  , NickTracker.mkMsgHandler
  , SongSearch.mkMsgHandler
  , Tell.mkMsgHandler
  ]
