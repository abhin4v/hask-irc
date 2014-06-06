module Network.IRC.Handlers (allMsgHandlerMakers) where

import Network.IRC.Handlers.Auth
import Network.IRC.Handlers.Greet
import Network.IRC.Handlers.MessageLogger
import Network.IRC.Handlers.NickTracker
import Network.IRC.Handlers.SongSearch
import Network.IRC.Handlers.Tell

import Network.IRC.Types

allMsgHandlerMakers :: [MsgHandlerMaker]
allMsgHandlerMakers =
  [ authMsgHandlerMaker
  , greetMsgHandlerMaker
  , messageLoggerMsgHandlerMaker
  , nickTrackerMsgHandlerMaker
  , songSearchMsgHandlerMaker
  , tellMsgHandlerMaker
  ]
