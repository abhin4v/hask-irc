{-# LANGUAGE FlexibleContexts #-}

module Network.IRC.Handlers (coreMsgHandlerNames, mkMsgHandler) where

import qualified Network.IRC.Handlers.Auth          as Auth
import qualified Network.IRC.Handlers.Core          as Core
import qualified Network.IRC.Handlers.Greet         as Greet
import qualified Network.IRC.Handlers.MessageLogger as Logger
import qualified Network.IRC.Handlers.NickTracker   as NickTracker
import qualified Network.IRC.Handlers.SongSearch    as SongSearch

import ClassyPrelude
import Control.Concurrent.Lifted  (Chan)

import Network.IRC.Types

coreMsgHandlerNames :: [Text]
coreMsgHandlerNames = ["pingpong", "help"]

mkMsgHandler :: BotConfig -> Chan SomeEvent -> MsgHandlerName -> IO (Maybe MsgHandler)
mkMsgHandler botConfig eventChan name =
  flip (`foldM` Nothing) handlerMakers $ \finalHandler handler ->
    case finalHandler of
      Just _  -> return finalHandler
      Nothing -> handler botConfig eventChan name

  where
    handlerMakers = [
        Auth.mkMsgHandler
      , Core.mkMsgHandler
      , Greet.mkMsgHandler
      , Logger.mkMsgHandler
      , NickTracker.mkMsgHandler
      , SongSearch.mkMsgHandler
      ]
