{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Network.IRC.Internal.Event.Types where

import ClassyPrelude
import Data.Typeable             (cast)

import Network.IRC.Internal.Message.Types
import Network.IRC.Internal.Command.Types

-- ** Events

-- | Events are used for communication between message handlers. To send events, write them to the
-- event channel provided to the 'MsgHandler' when it is created. To receive events, provide
-- an 'onEvent' function as a part of the message handler.
class (Typeable e, Show e, Eq e) => EventC e where
  -- | Creates an event.
  toEvent :: e -> IO Event
  toEvent e = Event <$> pure e <*> getCurrentTime

  -- | Extracts a received event.
  fromEvent :: Event -> Maybe (e, UTCTime)
  fromEvent (Event e time) = do
    ev <- cast e
    return (ev, time)

-- | A wrapper over all types of 'Event's to allow sending them over channel of same type.
data Event = forall e. (EventC e, Typeable e) => Event e UTCTime deriving (Typeable)

instance Show Event where
  show (Event e time) = formatTime defaultTimeLocale "[%F %T] " time ++ show e

instance Eq Event where
  Event e1 t1 == Event e2 t2 =
    case cast e2 of
      Just e2' -> e1 == e2' && t1 == t2
      Nothing  -> False

-- | Response to an event received by a message handler.
data EventResponse =
  -- | No response
    RespNothing
  -- | Events as the response. They will be sent to all message handlers like usual events.
  | RespEvent [Event]
  -- | Messages as the response. They will be sent to all message handlers like usual messages.
  | RespMessage [FullMessage]
  -- | Commands as the response. They will be sent to the server like usual commands.
  | RespCommand [Command]
  deriving (Show, Eq)

-- | An event signifying the bot quitting the server.
data QuitEvent = QuitEvent deriving (Show, Eq, Ord, Typeable)
instance EventC QuitEvent
