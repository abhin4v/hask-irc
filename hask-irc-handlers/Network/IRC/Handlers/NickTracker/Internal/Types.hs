{-# LANGUAGE TemplateHaskell #-}

module Network.IRC.Handlers.NickTracker.Internal.Types where

import ClassyPrelude
import Control.Concurrent.Lifted (Chan, writeChan)
import Data.Data                 (Data)
import Data.IxSet                (IxSet, Indexable (..), ixSet, ixFun)
import Data.SafeCopy             (base, deriveSafeCopy)

import Network.IRC.Types

newtype CanonicalNick = CanonicalNick { canonicalNickToText :: Text }
                        deriving (Eq, Ord, Show, Data, Typeable)
newtype LastSeenOn    = LastSeenOn UTCTime deriving (Eq, Ord, Show, Data, Typeable)

data NickTrack = NickTrack {
  nick          :: !Nick,
  canonicalNick :: !CanonicalNick,
  lastSeenOn    :: !UTCTime,
  lastMessageOn :: !UTCTime,
  lastMessage   :: !Text
} deriving (Eq, Ord, Show, Data, Typeable)

instance Indexable NickTrack where
  empty = ixSet [ ixFun $ (: []) . nick
                , ixFun $ (: []) . canonicalNick
                , ixFun $ (: []) . LastSeenOn . lastSeenOn ]

newtype NickTracking = NickTracking { nickTracking :: IxSet NickTrack }
                       deriving (Eq, Ord, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''CanonicalNick)
$(deriveSafeCopy 0 'base ''LastSeenOn)
$(deriveSafeCopy 0 'base ''NickTrack)
$(deriveSafeCopy 0 'base ''NickTracking)

emptyNickTracking :: NickTracking
emptyNickTracking = NickTracking empty

data NickTrackRequest = NickTrackRequest Nick (MVar (Maybe NickTrack)) deriving (Eq, Typeable)

instance EventC NickTrackRequest

instance Show NickTrackRequest where
  show (NickTrackRequest nick _) = "NickTrackRequest[" ++ unpack (nickToText nick) ++ "]"

getCanonicalNick :: Chan Event -> Nick -> IO (Maybe CanonicalNick)
getCanonicalNick eventChan nick = do
  reply   <- newEmptyMVar
  request <- toEvent $ NickTrackRequest nick reply
  writeChan eventChan request
  map (map canonicalNick) $ takeMVar reply
