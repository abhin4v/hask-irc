{-# LANGUAGE TemplateHaskell #-}

module Network.IRC.Handlers.NickTracker.Types where

import ClassyPrelude
import Data.Data     (Data)
import Data.IxSet    (IxSet, Indexable (..), ixSet, ixFun)
import Data.SafeCopy (base, deriveSafeCopy)

import Network.IRC.Types

newtype CanonicalNick = CanonicalNick Text deriving (Eq, Ord, Show, Data, Typeable)
newtype LastSeenOn    = LastSeenOn UTCTime deriving (Eq, Ord, Show, Data, Typeable)

data NickTrack = NickTrack {
  nick          :: !Nick,
  canonicalNick :: !CanonicalNick,
  lastSeenOn    :: !LastSeenOn,
  lastMessageOn :: !UTCTime,
  lastMessage   :: !Text
} deriving (Eq, Ord, Show, Data, Typeable)

instance Indexable NickTrack where
  empty = ixSet [ ixFun $ (: []) . nick
                , ixFun $ (: []) . canonicalNick
                , ixFun $ (: []) . lastSeenOn ]

newtype NickTracking = NickTracking { nickTracking :: IxSet NickTrack }
                       deriving (Eq, Ord, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''CanonicalNick)
$(deriveSafeCopy 0 'base ''LastSeenOn)
$(deriveSafeCopy 0 'base ''NickTrack)
$(deriveSafeCopy 0 'base ''NickTracking)

emptyNickTracking :: NickTracking
emptyNickTracking = NickTracking empty
