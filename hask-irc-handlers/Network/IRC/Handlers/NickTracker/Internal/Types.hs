{-# LANGUAGE TemplateHaskell #-}

module Network.IRC.Handlers.NickTracker.Internal.Types where

import ClassyPrelude
import Data.Data     (Data)
import Data.IxSet    (IxSet, Indexable (..), ixSet, ixFun)
import Data.SafeCopy (base, deriveSafeCopy)

import Network.IRC

newtype CanonicalNick = CanonicalNick { canonicalNickToText :: Text }
                        deriving (Eq, Ord, Show, Data, Typeable)
newtype LastSeenOn    = LastSeenOn UTCTime deriving (Eq, Ord, Show, Data, Typeable)

data NickTrack = NickTrack
  { nick          :: !Nick
  , canonicalNick :: !CanonicalNick
  , lastSeenOn    :: !UTCTime
  , lastMessageOn :: !UTCTime
  , lastMessage   :: !Text
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

instance MessageC NickTrackRequest

instance Show NickTrackRequest where
  show (NickTrackRequest nick _) = "NickTrackRequest[" ++ unpack (nickToText nick) ++ "]"

instance Ord NickTrackRequest where
  (NickTrackRequest nick1 _) `compare` (NickTrackRequest nick2 _) = nick1 `compare` nick2

getCanonicalNick :: MessageChannel Message -> Nick -> IO (Maybe CanonicalNick)
getCanonicalNick messageChannel nick = do
  reply   <- newEmptyMVar
  request <- newMessage $ NickTrackRequest nick reply
  sendMessage messageChannel request
  map (map canonicalNick) $ takeMVar reply
