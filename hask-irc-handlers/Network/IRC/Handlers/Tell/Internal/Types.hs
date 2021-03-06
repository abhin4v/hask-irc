{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.IRC.Handlers.Tell.Internal.Types where

import ClassyPrelude
import Data.Data     (Data)
import Data.IxSet    (IxSet, Indexable (..), ixSet, ixFun)
import Data.SafeCopy (base, deriveSafeCopy)

import Network.IRC
import Network.IRC.Handlers.NickTracker.Types

newtype TellId  = TellId Int deriving (Eq, Ord, Show, Data, Typeable, Num)
data TellStatus = NewTell | DeliveredTell deriving (Eq, Ord, Show, Data, Typeable)

data Tell = Tell
  { tellId          :: !TellId
  , tellFromNick    :: !Nick
  , tellToNick      :: !CanonicalNick
  , tellTopic       :: !(Maybe Text)
  , tellStatus      :: !TellStatus
  , tellCreatedOn   :: !UTCTime
  , tellDeliveredOn :: !(Maybe UTCTime)
  , tellContent     :: !Text
  } deriving (Eq, Ord, Show, Data, Typeable)

instance Indexable Tell where
  empty = ixSet [ ixFun $ (: []) . tellId
                , ixFun $ (: []) . tellToNick
                , ixFun $ (: []) . tellStatus ]

data Tells = Tells { nextTellId :: TellId, tells :: IxSet Tell }
             deriving (Eq, Ord, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''TellId)
$(deriveSafeCopy 0 'base ''TellStatus)
$(deriveSafeCopy 0 'base ''Tell)
$(deriveSafeCopy 0 'base ''Tells)

emptyTells :: Tells
emptyTells = Tells (TellId 1) empty

data TellRequest = TellRequest User Text deriving (Eq, Typeable, Ord)

instance MessageC TellRequest

instance Show TellRequest where
  show (TellRequest user tell) =
    "TellRequest[" ++ unpack (nickToText (userNick user) ++ ": " ++ tell) ++ "]"

sendTell :: MessageChannel Message -> User -> Text -> IO ()
sendTell messageChannel user tell =
  newMessage (TellRequest user tell) >>= sendMessage messageChannel
