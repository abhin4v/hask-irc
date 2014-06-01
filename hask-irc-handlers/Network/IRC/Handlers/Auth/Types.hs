{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.IRC.Handlers.Auth.Types where

import ClassyPrelude
import Data.Data      (Data)
import Data.SafeCopy  (base, deriveSafeCopy)

import Network.IRC.Types hiding (user)

type Token   = Text
newtype Auth = Auth { auth :: Map Nick Token } deriving (Eq, Show, Data, Typeable)

emptyAuth :: Auth
emptyAuth = Auth mempty

$(deriveSafeCopy 0 'base ''Auth)

data AuthEvent = AuthEvent Nick Token (MVar Bool) deriving (Eq, Typeable)

instance Event AuthEvent

instance Show AuthEvent where
  show (AuthEvent nick token _) =
    "AuthEvent[" ++ unpack (nickToText nick) ++ ", " ++ unpack token ++ "]"
