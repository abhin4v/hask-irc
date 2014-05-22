{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Network.IRC.Handlers.Auth.Types where

import ClassyPrelude
import Data.Data (Data)

import Network.IRC.Types hiding (user)

type Token = Text
newtype Auth = Auth { auth :: Map Nick Token } deriving (Eq, Show, Data, Typeable)

emptyAuth :: Auth
emptyAuth = Auth mempty

data AuthEvent = AuthEvent Nick Token (MVar Bool) deriving (Typeable)

instance Event AuthEvent

instance Show AuthEvent where
  show (AuthEvent user token _) = "AuthEvent[" ++ unpack user ++ ", " ++ unpack token ++ "]"
