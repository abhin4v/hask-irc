{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.IRC.Handlers.Auth.Types where

import ClassyPrelude
import Data.Data     (Data)
import Data.SafeCopy (base, deriveSafeCopy)

import Network.IRC.Types

type Token   = Text
newtype Auth = Auth { auth :: Map Nick Token } deriving (Eq, Show, Data, Typeable)

emptyAuth :: Auth
emptyAuth = Auth mempty

$(deriveSafeCopy 0 'base ''Auth)

data AuthRequest = AuthRequest Nick Token (MVar Bool) deriving (Eq, Typeable)

instance MessageC AuthRequest

instance Show AuthRequest where
  show (AuthRequest nick token _) =
    "AuthRequest[" ++ unpack (nickToText nick) ++ ", " ++ unpack token ++ "]"

instance Ord AuthRequest where
  (AuthRequest nick1 _ _) `compare` (AuthRequest nick2 _ _) = nick1 `compare` nick2
