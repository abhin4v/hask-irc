{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ExistentialQuantification, GADTs #-}

module Network.IRC.Configuration
  ( Name
  , Value (..)
  , Configuration
  , Configurable (..)
  , fromMap
  , lookup
  , require
  , lookupDefault
  ) where

import qualified ClassyPrelude as P

import ClassyPrelude hiding (lookup)
import Data.Maybe (fromJust)

type Name = Text

class Configurable a where
  fromValue :: Value -> Maybe a

  valueToList :: Value -> Maybe [a]
  valueToList (List xs) = mapM fromValue xs
  valueToList _         = Nothing

  toValue :: a -> Value

  listToValue :: [a] -> Value
  listToValue = List . map toValue

valueToNum :: (Num a) => Value -> Maybe a
valueToNum (Number n) = Just . fromInteger $ n
valueToNum _          = Nothing

instance Configurable Integer where
  fromValue   = valueToNum
  toValue = Number

instance Configurable Int where
  fromValue   = valueToNum
  toValue = Number . toInteger

instance Configurable Text where
  fromValue (String s) = Just s
  fromValue _          = Nothing

  toValue = String

instance Configurable Bool where
  fromValue (Boolean b) = Just b
  fromValue _           = Nothing

  toValue = Boolean

instance Configurable a => Configurable [a] where
  fromValue   = valueToList
  toValue = listToValue

data Value = String Text
           | Number Integer
           | Boolean Bool
           | List [Value]
           deriving (Eq, Show)

newtype Configuration = Configuration { configMap :: (Map Name Value) } deriving (Show)

fromMap :: Map Name Value -> Configuration
fromMap = Configuration

lookup :: (Configurable a) => Name -> Configuration -> Maybe a
lookup name Configuration {..} = join . map fromValue $ P.lookup name configMap

require :: (Configurable a) => Name -> Configuration -> a
require n = fromJust . lookup n

lookupDefault :: (Configurable a) => Name -> Configuration -> a -> a
lookupDefault n c v = fromMaybe v $ lookup n c
