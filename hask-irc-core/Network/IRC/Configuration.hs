{-|
Module      : Network.IRC.Client
Description : Extensible configuration for the IRC bot.
Copyright   : (c) Abhinav Sarkar, 2014-2015
License     : Apache-2.0
Maintainer  : abhinav@abhinavsarkar.net
Stability   : experimental
Portability : POSIX

Extensible configuration for the IRC bot.
-}

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ExistentialQuantification, GADTs #-}

module Network.IRC.Configuration
  ( Name
  , Value (..)
  , Configurable (..)
  , Configuration
  , fromMap
  , lookup
  , require
  , lookupDefault
  ) where

import qualified ClassyPrelude as P

import ClassyPrelude hiding (lookup)
import Data.Maybe           (fromJust)

-- | Name of a configuration property.
type Name = Text

-- | Typeclass for the types that can be used as values in 'Configuration'.
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

-- | Value of a configuration property.
data Value = String Text    -- ^ A text value.
           | Number Integer -- ^ An integer value.
           | Boolean Bool   -- ^ A boolean value.
           | List [Value]   -- ^ A list of values as a value.
           deriving (Eq, Show)

-- | A configuration data which can be used to look up properties.
newtype Configuration = Configuration { configMap :: Map Name Value } deriving (Show, Eq)

-- | Creates a 'Configuration' from a map of 'Name's to 'Value's.
fromMap :: Map Name Value -> Configuration
fromMap = Configuration

-- | Looks up a property in the 'Configuration' by the given 'Name'.
--   Returns 'Nothing' if the property is not found or is of wrong type.
lookup :: (Configurable a) => Name -> Configuration -> Maybe a
lookup name Configuration {..} = join . map fromValue $ P.lookup name configMap

-- | Looks up a property in the 'Configuration' by the given 'Name'.
--   Fails with an error if the property is not found or is of wrong type.
require :: (Configurable a) => Name -> Configuration -> a
require n = fromJust . lookup n

-- | Looks up a property in the 'Configuration' by the given 'Name'.
--   Returns the given default if the property is not found or is of wrong type.
lookupDefault :: (Configurable a) => Name -> Configuration -> a -> a
lookupDefault n c v = fromMaybe v $ lookup n c
