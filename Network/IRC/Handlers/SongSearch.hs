{-# LANGUAGE RecordWildCards, OverloadedStrings, ScopedTypeVariables, BangPatterns #-}

module Network.IRC.Handlers.SongSearch (songSearch) where

import qualified Data.Configurator as C

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (emptyArray)
import Data.Text
import Data.Text.IO
import Network.Curl.Aeson
import Network.HTTP.Base
import Prelude hiding (putStrLn, drop)

import Network.IRC.Types

(+++) = append

data Song = NoSong | Song { url :: Text, name :: Text, artist :: Text }
            deriving (Show)

instance FromJSON Song where
    parseJSON (Object o)          = Song <$> o .: "Url" <*> o .: "SongName" <*> o .: "ArtistName"
    parseJSON a | a == emptyArray = return NoSong
    parseJSON _                   = mzero

songSearch bot@BotConfig { .. } ChannelMsg { .. }
  | "!m " `isPrefixOf` msg = do
      let query = strip . drop 3 $ msg
      apiKey <- C.require config "songsearch.tinysong_apikey"
      let apiUrl = "http://tinysong.com/b/" ++ urlEncode (unpack query)
                    ++ "?format=json&key=" ++ apiKey

      result <- try $ curlAesonGet apiUrl >>= evaluate

      return . Just . ChannelMsgReply $ case result of
        Left (_ :: CurlAesonException) -> "Error while searching for " +++ query
        Right song                     -> case song of
          Song { .. } -> "Listen to " +++ artist +++ " - " +++ name +++ " at " +++ url
          NoSong      -> "No song found for: " +++ query
  | otherwise = return Nothing
songSearch _ _ = return Nothing
