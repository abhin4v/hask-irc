{-# LANGUAGE RecordWildCards, OverloadedStrings, ScopedTypeVariables #-}

module Network.IRC.Handlers.SongSearch (songSearch) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Data.Aeson
import Data.Aeson.Types (emptyArray)
import Data.Configurator
import Data.Text
import Data.Text.IO
import Network.Curl.Aeson
import Network.HTTP.Base
import Prelude hiding (putStrLn, drop, lookup)

import Network.IRC.Types

(+++) = append

data Song = NoSong | Song { url :: Text, name :: Text, artist :: Text }
            deriving (Show, Eq)

instance FromJSON Song where
    parseJSON (Object o)          = Song <$> o .: "Url" <*> o .: "SongName" <*> o .: "ArtistName"
    parseJSON a | a == emptyArray = return NoSong
    parseJSON _                   = mzero

songSearch bot@BotConfig { .. } ChannelMsg { .. }
  | "!m " `isPrefixOf` msg = liftIO $ do
      let query = strip . drop 3 $ msg
      mApiKey <- lookup config "songsearch.tinysong_apikey"
      fmap (Just . ChannelMsgReply) $ case mApiKey of
        Nothing     -> -- do log "tinysong api key not found in config"
          return $ "Error while searching for " +++ query
        Just apiKey -> do
          let apiUrl = "http://tinysong.com/b/" ++ urlEncode (unpack query)
                        ++ "?format=json&key=" ++ apiKey

          result <- try $ curlAesonGet apiUrl >>= evaluate
          return $ case result of
            Left (_ :: CurlAesonException) -> "Error while searching for " +++ query
            Right song                     -> case song of
              Song { .. } -> "Listen to " +++ artist +++ " - " +++ name +++ " at " +++ url
              NoSong      -> "No song found for: " +++ query
  | otherwise = return Nothing
songSearch _ _ = return Nothing
