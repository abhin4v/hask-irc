{-# LANGUAGE RecordWildCards, OverloadedStrings, ScopedTypeVariables, NoImplicitPrelude #-}

module Network.IRC.Handlers.SongSearch (songSearch) where

import qualified Data.Configurator as CF

import ClassyPrelude hiding (try)
import Control.Exception
import Data.Aeson
import Data.Aeson.Types (emptyArray)
import Data.Text (strip)
import Network.Curl.Aeson
import Network.HTTP.Base

import Network.IRC.Types

data Song = NoSong | Song { url :: Text, name :: Text, artist :: Text }
            deriving (Show, Eq)

instance FromJSON Song where
    parseJSON (Object o)          = Song <$> o .: "Url" <*> o .: "SongName" <*> o .: "ArtistName"
    parseJSON a | a == emptyArray = return NoSong
    parseJSON _                   = mempty

songSearch :: MonadIO m => BotConfig -> Message -> m (Maybe Command)
songSearch BotConfig { .. } ChannelMsg { .. }
  | "!m " `isPrefixOf` msg = liftIO $ do
      let query = strip . drop 3 $ msg
      mApiKey <- CF.lookup config "songsearch.tinysong_apikey"
      map (Just . ChannelMsgReply) $ case mApiKey of
        Nothing     -> -- do log "tinysong api key not found in config"
          return $ "Error while searching for " ++ query
        Just apiKey -> do
          let apiUrl = "http://tinysong.com/b/" ++ urlEncode (unpack query)
                        ++ "?format=json&key=" ++ apiKey

          result <- try $ curlAesonGet apiUrl >>= evaluate
          return $ case result of
            Left (_ :: CurlAesonException) -> "Error while searching for " ++ query
            Right song                     -> case song of
              Song { .. } -> "Listen to " ++ artist ++ " - " ++ name ++ " at " ++ url
              NoSong      -> "No song found for: " ++ query
  | otherwise = return Nothing
songSearch _ _ = return Nothing
