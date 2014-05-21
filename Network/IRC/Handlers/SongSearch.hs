{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.IRC.Handlers.SongSearch (mkMsgHandler) where

import qualified Data.Configurator as CF
import qualified System.Log.Logger as HSL

import ClassyPrelude
import Control.Concurrent.Lifted (Chan)
import Control.Exception.Lifted  (evaluate)
import Control.Monad.Reader      (ask)
import Data.Aeson                (FromJSON, parseJSON, Value (..), (.:))
import Data.Aeson.Types          (emptyArray)
import Data.Text                 (strip)
import Network.Curl.Aeson        (curlAesonGet, CurlAesonException)
import Network.HTTP.Base         (urlEncode)
import System.Log.Logger.TH      (deriveLoggers)

import Network.IRC.Types

$(deriveLoggers "HSL" [HSL.ERROR])

mkMsgHandler :: BotConfig -> Chan SomeEvent -> MsgHandlerName -> IO (Maybe MsgHandler)
mkMsgHandler _ _ "songsearch" = return . Just $ newMsgHandler { onMessage = songSearch }
mkMsgHandler _ _ _            = return Nothing

data Song = NoSong | Song { url :: Text, name :: Text, artist :: Text }
            deriving (Show, Eq)

instance FromJSON Song where
    parseJSON (Object o)          = Song <$> o .: "Url" <*> o .: "SongName" <*> o .: "ArtistName"
    parseJSON a | a == emptyArray = return NoSong
    parseJSON _                   = mempty

songSearch :: MonadMsgHandler m => Message -> m (Maybe Command)
songSearch ChannelMsg { .. }
  | "!m " `isPrefixOf` msg = do
      BotConfig { .. } <- ask
      liftIO $ do
        let query = strip . drop 3 $ msg
        mApiKey <- CF.lookup config "songsearch.tinysong_apikey"
        map (Just . ChannelMsgReply) $ case mApiKey of
          Nothing     -> do
            errorM "tinysong api key not found in config"
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
  | otherwise              = return Nothing
songSearch _ = return Nothing
