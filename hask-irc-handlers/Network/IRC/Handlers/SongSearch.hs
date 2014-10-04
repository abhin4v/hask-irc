{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.IRC.Handlers.SongSearch (songSearchMsgHandlerMaker) where

import qualified Data.Configurator as CF
import qualified System.Log.Logger as HSL

import ClassyPrelude
import Control.Exception.Lifted (evaluate)
import Data.Aeson               (FromJSON, parseJSON, Value (..), (.:))
import Data.Aeson.Types         (emptyArray)
import Data.Text                (strip)
import Network.Curl.Aeson       (curlAesonGet, CurlAesonException)
import Network.HTTP.Base        (urlEncode)
import System.Log.Logger.TH     (deriveLoggers)

import Network.IRC

$(deriveLoggers "HSL" [HSL.ERROR])

songSearchMsgHandlerMaker :: MsgHandlerMaker
songSearchMsgHandlerMaker = MsgHandlerMaker "songsearch" go
  where
    helpMsg = "Search for song. !m <song> or !m <artist> - <song>"

    go _ _ =
      return $ newMsgHandler { onMessage   = songSearch
                             , handlerHelp = return $ singletonMap "!m" helpMsg }

data Song = NoSong | Song { url :: Text, name :: Text, artist :: Text }
            deriving (Show, Eq)

instance FromJSON Song where
    parseJSON (Object o)          = Song <$> o .: "Url" <*> o .: "SongName" <*> o .: "ArtistName"
    parseJSON a | a == emptyArray = return NoSong
    parseJSON _                   = mempty

songSearch :: MonadMsgHandler m => Message -> m [Message]
songSearch Message { .. }
  | Just (ChannelMsg _ msg) <- fromMessage message
  , "!m " `isPrefixOf` msg = do
      BotConfig { .. } <- ask
      liftIO $ do
        let query = strip . drop 3 $ msg
        mApiKey   <- CF.lookup config "songsearch.tinysong_apikey"
        reply     <- map ChannelMsgReply $ case mApiKey of
          Nothing     -> do
            errorM "tinysong api key not found in config"
            return $ "Error while searching for " ++ query
          Just apiKey -> do
            let apiUrl = "http://tinysong.com/b/" ++ urlEncode (unpack query)
                          ++ "?format=json&key=" ++ apiKey

            result <- try $ curlAesonGet apiUrl >>= evaluate
            case result of
              Left (e :: CurlAesonException) -> do
                errorM . unpack $ "Error while searching for " ++ query ++ ": " ++ pack (show e)
                return $ "Error while searching for " ++ query
              Right song                     -> return $ case song of
                Song { .. } -> "Listen to " ++ artist ++ " - " ++ name ++ " at " ++ url
                NoSong      -> "No song found for: " ++ query
        map singleton . newMessage $ reply
  | otherwise              = return []
