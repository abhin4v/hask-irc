{-# LANGUAGE OverlappingInstances #-}

module Network.IRC.Config (loadBotConfig) where

import qualified Data.Configurator as CF

import ClassyPrelude
import Data.Configurator.Types (Configured (..), Value (List), ConfigError (..), KeyError (..))

import Network.IRC.Handlers
import Network.IRC.Types

instance Configured a => Configured [a] where
  convert (List xs) = Just . mapMaybe convert $ xs
  convert _ = Nothing

loadBotConfig :: String -> IO BotConfig
loadBotConfig configFile = do
  eCfg <- try $ CF.load [CF.Required configFile]
  case eCfg of
    Left (ParseError _ _) -> error "Error while loading config"
    Right cfg             -> do
      eBotConfig <- try $ do
        handlers :: [Text] <- CF.require cfg "msghandlers"
        let handlerInfo = foldl' (\m h -> insertMap h mempty m) mempty handlers
        BotConfig                          <$>
          CF.require cfg "server"          <*>
          CF.require cfg "port"            <*>
          CF.require cfg "channel"         <*>
          (Nick <$> CF.require cfg "nick") <*>
          CF.require cfg "timeout"         <*>
          pure handlerInfo                 <*>
          pure allMsgHandlerMakers         <*>
          pure cfg

      case eBotConfig of
        Left (KeyError k) -> error $ "Error while reading key from config: " ++ unpack k
        Right botConf     -> return botConf
