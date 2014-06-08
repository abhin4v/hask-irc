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
  eConfig <- try $ CF.load [CF.Required configFile]
  case eConfig of
    Left (ParseError _ _) -> error "Error while loading config"
    Right config             -> do
      eBotConfig <- try $ do
        handlers :: [Text] <- CF.require config "msghandlers"
        let handlerInfo = foldl' (\m h -> insertMap h mempty m) mempty handlers
        botConfig <- newBotConfig                          <$>
                       CF.require config "server"          <*>
                       CF.require config "port"            <*>
                       CF.require config "channel"         <*>
                       (Nick <$> CF.require config "nick") <*>
                       CF.require config "timeout"
        return botConfig { msgHandlerInfo   = handlerInfo
                         , msgHandlerMakers = allMsgHandlerMakers
                         , config           = config
                         }

      case eBotConfig of
        Left (KeyError k) -> error $ "Error while reading key from config: " ++ unpack k
        Right botConf     -> return botConf
