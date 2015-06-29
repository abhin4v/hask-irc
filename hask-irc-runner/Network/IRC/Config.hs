{-# LANGUAGE OverlappingInstances #-}

module Network.IRC.Config (loadBotConfig) where

import qualified Data.Configurator as CF
import qualified Data.Configurator.Types as CFT
import qualified Data.Ratio as R

import ClassyPrelude
import Data.Configurator.Types (Configured (..), ConfigError (..), KeyError (..))
import Prelude                 (read)

import Network.IRC
import Network.IRC.Configuration
import Network.IRC.Handlers

instance Configured a => Configured [a] where
  convert (CFT.List xs) = Just . mapMaybe convert $ xs
  convert _ = Nothing

instance Configurable CFT.Value where
  fromValue (String a)  = Just $ CFT.String a
  fromValue (Number a)  = Just $ CFT.Number (a R.% 1)
  fromValue (Boolean a) = Just $ CFT.Bool a
  fromValue (List a)    = Just $ CFT.List (mapMaybe fromValue a)

  toValue (CFT.String a) = toValue a
  toValue (CFT.Number r) = toValue (R.numerator r `div` R.denominator r)
  toValue (CFT.Bool a)   = toValue a
  toValue (CFT.List vs)  = toValue vs

fromConfiguratorConfig :: CFT.Config -> IO Configuration
fromConfiguratorConfig config =
  fromMap
  . foldl' (\m (k, v) -> insertMap k (toValue v) m) mempty
  . mapToList
  <$> CF.getMap config

loadBotConfig :: String -> IO BotConfig
loadBotConfig configFile = do
  eConfig <- try $ CF.load [CF.Required configFile]
  case eConfig of
    Left (ParseError _ msg) -> error $ "Error while loading config: " ++ msg
    Right config            -> do
      eBotConfig <- try $ do
        handlers :: [Text] <- CF.require config "msghandlers"
        let handlerInfo    = foldl' (\m h -> insertMap h mempty m) mempty handlers
        let handlerMakers  = foldl' (\m maker -> insertMap (msgHandlerName maker) maker m) mempty
                             . filter (\MsgHandlerMaker { .. } -> msgHandlerName `member` handlerInfo)
                             $ allMsgHandlerMakers

        botConfig <- newBotConfig                          <$>
                       CF.require config "server"          <*>
                       CF.require config "port"            <*>
                       CF.require config "channel"         <*>
                       (Nick <$> CF.require config "nick") <*>
                       CF.require config "timeout"         <*>
                       (read <$> CF.require config "loglevel")
        configMap <- fromConfiguratorConfig config
        return botConfig { msgHandlerInfo   = handlerInfo
                         , msgHandlerMakers = handlerMakers
                         , config           = configMap
                         }

      case eBotConfig of
        Left (KeyError k) -> error $ "Error while reading key from config: " ++ unpack k
        Right botConf     -> return botConf
