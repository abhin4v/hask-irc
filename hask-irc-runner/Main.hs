{-# LANGUAGE OverlappingInstances #-}

module Main where

import qualified Data.Configurator as CF

import ClassyPrelude hiding    (getArgs)
import Data.Configurator.Types (Configured (..), Value (List), ConfigError (..), KeyError (..))
import System.Environment      (getArgs, getProgName)
import System.Exit             (exitFailure)

import Network.IRC.Client
import Network.IRC.Handlers
import Network.IRC.Types

instance Configured a => Configured [a] where
  convert (List xs) = Just . mapMaybe convert $ xs
  convert _ = Nothing

main :: IO ()
main = do
  -- get args
  args <- getArgs
  prog <- getProgName

  when (length args < 1) $ do
    putStrLn $ "Usage: " ++ pack prog ++ " <config file path>"
    exitFailure

  -- load config and start the bot
  let configFile = headEx args
  loadBotConfig configFile >>= runBot

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
