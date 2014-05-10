{-# LANGUAGE OverloadedStrings, OverlappingInstances, NoImplicitPrelude #-}

module Main (main) where

import qualified Data.Configurator as CF

import ClassyPrelude hiding (try, getArgs)
import Control.Exception
import Data.Configurator.Types
import System.Environment
import System.Exit

import Network.IRC.Types (BotConfig(BotConfig))
import Network.IRC.Client

instance Configured a => Configured [a] where
  convert (List xs) = Just . mapMaybe convert $ xs
  convert _ = Nothing

main :: IO ()
main = do
  args <- getArgs
  prog <- getProgName

  when (length args < 1) $ do
    putStrLn $ "Usage: " ++ pack prog ++ " <config file path>"
    exitFailure

  let configFile = headEx args
  loadBotConfig configFile >>= run

loadBotConfig :: String -> IO BotConfig
loadBotConfig configFile = do
  eCfg <- try $ CF.load [Required configFile]
  case eCfg of
    Left (ParseError _ _) -> error "Error while loading config"
    Right cfg             -> do
      eBotConfig <- try $ do
        server   <- CF.require cfg "server"
        port     <- CF.require cfg "port"
        channel  <- CF.require cfg "channel"
        botNick  <- CF.require cfg "nick"
        timeout  <- CF.require cfg "timeout"
        handlers <- CF.require cfg "handlers"
        return $ BotConfig server port channel botNick timeout handlers cfg

      case eBotConfig of
        Left (KeyError k) -> error $ "Error while reading key from config: " ++ unpack k
        Right botConfig   -> return botConfig
