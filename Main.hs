{-# LANGUAGE OverloadedStrings, OverlappingInstances #-}

module Main (main) where

import qualified Data.Text as T

import Control.Exception
import Control.Monad
import Data.Configurator
import Data.Configurator.Types
import Data.Maybe
import System.Environment
import System.Exit

import Network.IRC.Types
import Network.IRC.Client

instance Configured a => Configured [a] where
  convert (List xs) = Just . mapMaybe convert $ xs
  convert _ = Nothing

main :: IO ()
main = do
  args <- getArgs
  prog <- getProgName

  when (length args < 1) $ do
    putStrLn ("Usage: " ++ prog ++ " <config file path>")
    exitFailure

  let configFile = head args
  loadBotConfig configFile >>= run

loadBotConfig :: FilePath -> IO BotConfig
loadBotConfig configFile = do
  eCfg <- try $ load [Required configFile]
  case eCfg of
    Left (ParseError _ _) -> error "Error while loading config"
    Right cfg             -> do
      eBotConfig <- try $ do
        server   <- require cfg "server"
        port     <- require cfg "port"
        channel  <- require cfg "channel"
        botNick  <- require cfg "nick"
        timeout  <- require cfg "timeout"
        handlers <- require cfg "handlers"
        return $ BotConfig server port channel botNick timeout handlers cfg

      case eBotConfig of
        Left (KeyError k) -> error $ "Error while reading key from config: " ++ T.unpack k
        Right botConfig   -> return botConfig
