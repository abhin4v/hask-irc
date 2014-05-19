{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Configurator as CF

import ClassyPrelude hiding (try, getArgs)
import Control.Concurrent.Lifted
import Control.Exception.Lifted
import Data.Configurator.Types
import System.Environment
import System.Exit
import System.Posix.Signals

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
    putStrLn $ "Usage: " ++ pack prog ++ " <config file path>"
    exitFailure

  mainThreadId <- myThreadId
  installHandler sigINT  (Catch $ throwTo mainThreadId UserInterrupt) Nothing
  installHandler sigTERM (Catch $ throwTo mainThreadId UserInterrupt) Nothing

  let configFile = headEx args
  loadBotConfig configFile >>= run

loadBotConfig :: String -> IO BotConfig
loadBotConfig configFile = do
  eCfg <- try $ CF.load [Required configFile]
  case eCfg of
    Left (ParseError _ _) -> error "Error while loading config"
    Right cfg             -> do
      eBotConfig <- try $ BotConfig                    <$>
                          CF.require cfg "server"      <*>
                          CF.require cfg "port"        <*>
                          CF.require cfg "channel"     <*>
                          CF.require cfg "nick"        <*>
                          CF.require cfg "timeout"     <*>
                          CF.require cfg "msghandlers" <*>
                          pure cfg

      case eBotConfig of
        Left (KeyError k) -> error $ "Error while reading key from config: " ++ unpack k
        Right botConf     -> return botConf
