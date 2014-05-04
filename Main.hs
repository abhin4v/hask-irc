{-# LANGUAGE OverloadedStrings, OverlappingInstances #-}

module Main (main) where

import qualified Data.Text as T

import qualified Data.Configurator as C
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

  let configFile = head args
  cfg <- C.load [C.Required configFile]

  server   <- C.require cfg "server"
  port     <- C.require cfg "port"
  channel  <- C.require cfg "channel"
  botNick  <- C.require cfg "nick"
  timeout  <- C.require cfg "timeout"
  handlers <- C.require cfg "handlers"

  if length args < 1
    then putStrLn ("Usage: " ++ prog ++ " <config file path>") >> exitFailure
    else run $ BotConfig server port channel botNick timeout handlers cfg
