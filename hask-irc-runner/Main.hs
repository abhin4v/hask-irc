module Main where

import ClassyPrelude hiding    (getArgs)
import System.Environment      (getArgs, getProgName)
import System.Exit             (exitFailure)

import Network.IRC.Client
import Network.IRC.Config

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
