{-# LANGUAGE OverlappingInstances #-}

module Main (main) where

import qualified Data.Configurator as CF

import ClassyPrelude hiding      (getArgs)
import Control.Concurrent.Lifted (myThreadId)
import Control.Exception.Lifted  (throwTo, AsyncException (UserInterrupt))
import Data.Configurator.Types   (Configured (..), Value (List), ConfigError (..), KeyError (..))
import System.Environment        (getArgs, getProgName)
import System.Exit               (exitFailure)
import System.Log.Formatter      (tfLogFormatter)
import System.Log.Handler        (setFormatter)
import System.Log.Handler.Simple (streamHandler)
import System.Log.Logger         (Priority (..), updateGlobalLogger, rootLoggerName,
                                  setHandlers, setLevel)
import System.Posix.Signals      (installHandler, sigINT, sigTERM, Handler (Catch))

import Network.IRC.Types
import Network.IRC.Client

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

  -- setup signal handling
  mainThreadId <- myThreadId
  installHandler sigINT  (Catch $ throwTo mainThreadId UserInterrupt) Nothing
  installHandler sigTERM (Catch $ throwTo mainThreadId UserInterrupt) Nothing

  -- setup logging
  stderrHandler <- streamHandler stderr DEBUG >>= \lh -> return $
                     setFormatter lh $ tfLogFormatter "%F %T" "[$utcTime] $loggername $prio $msg"
  updateGlobalLogger rootLoggerName (setHandlers [stderrHandler] . setLevel DEBUG)

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
        BotConfig                  <$>
          CF.require cfg "server"  <*>
          CF.require cfg "port"    <*>
          CF.require cfg "channel" <*>
          CF.require cfg "nick"    <*>
          CF.require cfg "timeout" <*>
          pure handlerInfo         <*>
          pure cfg

      case eBotConfig of
        Left (KeyError k) -> error $ "Error while reading key from config: " ++ unpack k
        Right botConf     -> return botConf
