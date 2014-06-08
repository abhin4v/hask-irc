{-|
Module      : Network.IRC.Client
Description : The IRC bot client used to create and run the bot.
Copyright   : (c) Abhinav Sarkar, 2014
License     : Apache-2.0
Maintainer  : abhinav@abhinavsarkar.net
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE TemplateHaskell #-}

module Network.IRC.Client (runBot) where

import qualified System.Log.Logger as HSL

import ClassyPrelude
import Control.Concurrent.Lifted (fork, newChan, threadDelay, myThreadId, Chan)
import Control.Exception.Lifted  (throwTo, AsyncException (UserInterrupt))
import Network                   (PortID (PortNumber), connectTo, withSocketsDo)
import System.IO                 (hSetBuffering, BufferMode(..))
import System.Log.Formatter      (tfLogFormatter)
import System.Log.Handler        (setFormatter)
import System.Log.Handler.Simple (streamHandler)
import System.Log.Logger         (Priority (..), updateGlobalLogger, rootLoggerName,
                                  setHandlers, setLevel)
import System.Log.Logger.TH      (deriveLoggers)
import System.Posix.Signals      (installHandler, sigINT, sigTERM, Handler (Catch))

import qualified Network.IRC.Handlers.Core as Core

import Network.IRC.Bot
import Network.IRC.Internal.Types
import Network.IRC.Types
import Network.IRC.Util

$(deriveLoggers "HSL" [HSL.DEBUG, HSL.ERROR])

coreMsgHandlerNames :: [MsgHandlerName]
coreMsgHandlerNames = ["pingpong", "help"]

connect :: BotConfig -> IO (Bot, MVar BotStatus, Channel Line, Channel Command, Channel Event)
connect botConfig@BotConfig { .. } = do
  debugM "Connecting ..."
  socket <- connectToWithRetry
  hSetBuffering socket LineBuffering
  debugM "Connected"

  lineChan        <- newChannel
  commandChan     <- newChannel
  eventChan       <- newChannel
  mvBotStatus     <- newMVar Connected
  msgHandlers     <- loadMsgHandlers (fst eventChan)
  msgHandlerInfo' <- foldM (\m (hn, h) -> getHelp h botConfig >>= \hm -> return $ insertMap hn hm m)
                       mempty (mapToList msgHandlers)
  let botConfig'  = botConfig { msgHandlerInfo = msgHandlerInfo'}
  return (Bot botConfig' socket msgHandlers, mvBotStatus, lineChan, commandChan, eventChan)
  where
    connectToWithRetry = connectTo (unpack server) (PortNumber (fromIntegral port))
                           `catch` (\(e :: SomeException) -> do
                                      errorM ("Error while connecting: " ++ show e ++ ". Waiting.")
                                      threadDelay (5 * oneSec)
                                      connectToWithRetry)

    newChannel = (,) <$> newChan <*> newEmptyMVar

    mkMsgHandler :: Chan Event -> MsgHandlerName -> IO (Maybe MsgHandler)
    mkMsgHandler eventChan name =
      flip (`foldM` Nothing) msgHandlerMakers $ \finalHandler handler ->
        case finalHandler of
          Just _  -> return finalHandler
          Nothing -> msgHandlerMaker handler botConfig eventChan name

    loadMsgHandlers eventChan =
      flip (`foldM` mempty) (mapKeys msgHandlerInfo) $ \hMap msgHandlerName -> do
        debugM . unpack $ "Loading msg handler: " ++ msgHandlerName
        mMsgHandler <- mkMsgHandler eventChan msgHandlerName
        case mMsgHandler of
          Nothing         -> do
            debugM . unpack $ "No msg handler found with name: " ++ msgHandlerName
            return hMap
          Just msgHandler -> return $ insertMap msgHandlerName msgHandler hMap

disconnect :: (Bot, MVar BotStatus, Channel Line, Channel Command, Channel Event) -> IO ()
disconnect (Bot { .. }, mvBotStatus, (_, readLatch), (commandChan, sendLatch), (eventChan, eventLatch)) = do
  debugM "Disconnecting ..."
  sendCommand commandChan $ toCommand QuitCmd
  awaitLatch sendLatch
  swapMVar mvBotStatus Disconnected
  awaitLatch readLatch
  sendEvent eventChan =<< toEvent QuitEvent
  awaitLatch eventLatch

  unloadMsgHandlers
  handle (\(_ :: SomeException) -> return ()) $ hClose botSocket
  debugM "Disconnected"
  where
    unloadMsgHandlers = forM_ (mapToList msgHandlers) $ \(msgHandlerName, msgHandler) -> do
      debugM . unpack $ "Unloading msg handler: " ++ msgHandlerName
      stopMsgHandler msgHandler botConfig

runBotIntenal :: BotConfig -> IO ()
runBotIntenal botConfig' = withSocketsDo $ do
  status <- run
  case status of
    Disconnected     -> debugM "Restarting .." >> runBotIntenal botConfig
    Errored          -> debugM "Restarting .." >> runBotIntenal botConfig
    Interrupted      -> return ()
    NickNotAvailable -> return ()
    _                -> error "Unsupported status"
  where
    botConfig = botConfig' {
      msgHandlerInfo =
        foldl' (\m name -> insertMap name mempty m) mempty
          (hashNub $ mapKeys (msgHandlerInfo botConfig') ++ coreMsgHandlerNames),
      msgHandlerMakers = ordNub $ Core.mkMsgHandler : msgHandlerMakers botConfig'
    }

    handleErrors :: SomeException -> IO BotStatus
    handleErrors e = case fromException e of
        Just UserInterrupt -> debugM "User interrupt"          >> return Interrupted
        _                  -> debugM ("Exception! " ++ show e) >> return Errored

    run = bracket (connect botConfig) disconnect $
      \(bot, mvBotStatus, (lineChan, readLatch), (commandChan, sendLatch), eventChannel) ->
        handle handleErrors $ do
          debugM $ "Running with config:\n" ++ show botConfig

          sendCommand commandChan $ toCommand NickCmd
          sendCommand commandChan $ toCommand UserCmd

          fork $ sendCommandLoop (commandChan, sendLatch) bot
          fork $ readLineLoop mvBotStatus (lineChan, readLatch) bot oneSec
          fork $ eventProcessLoop eventChannel lineChan commandChan bot
          runIRC bot Connected (messageProcessLoop lineChan commandChan)

-- | Creates and runs an IRC bot for given the config. This IO action runs forever.
runBot :: BotConfig -- ^ The bot config used to create the bot.
       -> IO ()
runBot botConfig = do
  -- setup signal handling
  mainThreadId <- myThreadId
  installHandler sigINT  (Catch $ throwTo mainThreadId UserInterrupt) Nothing
  installHandler sigTERM (Catch $ throwTo mainThreadId UserInterrupt) Nothing

  -- setup logging
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  stderrHandler <- streamHandler stderr DEBUG >>= \lh -> return $
                     setFormatter lh $ tfLogFormatter "%F %T" "[$utcTime] $loggername $prio $msg"
  updateGlobalLogger rootLoggerName (setHandlers [stderrHandler] . setLevel DEBUG)

  -- run
  runBotIntenal botConfig
