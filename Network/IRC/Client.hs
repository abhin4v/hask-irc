{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.IRC.Client (runBot) where

import qualified System.Log.Logger as HSL

import ClassyPrelude
import Control.Concurrent.Lifted (fork, newChan, threadDelay)
import Control.Exception.Lifted  (AsyncException (UserInterrupt))
import Network                   (PortID (PortNumber), connectTo, withSocketsDo)
import System.IO                 (hSetBuffering, BufferMode(..))
import System.Log.Logger.TH      (deriveLoggers)

import Network.IRC.Bot
import Network.IRC.Handlers
import Network.IRC.Types
import Network.IRC.Util

$(deriveLoggers "HSL" [HSL.DEBUG, HSL.ERROR])

connect :: BotConfig -> IO (Bot, MVar BotStatus, Channel Line, Channel Command, Channel SomeEvent)
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

    loadMsgHandlers eventChan =
      flip (`foldM` mempty) (mapKeys msgHandlerInfo) $ \hMap msgHandlerName -> do
        debugM . unpack $ "Loading msg handler: " ++ msgHandlerName
        mMsgHandler <- mkMsgHandler botConfig eventChan msgHandlerName
        case mMsgHandler of
          Nothing         -> do
            debugM . unpack $ "No msg handler found with name: " ++ msgHandlerName
            return hMap
          Just msgHandler -> return $ insertMap msgHandlerName msgHandler hMap

disconnect :: (Bot, MVar BotStatus, Channel Line, Channel Command, Channel SomeEvent) -> IO ()
disconnect (Bot { .. }, mvBotStatus, (_, readLatch), (commandChan, sendLatch), (eventChan, eventLatch)) = do
  debugM "Disconnecting ..."
  sendCommand commandChan QuitCmd
  awaitLatch sendLatch
  swapMVar mvBotStatus Disconnected
  awaitLatch readLatch
  sendEvent eventChan =<< toEvent QuitEvent
  awaitLatch eventLatch

  unloadMsgHandlers
  handle (\(_ :: SomeException) -> return ()) $ hClose socket
  debugM "Disconnected"
  where
    unloadMsgHandlers = forM_ (mapToList msgHandlers) $ \(msgHandlerName, msgHandler) -> do
      debugM . unpack $ "Unloading msg handler: " ++ msgHandlerName
      stopMsgHandler msgHandler botConfig

runBot :: BotConfig -> IO ()
runBot botConfig' = withSocketsDo $ do
  hSetBuffering stdout LineBuffering
  debugM "Running with config:"
  print botConfig
  status <- runBot_
  case status of
    Disconnected     -> debugM "Restarting .." >> runBot botConfig
    Errored          -> debugM "Restarting .." >> runBot botConfig
    Interrupted      -> return ()
    NickNotAvailable -> return ()
    _                -> error "Unsupported status"
  where
    botConfig = botConfig' {
      msgHandlerInfo =
        foldl' (\m name -> insertMap name mempty m) mempty
          (hashNub $ mapKeys (msgHandlerInfo botConfig') ++ coreMsgHandlerNames)
    }

    handleErrors :: SomeException -> IO BotStatus
    handleErrors e = case fromException e of
        Just UserInterrupt -> debugM "User interrupt"          >> return Interrupted
        _                  -> debugM ("Exception! " ++ show e) >> return Errored

    runBot_ = bracket (connect botConfig) disconnect $
      \(bot, mvBotStatus, (lineChan, readLatch), (commandChan, sendLatch), eventChannel) ->
        handle handleErrors $ do
          sendCommand commandChan NickCmd
          sendCommand commandChan UserCmd

          fork $ sendCommandLoop (commandChan, sendLatch) bot
          fork $ readLineLoop mvBotStatus (lineChan, readLatch) bot oneSec
          fork $ eventProcessLoop eventChannel lineChan commandChan bot
          runIRC bot Connected (messageProcessLoop lineChan commandChan 0)
