{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.IRC.Client (runBot) where

import ClassyPrelude
import Control.Concurrent.Lifted (fork, newChan, threadDelay)
import Control.Exception.Lifted  (AsyncException (UserInterrupt))
import Network                   (PortID (PortNumber), connectTo, withSocketsDo)
import System.IO                 (hSetBuffering, BufferMode(..))

import Network.IRC.Bot
import Network.IRC.Handlers
import Network.IRC.Types
import Network.IRC.Util

connect :: BotConfig -> IO (Bot, MVar BotStatus, Channel Line, Channel Command, Channel SomeEvent)
connect botConfig@BotConfig { .. } = do
  debug "Connecting ..."
  socket <- connectToWithRetry
  hSetBuffering socket LineBuffering
  debug "Connected"

  lineChan    <- newChannel
  commandChan <- newChannel
  eventChan   <- newChannel
  mvBotStatus <- newMVar Connected
  msgHandlers <- loadMsgHandlers (fst eventChan)

  return (Bot botConfig socket msgHandlers, mvBotStatus, lineChan, commandChan, eventChan)
  where
    connectToWithRetry = connectTo (unpack server) (PortNumber (fromIntegral port))
                           `catch` (\(e :: SomeException) -> do
                                      debug ("Error while connecting: " ++ pack (show e) ++ ". Waiting.")
                                      threadDelay (5 * oneSec)
                                      connectToWithRetry)

    newChannel = (,) <$> newChan <*> newEmptyMVar

    loadMsgHandlers eventChan = flip (`foldM` mempty) msgHandlerNames $ \hMap msgHandlerName -> do
      debug $ "Loading msg handler: " ++ msgHandlerName
      mMsgHandler <- mkMsgHandler botConfig eventChan msgHandlerName
      case mMsgHandler of
        Nothing         -> debug ("No msg handler found with name: " ++ msgHandlerName) >> return hMap
        Just msgHandler -> return $ insertMap msgHandlerName msgHandler hMap

disconnect :: (Bot, MVar BotStatus, Channel Line, Channel Command, Channel SomeEvent) -> IO ()
disconnect (Bot { .. }, mvBotStatus, (_, readLatch), (commandChan, sendLatch), (eventChan, eventLatch)) = do
  debug "Disconnecting ..."
  sendCommand commandChan QuitCmd
  awaitLatch sendLatch
  swapMVar mvBotStatus Disconnected
  awaitLatch readLatch
  sendEvent eventChan =<< toEvent QuitEvent
  awaitLatch eventLatch

  unloadMsgHandlers
  hClose socket
  debug "Disconnected"
  where
    unloadMsgHandlers = forM_ (mapToList msgHandlers) $ \(msgHandlerName, msgHandler) -> do
      debug $ "Unloading msg handler: " ++ msgHandlerName
      stopMsgHandler msgHandler botConfig

runBot :: BotConfig -> IO ()
runBot botConfig' = withSocketsDo $ do
  hSetBuffering stdout LineBuffering
  debug "Running with config:"
  print botConfig
  status <- runBot_
  case status of
    Disconnected     -> debug "Restarting .." >> runBot botConfig
    Errored          -> debug "Restarting .." >> runBot botConfig
    Interrupted      -> return ()
    NickNotAvailable -> return ()
    _                -> error "Unsupported status"
  where
    botConfig = botConfig' { msgHandlerNames = hashNub $ msgHandlerNames botConfig' ++ coreMsgHandlerNames }

    handleErrors :: SomeException -> IO BotStatus
    handleErrors e = case fromException e of
        Just UserInterrupt -> debug "User interrupt"                 >> return Interrupted
        _                  -> debug ("Exception! " ++ pack (show e)) >> return Errored

    runBot_ = bracket (connect botConfig) disconnect $
      \(bot, mvBotStatus, (lineChan, readLatch), (commandChan, sendLatch), eventChannel) ->
        handle handleErrors $ do
          sendCommand commandChan NickCmd
          sendCommand commandChan UserCmd

          fork $ sendCommandLoop (commandChan, sendLatch) bot
          fork $ readLineLoop mvBotStatus (lineChan, readLatch) bot oneSec
          fork $ eventProcessLoop eventChannel lineChan commandChan bot
          runIRC bot Connected (messageProcessLoop lineChan commandChan 0)
