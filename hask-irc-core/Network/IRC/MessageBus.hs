{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Network.IRC.MessageBus
  ( MessageBus
  , newMessageBus
  , MessageChannel
  , newMessageChannel
  , sendMessage
  , receiveMessage
  , closeMessageChannel
  , awaitMessageChannel ) where

import ClassyPrelude

newtype Latch = Latch (MVar ())

newLatch :: IO Latch
newLatch = liftM Latch newEmptyMVar

doLatch :: Latch -> IO ()
doLatch (Latch mv) = putMVar mv ()

awaitLatch :: Latch -> IO ()
awaitLatch (Latch mv) = void $ takeMVar mv

newtype MessageBus a = MessageBus (TChan a)

newMessageBus :: IO (MessageBus a)
newMessageBus = MessageBus <$> newBroadcastTChanIO

data MessageChannel a = MessageChannel Latch (TChan a) (TChan a)

newMessageChannel ::MessageBus a -> IO (MessageChannel a)
newMessageChannel (MessageBus wChan) = do
  latch <- newLatch
  rChan <- atomically $ dupTChan wChan
  return $ MessageChannel latch rChan wChan

sendMessageSTM :: MessageChannel a -> a -> STM ()
sendMessageSTM (MessageChannel _ _ wChan) = writeTChan wChan

receiveMessageSTM :: MessageChannel a -> STM a
receiveMessageSTM (MessageChannel _ rChan _) = readTChan rChan

sendMessage :: MessageChannel a -> a -> IO ()
sendMessage chan = atomically . sendMessageSTM chan

receiveMessage :: MessageChannel a -> IO a
receiveMessage = atomically . receiveMessageSTM

closeMessageChannel :: MessageChannel a -> IO ()
closeMessageChannel (MessageChannel latch _ _) = doLatch latch

awaitMessageChannel :: MessageChannel a -> IO ()
awaitMessageChannel (MessageChannel latch _ _) = awaitLatch latch