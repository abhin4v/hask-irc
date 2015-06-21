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
  , receiveMessageEither
  , closeMessageChannel
  , awaitMessageChannel
  , isClosedMessageChannel ) where

import ClassyPrelude

newtype Latch = Latch (MVar ())

newLatch :: IO Latch
newLatch = liftM Latch newEmptyMVar

doLatch :: Latch -> IO ()
doLatch (Latch mv) = putMVar mv ()

awaitLatch :: Latch -> IO ()
awaitLatch (Latch mv) = void $ takeMVar mv

latched :: Latch -> IO Bool
latched (Latch mv) = map isJust . tryReadMVar $ mv

newtype MessageBus a = MessageBus (TChan a)

newMessageBus :: IO (MessageBus a)
newMessageBus = MessageBus <$> newBroadcastTChanIO

-- | A channel through which messages are sent and received.
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

-- | Sends a message through a message channel.
sendMessage :: MessageChannel a -- ^ The channel
            -> a                -- ^ The message to send
            -> IO ()
sendMessage chan = atomically . sendMessageSTM chan

receiveMessage :: MessageChannel a -> IO a
receiveMessage = atomically . receiveMessageSTM

closeMessageChannel :: MessageChannel a -> IO ()
closeMessageChannel (MessageChannel latch _ _) = doLatch latch

awaitMessageChannel :: MessageChannel a -> IO ()
awaitMessageChannel (MessageChannel latch _ _) = awaitLatch latch

isClosedMessageChannel :: MessageChannel a -> IO Bool
isClosedMessageChannel (MessageChannel latch _ _) = latched latch

receiveMessageEither :: MessageChannel a -> MessageChannel b -> IO (Either a b)
receiveMessageEither chan1 chan2 = atomically $
  map Left (receiveMessageSTM chan1) `orElseSTM` map Right (receiveMessageSTM chan2)
