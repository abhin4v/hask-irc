{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.IRC.Util where

import qualified Data.Text.Format as TF
import qualified Data.Text.Format.Params as TF

import ClassyPrelude
import Control.Concurrent.Lifted (Chan)

oneSec :: Int
oneSec = 1000000

debug :: Text -> IO ()
debug msg = do
  time <- getCurrentTime
  TF.print "[{}] {}\n" $ TF.buildParams (formatTime defaultTimeLocale "%F %T" time, msg)

type Latch = MVar ()

latchIt :: Latch -> IO ()
latchIt latch = putMVar latch ()

awaitLatch :: Latch -> IO ()
awaitLatch latch = void $ takeMVar latch

type Channel a = (Chan a, Latch)
