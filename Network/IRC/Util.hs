{-# LANGUAGE FlexibleContexts #-}

module Network.IRC.Util where

import ClassyPrelude
import Control.Arrow             (Arrow)
import Control.Concurrent.Lifted (Chan)
import Control.Monad.Base        (MonadBase)
import Data.Text                 (strip)

oneSec :: Int
oneSec = 1000000

type Latch = MVar ()

latchIt :: Latch -> IO ()
latchIt latch = putMVar latch ()

awaitLatch :: Latch -> IO ()
awaitLatch latch = void $ takeMVar latch

type Channel a = (Chan a, Latch)

mapKeys :: IsMap map => map -> [ContainerKey map]
mapKeys   = map fst . mapToList

mapValues :: IsMap map => map -> [MapValue map]
mapValues = map snd . mapToList

whenJust :: Monad m => Maybe t -> (t -> m ()) -> m ()
whenJust m f = maybe (return ()) f m

clean :: Text -> Text
clean = toLower . strip

io :: MonadIO m => IO a -> m a
io = liftIO

both :: Arrow cat => cat b d -> cat (b, b) (d, d)
both f = first f . second f

atomicModIORef :: MonadBase IO f => IORef t -> (t -> t) -> f ()
atomicModIORef ref f = void . atomicModifyIORef' ref $ \v -> (f v, v)

