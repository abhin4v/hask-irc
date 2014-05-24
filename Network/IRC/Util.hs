module Network.IRC.Util where

import ClassyPrelude
import Control.Concurrent.Lifted (Chan)
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
