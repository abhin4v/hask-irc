{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK hide #-}

module Network.IRC.Util where

import qualified Data.Text.Format as TF

import ClassyPrelude
import Control.Monad.Base (MonadBase)
import Data.Convertible   (convert)
import Data.Maybe         (fromJust)
import Data.Text          (strip)
import Data.Time          (diffUTCTime)

oneSec :: Int
oneSec = 1000000

mapKeys :: IsMap map => map -> [ContainerKey map]
mapKeys = map fst . mapToList

mapValues :: IsMap map => map -> [MapValue map]
mapValues = map snd . mapToList

mergeMaps :: forall map map1 map2.
             (IsMap map1, IsMap map2, IsMap map,
              MapValue map ~ (MapValue map1, MapValue map2),
              ContainerKey map1 ~ ContainerKey map,
              ContainerKey map2 ~ ContainerKey map) =>
             map1 -> map2 -> map
mergeMaps map1 map2 =
  flip (`foldl'` mempty) (mapKeys map1) $ \acc key ->
    if key `member` map2
      then insertMap key (fromJust $ lookup key map1, fromJust $ lookup key map2) acc
      else acc

whenJust :: Monad m => Maybe t -> (t -> m ()) -> m ()
whenJust m f = maybe (return ()) f m

clean :: Text -> Text
clean = toLower . strip

io :: MonadIO m => IO a -> m a
io = liftIO

atomicModIORef :: MonadBase IO f => IORef t -> (t -> t) -> f ()
atomicModIORef ref f = void . atomicModifyIORef' ref $ \v -> (f v, v)

-- | Display a time span as one time relative to another.
relativeTime :: UTCTime -> UTCTime -> Text
relativeTime t1 t2 =
  maybe "unknown" (convert . format) $ find (\(s,_,_) -> abs period >= s) ranges
  where
    minute = 60; hour = minute * 60; day = hour * 24;
    week = day * 7; month = day * 30; year = month * 12

    format range =
      (if period > 0 then "in " else "")
      ++ case range of
          (_, str, 0)    -> pack str
          (_, str, base) -> TF.format (fromString str) $ TF.Only (abs $ round (period / base) :: Integer)
      ++ (if period <= 0 then " ago" else "")

    period = t1 `diffUTCTime` t2

    ranges = [ (year*2,    "{} years",     year)
             , (year,      "a year",       0)
             , (month*2,   "{} months",    month)
             , (month,     "a month",      0)
             , (week*2,    "{} weeks",     week)
             , (week,      "a week",       0)
             , (day*2,     "{} days",      day)
             , (day,       "a day",        0)
             , (hour*4,    "{} hours",     hour)
             , (hour*3,    "a few hours",  0)
             , (hour*2,    "{} hours",     hour)
             , (hour,      "an hour",      0)
             , (minute*31, "{} minutes",   minute)
             , (minute*30, "half an hour", 0)
             , (minute*2,  "{} minutes",   minute)
             , (minute,    "a minute",     0)
             , (0,         "{} seconds",   1)
             ]
