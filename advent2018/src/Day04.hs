{-# LANGUAGE Haskell2010, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Day04 where

import Prelude hiding (id, Nothing)
import Control.Monad
import Control.Monad.ST
import Data.Function (on)
import Data.List
import Data.List.Split
import Data.Ord
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Time
import Text.Regex.PCRE
import Debug.Trace

type GuardId = Int
type SleepMap = M.Map Int (V.Vector Int)
type Minute = Int

data LogEntry =
  LogEntry { timestamp :: UniversalTime, id :: GuardId, event :: Event }
  | Nothing
  deriving Show

data Event = BeginsShift | FallsAsleep | WakesUp deriving Show

incrange :: V.Vector Int -> Int -> Int -> V.Vector Int
incrange vector from to = runST $ do
  v <- V.thaw vector
  forM_ [from..(to - 1)] $ \i -> do
    curr <- MV.read v i
    MV.write v i (curr + 1)
  V.freeze v

maxIndex ::  Ord a => [a] -> Int
maxIndex = fst . maximumBy (comparing snd) . zip [0..]

parseGuardId :: String -> Int
parseGuardId s = read (s =~ "\\d+" :: String) :: Int

parseTimestamp :: String -> UniversalTime
parseTimestamp s = parseTimeOrError False defaultTimeLocale "%Y-%m-%d %H:%M" s :: UniversalTime

toEvent :: String -> LogEntry
toEvent s =
  let [timestamp, event] = drop 1 $ head $ (s =~ "\\[(.*)\\] (.*)" :: [[String]])
  in go (parseTimestamp timestamp) event
  where
    go t ('G':'u':'a':'r':'d':rest) = LogEntry t (parseGuardId rest) BeginsShift
    go t ('f':'a':'l':'l':'s':_)    = LogEntry t 0 FallsAsleep
    go t _                          = LogEntry t 0 WakesUp

compareEntries :: LogEntry -> LogEntry -> Ordering
compareEntries LogEntry{timestamp = t1} LogEntry{timestamp = t2} = compare t1 t2

-- When new Guard entry is seen, add a key to the accumulator with an an empty
-- minutes vector as the value. In case the Guard has already been seen, there
-- is already a vector, so preserve it (flip const).
beginShift :: SleepMap -> LogEntry -> SleepMap
beginShift sleepMap LogEntry{..} = M.insertWith (flip const) id (V.replicate 60 0) sleepMap

toMinute :: UniversalTime -> Int
toMinute t = read (formatTime defaultTimeLocale "%M" t) :: Int

recordSleepPeriod :: SleepMap -> GuardId -> UniversalTime -> UniversalTime -> SleepMap
recordSleepPeriod sleepMap id from to =
  M.update (\v -> Just (incrange v (toMinute from) (toMinute to))) id sleepMap

recordSleepTimes :: [LogEntry] -> SleepMap
recordSleepTimes events = go events Nothing M.empty
  where
    go [] _ acc = acc

    go (entry@LogEntry{id = id, event = BeginsShift}:rest) _ acc = go rest entry (beginShift acc entry)

    go (entry@LogEntry{event = FallsAsleep}:rest) LogEntry{id = id} acc =
      go rest (entry {id = id}) acc

    go (entry@LogEntry{timestamp = to, event = WakesUp}:rest)
       LogEntry{timestamp = from, id = id, event = FallsAsleep}
       acc =
      go rest (entry {id = id}) (recordSleepPeriod acc id from to)

mostAsleepGuard :: SleepMap -> (GuardId, Int)
mostAsleepGuard sleepMap =
  let sleepList = M.toList sleepMap
      sleepSums = sortBy (flip compare `on` snd) (map (\(k, v) -> (k, V.sum v)) (M.toList sleepMap))
      mostAsleep = head sleepSums
      mostAsleepId = fst mostAsleep
      minuteMostAsleep = maxIndex $ V.toList $ sleepMap M.! mostAsleepId
  in (mostAsleepId, minuteMostAsleep)

mostFrequentMinute :: SleepMap -> (GuardId, Int)
mostFrequentMinute sleepMap =
  let sleepList = M.toList sleepMap
      sleepMaxFrequencies = sortBy (flip compare `on` snd) (map (\(k, v) -> (k, V.maximum v)) (M.toList sleepMap))
      mostFrequentlyOnSameMinute = head sleepMaxFrequencies
      mostFrequentlyAsleepId = fst mostFrequentlyOnSameMinute
      minuteMostFrequentlyAsleep = maxIndex $ V.toList $ sleepMap M.! mostFrequentlyAsleepId
  in (mostFrequentlyAsleepId, minuteMostFrequentlyAsleep)

main :: IO ()
main = do
  input <- readFile "data/day04.txt"
  let inputLines = filter (\s -> length s > 0) (splitOn "\n" input)
      events = sortBy compareEntries (map toEvent inputLines)
      sleepTimes = recordSleepTimes events
      -- (id, minute) = mostAsleepGuard sleepTimes
      (id, minute) = mostFrequentMinute sleepTimes
  print $ id
  print $ minute
