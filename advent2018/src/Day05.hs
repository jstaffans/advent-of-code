{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

module Day05 where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.List
import Data.List.Split
import Data.Char
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

pattern Empty         <- (Seq.viewl -> Seq.EmptyL)
pattern x :< xs       <- (Seq.viewl -> x Seq.:< xs)

-- fast version using Data.Sequence:

react :: Seq.Seq Char -> Seq.Seq Char
react s = go s Seq.empty
  where
    go Empty out     = out
    go (x :< xs) out
      | Seq.length xs == 0 = out Seq.|> x
      | let x1 = Seq.index xs 0
        in x /= x1 && toLower x == toLower x1 = go (Seq.drop 1 xs) out
      | otherwise = go xs (out Seq.|> x)

-- Slow version using strings:

-- react :: String -> String
-- react s = go s []
--   where
--     go [] out = out
--     go (c:[]) out = out ++ [c]
--     go (c1:c2:rest) out
--       | c1 /= c2 && toLower c1 == toLower c2 = go rest out
--       | otherwise                           = go ([c2] ++ rest) (out ++ [c1])


removeUnit :: Seq.Seq Char -> Char -> Seq.Seq Char
removeUnit s c = go s Seq.empty
  where
    go Empty out = out
    go (c1 :< rest) out
      | toLower c == toLower c1 = go rest out
      | otherwise               = go rest (out Seq.|> c1)

chainReact :: Seq.Seq Char -> String
chainReact s = go (react s) (length s)
  where
    go s origLength
      | length s == origLength  = toList s
      | otherwise               = go (react s) (length s)

main :: IO ()
main = do
  input <- readFile "data/day05.txt"
  -- print $ (length $ chainReact input) - 1
  forM_ ['a'..'z'] $ \c -> do
    print $ (length $ chainReact $ removeUnit (Seq.fromList input) c) - 1
