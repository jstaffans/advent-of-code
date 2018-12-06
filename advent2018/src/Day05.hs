module Day05 where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.List
import Data.List.Split
import Data.Char

react :: String -> String
react s = go s []
  where
    go [] out = out
    go (c:[]) out = out ++ [c]
    go (c1:c2:rest) out
      | c1 /= c2 && toLower c1 == toLower c2 = go rest out
      | otherwise                           = go ([c2] ++ rest) (out ++ [c1])

chainReact :: String -> String
chainReact s = go (react s) (length s)
  where
    go s origLength
      | length s == origLength  = s
      | otherwise               = go (react s) (length s)

main :: IO ()
main = do
  input <- readFile "data/day05.txt"
  print $ (length $ chainReact input) - 1
