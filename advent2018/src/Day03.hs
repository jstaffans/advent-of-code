
module Day03 where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.List
import Data.List.Split
import Text.Regex.PCRE
import Debug.Trace

data Patch =
  Patch { x :: Int, y :: Int, width :: Int, height :: Int }
  deriving Show

createPatch [x, y, w, h] = Patch x y w h

toPatch :: String -> Patch
toPatch s =
  let result = s =~ "(\\d+),(\\d+): (\\d+)x(\\d+)" :: [[String]]
  in createPatch $ map (\i -> read i::Int) (drop 1 $ head result)

patchBounds :: [Patch] -> (Int, Int)
patchBounds patches = go patches 0 0
  where
    go [] xb yb = (xb, yb)
    go ((Patch { x = x, y = y, width = w, height = h}):patches) xb yb = go patches (max xb (x + w)) (max yb (y + h))

matrix :: [Patch] -> UArray (Int, Int) Int
matrix patches = runSTUArray $ do
  matrix <- newArray ((0, 0), (patchBounds patches)) 0
  forM_ patches $ \(Patch { x = x, y = y, width = w, height = h}) -> do
    forM_ [y..(y+h-1)] $ \i -> do
      forM [x..(x+w-1)] $ \j -> do
        curr <- readArray matrix (j, i)
        writeArray matrix (j, i) (curr + 1)
  return matrix

part1 :: [String] -> Int
part1 inputLines = length $ filter (\x -> x > 1) $ elems $ matrix (map toPatch inputLines)

main :: IO ()
main = do
  input <- readFile "data/day03.txt"
  let patches = filter (\s -> length s > 0) (splitOn "\n" input)
  print $ part1 patches
