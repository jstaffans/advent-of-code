{-# LANGUAGE Haskell2010, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Day03 where

import Prelude hiding (id)
import Control.Monad
import Data.Array.ST
import Data.Array.Unboxed
import Data.List
import Data.List.Split
import qualified Data.Set as Set
import Text.Regex.PCRE

data Patch =
  Patch { id :: Int, x :: Int, y :: Int, w :: Int, h :: Int }
  deriving Show

type Matrix = UArray (Int, Int) Int

createPatch :: [Int] -> Patch
createPatch [id, x, y, w, h] = Patch id x y w h

toPatch :: String -> Patch
toPatch s =
  let result = s =~ "#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)" :: [[String]]
  in createPatch $ map (\i -> read i::Int) (drop 1 $ head result)

patchBounds :: [Patch] -> (Int, Int)
patchBounds patches = go patches 0 0
  where
    go [] xb yb = (xb, yb)
    go ((Patch {..}):ps) xb yb = go ps (max xb (x + w)) (max yb (y + h))

-- claimCell sets a cell value to the patch id when a cell is as of now unclaimed.
-- If the cell is claimed, set the cell value to the _negative_ patch id.
claimCell :: Patch -> Int -> Int
claimCell (Patch { id = id }) 0 = id
claimCell (Patch { id = id }) currentValue = (-id)

toMatrix :: [Patch] -> Matrix
toMatrix patches = runSTUArray $ do
  matrix <- newArray ((0, 0), (patchBounds patches)) 0
  forM_ patches $ \p@Patch {..} -> do
    forM_ [y..(y+h-1)] $ \i -> do
      forM [x..(x+w-1)] $ \j -> do
        curr <- readArray matrix (j, i)
        writeArray matrix (j, i) (claimCell p curr)
  return matrix

part1 :: Matrix -> Int
part1 matrix = length $ filter (\id -> id < 0) $ elems matrix

part2 :: [Patch] -> Matrix -> Set.Set (Int, Int)
part2 patches matrix =
  let patchSizes = map (\Patch{..} -> (id, w * h)) patches
      cells = filter (\id -> id > 0) $ elems matrix
      groupedClaims = map (\ids -> (head ids, length ids)) $ group $ sort cells
  in Set.fromList patchSizes `Set.intersection` Set.fromList groupedClaims

main :: IO ()
main = do
  input <- readFile "data/day03.txt"
  let inputLines = filter (\s -> length s > 0) (splitOn "\n" input)
      patches = map toPatch inputLines
      matrix = toMatrix patches
  print $ part1 matrix
  print $ part2 patches matrix
