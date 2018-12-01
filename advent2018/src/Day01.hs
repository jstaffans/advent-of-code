{-# OPTIONS_GHC -Wall #-}

module Day01 where

import qualified Data.Set as Set
import Data.List.Split

stripPlus :: String -> String
stripPlus ('+':rest) = rest
stripPlus s = s

toInt :: String -> Int
toInt "" = 0
toInt x = read x::Int

search :: Set.Set Int -> Int -> [Int] -> Maybe Int
search _ _ []                  = Nothing
search acc total (change:rest)
  | Set.member newTotal acc = Just newTotal
  | otherwise = search (Set.union acc (Set.singleton newTotal)) newTotal rest
  where
    newTotal = total + change

run :: [String] -> Maybe Int
run changes =
  let intChanges = filter (\x -> x /= 0) (fmap (toInt . stripPlus) changes)
  in search (Set.singleton 0) 0 (cycle intChanges)

main :: IO ()
main = do
  input <- readFile "data/day01.txt"
  print $ run $ splitOn "\n" input
