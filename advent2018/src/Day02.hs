
module Day02 where

import Data.Maybe (catMaybes)
import Data.Ord
import Data.List
import Data.List.Split

sortByFreq :: String -> String
sortByFreq s = concat $ sortBy (comparing length) $ group $ sort s

countSame :: [Int] -> String -> [Int]
countSame xs (_:[]) = xs
countSame (x:xs) (c1:c2:s)
  | c1 == c2 = countSame ([(x + 1)] ++ xs) ([c2] ++ s)
  | otherwise = countSame ([1] ++ [x] ++ xs) ([c2] ++ s)

appears :: Int -> [[Int]] -> Int
times `appears` sameCounts = length $ filter (\xs -> times `elem` xs) sameCounts

part1 :: [String] -> Int
part1 ids =
  let sameCounts = map (countSame [1] . sortByFreq) ids
      twoAppears = appears 2
      threeAppears = appears 3
  in (twoAppears sameCounts) * (threeAppears sameCounts)

differsInOnePos :: String -> String -> Int -> Bool
differsInOnePos [] [] acc = acc == 1
differsInOnePos (c1:s1) (c2:s2) acc
  | acc > 1 = False
  | c1 == c2 = differsInOnePos s1 s2 acc
  | otherwise = differsInOnePos s1 s2 (acc + 1)

onePosMatch :: String -> String -> Maybe String
onePosMatch s1 s2
  | differsInOnePos s1 s2 0 = Just s1
  | otherwise = Nothing

filterDifferent :: String -> String -> String
filterDifferent [] [] = []
filterDifferent (c1:s1) (c2:s2)
  | c1 == c2 = [c1] ++ filterDifferent s1 s2
  | otherwise = filterDifferent s1 s2

part2 :: [String] -> [String] -> [String] -> Maybe String
part2 _ [] _ = Nothing
part2 _ _ (s1:s2:[]) = Just $ filterDifferent s1 s2
part2 allIds (currId:restIds) [] =
  let differingInOnePos = catMaybes $ map (\s -> onePosMatch s currId) allIds
      found =
        if length differingInOnePos > 0
        then differingInOnePos ++ [currId]
        else []
  in part2 allIds restIds found

main :: IO ()
main = do
  input <- readFile "data/day02.txt"
  let ids = filter (\s -> length s > 0) (splitOn "\n" input)
  print $ part1 ids
  print $ part2 ids ids []
