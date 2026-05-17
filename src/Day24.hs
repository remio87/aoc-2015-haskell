module Day24 where

import Data.List (minimumBy, sort)
import Data.Ord (comparing)

main :: IO ()
main = do
  input <- readFile "inputs/day24.txt"
  let inputL = reverse $ sort $ map read $ lines input
  putStrLn $ "part1: " ++ show (part1 inputL)
  putStrLn $ "part2: " ++ show (part2 inputL)

testInput :: [Int]
testInput = [11, 10, 9, 8, 7, 5, 4, 3, 2, 1]

choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose _ [] = []
choose k (x : xs) = map (x :) (choose (k - 1) xs) ++ choose k xs

allCandidates :: [Int] -> Int -> [[Int]]
allCandidates [] 0 = [[]]
allCandidates [] _ = []
allCandidates (x : xs) target
  | x > target = allCandidates xs target
  | x == target = [x] : allCandidates xs target
  | otherwise = allCandidates xs target ++ map (x :) (allCandidates xs (target - x))

canComposeGroups :: [Int] -> Int -> [Int] -> Int -> Bool
canComposeGroups input target g1 groupsLeft
  | groupsLeft == 0 = True
  | otherwise = any tryGroup (allCandidates remaining target)
  where
    remaining = filter (`notElem` g1) input
    tryGroup g2 = canComposeGroups remaining target g2 (groupsLeft - 1)

part1 :: [Int] -> Int
part1 input = product $ minimumBy (comparing product) $ possibleGroup1 input
  where
    possibleGroup1 = possibleGroup1With 3

part2 :: [Int] -> Int
part2 input = product $ minimumBy (comparing product) $ possibleGroup1 input
  where
    possibleGroup1 = possibleGroup1With 4

possibleGroup1With :: Int -> [Int] -> [[Int]]
possibleGroup1With totalGroups input = head $ filter (not . null) $ map validOfSize [1 ..]
  where
    target = sum input `div` totalGroups
    validOfSize k =
      filter (\g -> canComposeGroups input target g (totalGroups - 1)) $
        filter (\g -> sum g == target) $
          choose k input