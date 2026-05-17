module Day24 where

import Data.List (sort)

main :: IO ()
main = do
  input <- readFile "inputs/day24.txt"
  let inputL = sort $ map read $ lines input
  putStrLn $ show inputL
  putStrLn $ "part1: " ++ show (part1 inputL)

testInput :: [Int]
testInput = [11, 10, 9, 8, 7, 5, 4, 3, 2, 1]

-- get all candidates
-- list is sorted and starts with largest number
-- inputs: list of Int, target value
-- output: list of candidates, which is list of Int
allCandidates :: [Int] -> Int -> [[Int]]
allCandidates [] 0 = [[]]
allCandidates [] _ = []
allCandidates (x : xs) target
  | x == target = [[x]] ++ allCandidates xs target
  | x > target = allCandidates xs target
  | x < target = (allCandidates xs target) ++ (map (++ [x]) (allCandidates xs (target - x)))
  | otherwise = undefined

-- input: input list, target weight, group1 list
-- output: can compose group2 and group3
canComposeGroups :: [Int] -> Int -> [Int] -> Bool
canComposeGroups input target g1 = not $ null $ allCandidates remaining target
  where
    remaining = filter (`notElem` g1) input

possibleGroup1 :: [Int] -> [[Int]]
possibleGroup1 input = filter (canComposeGroups input target) (allCandidates input target)
  where
    target = sum input `div` 3

getFewestElements :: [[Int]] -> [[Int]]
getFewestElements xs = filter (\l -> length l == minLen) xs
  where
    minLen = minimum $ map length xs

getMinEntanglement :: [[Int]] -> Int
getMinEntanglement xs = head $ sort $ map product xs

part1 :: [Int] -> Int
part1 = getMinEntanglement . getFewestElements . possibleGroup1