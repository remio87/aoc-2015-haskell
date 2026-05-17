module Day24 where

import Data.List (minimumBy, sort)
import Data.Ord (comparing)

main :: IO ()
main = do
  input <- readFile "inputs/day24.txt"
  let inputL = reverse $ sort $ map read $ lines input
  putStrLn $ "part1: " ++ show (part1 inputL)

testInput :: [Int]
testInput = [11, 10, 9, 8, 7, 5, 4, 3, 2, 1]

choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose _ [] = []
choose k (x : xs) = map (x :) (choose (k - 1) xs) ++ choose k xs

canComposeGroups :: [Int] -> Int -> [Int] -> Bool
canComposeGroups input target g1 = not $ null $ allCandidates remaining target
  where
    remaining = filter (`notElem` g1) input

allCandidates :: [Int] -> Int -> [[Int]]
allCandidates [] 0 = [[]]
allCandidates [] _ = []
allCandidates (x : xs) target
  | x > target = allCandidates xs target
  | x == target = [x] : allCandidates xs target
  | otherwise = allCandidates xs target ++ map (x :) (allCandidates xs (target - x))

possibleGroup1 :: [Int] -> [[Int]]
possibleGroup1 input = head $ filter (not . null) $ map validOfSize [1 ..]
  where
    target = sum input `div` 3
    validOfSize k =
      filter (canComposeGroups input target) $
        filter (\g -> sum g == target) $
          choose k input

part1 :: [Int] -> Int
part1 input = product $ minimumBy (comparing product) $ possibleGroup1 input
