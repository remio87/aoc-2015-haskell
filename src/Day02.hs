module Day02 where

import Data.List
import Data.Maybe

main :: IO ()
main = do
  input <- readFile "inputs/day02.txt"
  let parsed = parse input
  return ()

--   putStrLn $ "Part 1: " ++ show (part1 parsed)
--   putStrLn $ "Part 2: " ++ show (part2 parsed)

parse :: String -> String
parse = id

part1 :: String -> Int
part1 = undefined
