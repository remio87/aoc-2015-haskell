module Day01 where

import Data.List
import Data.Maybe

main :: IO ()
main = do
  input <- readFile "inputs/day01.txt"
  let parsed = parse input
  putStrLn $ "Part 1: " ++ show (part1 parsed)
  putStrLn $ "Part 2: " ++ show (part2 parsed)

parse :: String -> String
parse = id

part1 :: String -> Int
part1 str = sum $ map conv str
  where
    conv '(' = 1
    conv ')' = -1
    conv _ = undefined

-- part1 s = foldr f 0 s
--   where
--     f ch flr
--       | ch == '(' = flr + 1
--       | ch == ')' = flr - 1
--       | otherwise = undefined


part2 str = snd $ fromJust $ find ((< 0) . fst) (zip flrs [1 ..])
  where
    flrs = scanl f 0 str
    f flr ch
      | ch == '(' = flr + 1
      | ch == ')' = flr - 1
      | otherwise = undefined

-- part2 str = snd $ step (0, 0) str

-- step :: (Int, Int) -> String -> (Int, Int)
-- step (flr, pos) "" = (flr, pos)
-- step (flr, pos) ('(' : rest) = step (flr + 1, pos + 1) rest
-- step (0, pos) (')' : _) = (-1, pos + 1)
-- step (flr, pos) (')' : rest) = step (flr - 1, pos + 1) rest
-- step _ _ = undefined