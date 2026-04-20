module Day08 where

import Data.Char (isHexDigit)

main :: IO ()
main = do
  input <- readFile "inputs/day08.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)

--   putStrLn $ "Part 2: " ++ show (part2 input)

countCharInMem :: Int -> String -> Int
countCharInMem i "" = i
countCharInMem i (_ : []) = i + 1
countCharInMem i ('\\' : '\\' : rest) = countCharInMem (i + 1) rest
countCharInMem i ('\\' : '"' : rest) = countCharInMem (i + 1) rest
countCharInMem i ('\\' : 'x' : d1 : d2 : rest) =
  let inc = if (isHexDigit d1 && isHexDigit d2) then 1 else 4
   in countCharInMem (i + inc) rest
countCharInMem i (_ : rest) = countCharInMem (i + 1) rest

checkDiff :: String -> Int
checkDiff s = (length s) - (countCharInMem 0 s) + 2

part1 :: String -> Int
part1 = sum . map checkDiff . lines

part2 :: String -> Int
part2 = undefined