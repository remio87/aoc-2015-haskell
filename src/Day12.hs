module Day12 where

import Data.Char (isDigit)
import qualified Data.HashSet as HashSet

main :: IO ()
main = do
  input <- readFile "inputs/day12.txt"
  putStrLn $ "part1: " ++ (show $ part1 input)

charToReplace :: HashSet.HashSet Char
charToReplace =
  HashSet.fromList
    [ '"',
      '{',
      '}',
      '[',
      ']',
      ',',
      ':'
    ]

replaceChar :: Char -> Char
replaceChar c = if HashSet.member c charToReplace then ' ' else c

replaceChars :: String -> String
replaceChars = map replaceChar

isElemNumber :: String -> Bool
isElemNumber s = all (\c -> isDigit c || c == '-') s

part1 :: String -> Int
part1 = sum . map read . filter isElemNumber . words . replaceChars