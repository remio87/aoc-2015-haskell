{-# LANGUAGE OverloadedStrings #-}

module Day12 where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (isDigit)
import qualified Data.HashSet as HashSet
import Data.Maybe (fromJust)
import qualified Data.Vector as V

main :: IO ()
main = do
  input <- readFile "inputs/day12.txt"
  putStrLn $ "part1: " ++ (show $ part1 input)
  putStrLn $ "part2: " ++ (show $ part2 input)

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

-- findObjectEnd :: String -> Int
-- findObjectEnd = findObjectEnd' 0 0

-- findObjectEnd' :: Int -> Int -> String -> Int
-- findObjectEnd' i _ [] = i
-- findObjectEnd' i count (c : rest) =
--   if c == '}'
--     then
--       if count == 0
--         then i
--         else findObjectEnd' (i + 1) (count - 1) rest
--     else
--       if c == '{'
--         then findObjectEnd' (i + 1) (count + 1) rest
--         else findObjectEnd' (i + 1) count rest

-- removeRed :: Int -> Int -> String -> String
-- removeRed _ _ [] = []
-- removeRed cur _ ('{' : rest) = removeRed (cur + 1) cur (['{'] ++ rest)
-- removeRed cur prevBra s =
--   if cur > length s
--     then s
--     else
--       if "{" `isPrefixOf` (drop cur s)
--         then removeRed (cur + 1) cur s
--         else
--           if "red" `isPrefixOf` (drop cur s)
--             then removeRed 0 0 (stripRed s)
--             else removeRed (cur + 1) prevBra s
--   where
--     stripRed str = take prevBra str ++ drop nextBra str
--     nextBra = findObjectEnd $ drop prevBra s

part1 :: String -> Int
part1 = sum . map read . filter isElemNumber . words . replaceChars

parseJson :: String -> Maybe Value
parseJson s = decode $ BL.pack s

hasObjectRed :: Object -> Bool
hasObjectRed o = any (\e -> e == (String "red")) o

removeRed :: Value -> Value
removeRed (Object o) = if hasObjectRed o then Null else Object (KM.map removeRed o)
removeRed (Array a) = Array (V.map removeRed a)
removeRed (String "red") = Null
removeRed v = v

part2 :: String -> Int
part2 s = fromJust $ (part1 . BL.unpack . encode) <$> (removeRed <$> parseJson s)