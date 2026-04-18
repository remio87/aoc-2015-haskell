module Day05 where

import qualified Data.Map as Map

main :: IO ()
main = do
  input <- readFile "inputs/day05.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

data NiceState = NiceState
  { vowels :: Int,
    twiceAppeared :: Bool,
    prohibitAppeared :: Bool,
    prevChar :: Maybe Char
  }
  deriving (Show)

isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"

isProhibited :: Maybe Char -> Char -> Bool
isProhibited Nothing _ = False
isProhibited (Just c1) c2 = let s = [c1, c2] in s `elem` ["ab", "cd", "pq", "xy"]

step :: NiceState -> Char -> NiceState
step ns c =
  NiceState
    { vowels = nv,
      twiceAppeared = nta,
      prohibitAppeared = npa,
      prevChar = Just c
    }
  where
    prev = prevChar ns
    nv = let vs = vowels ns in if isVowel c then vs + 1 else vs
    nta =
      twiceAppeared ns || case prev of
        Nothing -> False
        Just p -> p == c
    npa = prohibitAppeared ns || isProhibited prev c

parse :: String -> [String]
parse = lines

isNice :: String -> Bool
isNice s =
  let res = foldl step (NiceState 0 False False Nothing) s
   in and [not (prohibitAppeared res), (vowels res) >= 3, twiceAppeared res]

part1 :: String -> Int
part1 = length . filter id . map isNice . parse

data NiceState2 = NiceState2
  { curPos :: Int,
    prevprev :: Maybe Char,
    prev :: Maybe Char,
    pastPairs :: Map.Map String Int,
    repeatAppeared :: Bool,
    pairAppeared :: Bool
  }
  deriving (Show)

addPair :: Maybe Char -> Char -> Int -> Map.Map String Int -> Map.Map String Int
addPair Nothing _ _ m = m
addPair (Just p) c pos m =
  let pair = [p, c]
   in case Map.lookup pair m of
        Nothing -> Map.insert pair pos m
        (Just _) -> m

checkRepeat :: Maybe Char -> Maybe Char -> Char -> Bool
checkRepeat Nothing _ _ = False
checkRepeat _ Nothing _ = False
checkRepeat (Just pp) (Just _) c = pp == c

checkPair :: Maybe Char -> Char -> Int -> Map.Map String Int -> Bool
checkPair Nothing _ _ _ = False
checkPair (Just p) c pos m =
  let pair = [p, c]
   in case Map.lookup pair m of
        Nothing -> False
        Just pairPos -> pos - pairPos > 1

isNice2 :: String -> Bool
isNice2 str =
  let res = foldl step2 (NiceState2 0 Nothing Nothing Map.empty False False) str
   in repeatAppeared res && pairAppeared res

step2 :: NiceState2 -> Char -> NiceState2
step2 (NiceState2 cp pp p pairs ra pa) c =
  let nextPos = cp + 1
      nextpp = p
      nextp = Just c
      nextPairs = addPair p c cp pairs
      nextRa = ra || checkRepeat pp p c
      nextPa = pa || checkPair p c cp pairs
   in NiceState2 nextPos nextpp nextp nextPairs nextRa nextPa

part2 :: String -> Int
part2 = length . filter id . map isNice2 . parse
