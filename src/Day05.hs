module Day05 where

main :: IO ()
main = do
  input <- readFile "inputs/day05.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)

--   putStrLn $ "Part 2: " ++ show (part2 prefix)

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

part2 :: String -> Int
part2 = undefined