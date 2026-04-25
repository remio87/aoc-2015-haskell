module Day11 where

import Data.Char (chr, ord)
import Data.List (find, findIndex, group)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  let input = "hxbxwxba"
  let part1Answer = part1 input
  putStrLn $ "Part 1: " ++ show (part1Answer)
  putStrLn $ "Part 2: " ++ show (part2 part1Answer)

nextChar :: Char -> Char
nextChar 'z' = 'a'
nextChar c = chr (ord c + 1)

-- do recursively, give digit at Int
increment :: Int -> String -> String
increment i s =
  let c = s !! (8 - i - 1)
      before = take (8 - i - 1) s
      after = drop (8 - i) s
      incremented = before ++ [nextChar c] ++ after
   in if c /= 'z'
        then incremented
        else (increment (i + 1) incremented)

incrementIfForbidden :: String -> String
incrementIfForbidden s =
  case findIndex (\c -> c == 'i' || c == 'o' || c == 'l') (reverse s) of
    Nothing -> s
    Just i -> incrementIfForbidden incremented
      where
        c = s !! (8 - i - 1)
        before = take (8 - i - 1) s
        after = replicate i 'a'
        incremented = before ++ [nextChar c] ++ after

checkStraight :: String -> Bool
checkStraight [] = False
checkStraight (_ : []) = False
checkStraight (_ : _ : []) = False
checkStraight (a : b : c : rest) =
  let nextA = nextChar a
      nextnextA = nextChar nextA
   in (b == nextA && c == nextnextA && a /= 'z' && b /= 'z') || checkStraight (b : c : rest)

data PairState = PairState {prev :: Maybe Char, pairPos :: Maybe Int}

checkPair :: PairState -> Int -> String -> Bool
checkPair _ _ [] = False
checkPair (PairState Nothing _) i (c : s) = checkPair (PairState (Just c) Nothing) (i + 1) s
checkPair (PairState (Just prevChar) Nothing) i (c : s) =
  if (c == prevChar)
    then checkPair (PairState (Just c) (Just i)) (i + 1) s
    else checkPair (PairState (Just c) Nothing) (i + 1) s
checkPair (PairState (Just prevChar) (Just prevIdx)) i (c : s) =
  if (c == prevChar)
    then
      if i - prevIdx > 1
        then True
        else checkPair (PairState (Just c) (Just prevIdx)) (i + 1) s
    else checkPair (PairState (Just c) (Just prevIdx)) (i + 1) s

checkPair2 :: String -> Bool
checkPair2 s = (length pairs) >= 2
  where
    pairs = filter (\g -> (length g) >= 2) $ group s

part1 :: String -> String
part1 = fromJust . find (\s -> checkStraight s && checkPair2 s) . map incrementIfForbidden . iterate (increment 0) . incrementIfForbidden

part2 :: String -> String
part2 = part1 . increment 0