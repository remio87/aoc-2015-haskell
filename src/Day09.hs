module Day09 where

import Data.List (permutations)
import qualified Data.Set as S

main :: IO ()
main = do
  input <- readFile "inputs/day09.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

type Graph = [(String, String, Int)]

lookupDistance :: Graph -> String -> String -> Int
lookupDistance g p1 p2 = case head $ filter (\(p1', p2', _) -> (p1 == p1' && p2 == p2') || (p1 == p2' && p2 == p1')) g of
  (_, _, d) -> d

parseLine :: String -> (String, String, Int)
parseLine s = case words s of
  (from : "to" : dest : "=" : dist : _) -> (from, dest, read dist)
  _ -> undefined

places :: Graph -> S.Set String
places ps = foldl (\set (p1, p2, _) -> S.insert p2 (S.insert p1 set)) S.empty ps

perm :: S.Set String -> [[String]]
perm set = permutations $ S.toList set

calcTotal :: Graph -> [String] -> Int
calcTotal g route = sum $ zipWith (lookupDistance g) route (tail $ route)

-- calcTotal :: Graph -> Int -> [String] -> Int
-- calcTotal _ i [] = i
-- calcTotal _ i (_ : []) = i
-- calcTotal g i (p1 : p2 : rest) = calcTotal g (i + (lookupDistance g p1 p2)) (p2 : rest)

part1 :: String -> Int
part1 input =
  let g = map parseLine $ lines input
      ps = places g
      prm = perm ps
   in minimum $ map (calcTotal g) prm

part2 :: String -> Int
part2 input =
  let g = map parseLine $ lines input
      ps = places g
      prm = perm ps
   in maximum $ map (calcTotal g) prm
