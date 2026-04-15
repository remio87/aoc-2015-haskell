module Day02 where

import Data.List (sort, subsequences)

-- import Data.List
-- import Data.Maybe

main :: IO ()
main = do
  input <- readFile "inputs/day02.txt"
  -- let parsed = parse input
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> [(Int, Int, Int)]
parse str = map getTuple $ lines str
  where
    getTuple s =
      let [(a, rest1)] = reads s
          [(b, rest2)] = reads $ tail rest1
          [(c, _)] = reads $ tail rest2
       in (a, b, c)

getArea :: [[Int]] -> Int
getArea l = (getAreaNoExtra l) + (getMinFaceArea l)

getAreaNoExtra :: [[Int]] -> Int
getAreaNoExtra l = (* 2) $ sum $ map (listProduct) l

getMinFaceArea :: [[Int]] -> Int
getMinFaceArea l = minimum $ map (listProduct) l

listProduct :: [Int] -> Int
listProduct [a, b] = a * b

singleData :: (Int, Int, Int)
singleData = (2, 3, 4)

combinationTwo :: (Int, Int, Int) -> [[Int]]
combinationTwo a = filter ((== 2) . length) $ subsequences $ tupleToList a

tupleToList :: (Int, Int, Int) -> [Int]
tupleToList (a, b, c) = [a, b, c]

ribbon :: (Int, Int, Int) -> Int
ribbon (a, b, c) = (ribbonToWrap (a, b, c)) + (ribbonForBow (a, b, c))
  where
    ribbonToWrap (a, b, c) = (* 2) $ sum $ init $ sort $ tupleToList (a, b, c)
    ribbonForBow t = product $ tupleToList t

--   where
--     getTuple s = (fst $ br s, fst $ br $ rest s, tail $ snd $ br $ rest s)
--     br s = break (== 'x') s
--     rest s = tail $ snd $ br s
--     readTuple (a, b, c) = (read a, read b, read c)

part1 :: String -> Int
part1 = sum . map (getArea . combinationTwo) . parse

part2 :: String -> Int
part2 = sum . map ribbon . parse