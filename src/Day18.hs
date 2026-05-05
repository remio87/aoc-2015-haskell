module Day18 where

import Data.Array (Array, bounds, elems, indices, listArray, (!))
import Data.Ix (inRange)

main :: IO ()
main = do
  input <- readFile "inputs/day18.txt"
  putStrLn $ "part 1: " ++ show (part1 input)

arrSize :: Int
arrSize = 100

initArray :: [[Bool]] -> Array (Int, Int) Bool
initArray = listArray ((0, 0), (arrSize - 1, arrSize - 1)) . concat

getCell :: Array (Int, Int) Bool -> (Int, Int) -> Bool
getCell ar idx = case inRange (bounds ar) idx of
  False -> False
  True -> ar ! idx

nextGrid :: Array (Int, Int) Bool -> Array (Int, Int) Bool
nextGrid arr = listArray (bounds arr) [nextCell (i, j) | (i, j) <- indices arr]
  where
    adjacentSum :: (Int, Int) -> Int
    adjacentSum idx = length $ filter id $ map (getCell arr) $ adjacentIndices idx
    nextCell :: (Int, Int) -> Bool
    nextCell idx = case arr ! idx of
      True -> if adjacentSum idx == 2 || adjacentSum idx == 3 then True else False
      False -> if adjacentSum idx == 3 then True else False

adjacentIndices :: (Int, Int) -> [(Int, Int)]
adjacentIndices (x, y) = filter (/= (x, y)) [(a, b) | a <- [x - 1 .. x + 1], b <- [y - 1 .. y + 1]]

part1 :: String -> Int
part1 = length . filter id . elems . (!! 100) . iterate nextGrid . initArray . map (map conv) . lines
  where
    conv '.' = False
    conv '#' = True
    conv _ = undefined