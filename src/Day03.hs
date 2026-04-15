module Day03 where

-- import Data.List
-- import Data.Maybe

import Control.Monad.Trans.State
import qualified Control.Monad.Trans.State
import Data.List (nub)

main :: IO ()
main = do
  input <- readFile "inputs/day03.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)

--   putStrLn $ "Part 2: " ++ show (part2 input)

data Direction = DRight | DLeft | DUp | DDown

type Coord = (Int, Int)

visit :: Direction -> State [Coord] Coord
visit d = state (\s -> (nextPos (last s) d, s ++ [(nextPos (last s) d)]))
  where
    nextPos (x, y) DRight = (x + 1, y)
    nextPos (x, y) DLeft = (x - 1, y)
    nextPos (x, y) DUp = (x, y + 1)
    nextPos (x, y) DDown = (x, y - 1)

parse :: String -> [Direction]
parse = map charToDir
  where
    charToDir '>' = DRight
    charToDir '<' = DLeft
    charToDir '^' = DUp
    charToDir 'v' = DDown
    charToDir _ = undefined

part1 :: String -> Int
part1 str = length $ nub visitedCoords
  where
    (_, visitedCoords) = runState (mapM_ visit (parse str)) [(0, 0)]

part2 :: String -> Int
part2 = undefined