module Day13 where

import Data.List (permutations)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

main :: IO ()
main = do
  input <- readFile "inputs/day13.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

data Line = Line
  { sub :: String,
    obj :: String,
    happy :: Int
  }
  deriving (Show)

type HappyEntry = Map.Map String Int

type HappyTable = Map.Map String HappyEntry

addLineToTable :: HappyTable -> Line -> HappyTable
addLineToTable t (Line s o h) =
  case Map.lookup s t of
    Nothing -> Map.insert s (Map.singleton o h) t
    Just entry -> Map.insert s (Map.insert o h entry) t

parseLine :: String -> Line
parseLine = go . words
  where
    go (s : _ : sign : v : _ : _ : _ : _ : _ : _ : o : _) =
      let val = read v
          hap = if sign == "gain" then val else -1 * val
       in Line s (init o) hap
    go _ = undefined

parseFile :: String -> [Line]
parseFile = map parseLine . lines

parseToTable :: String -> HappyTable
parseToTable = foldl addLineToTable Map.empty . parseFile

allArrangement :: [String] -> [[String]]
allArrangement (a : rest) = map (\ss -> [a] ++ ss ++ [a]) (permutations rest)
allArrangement _ = undefined

evalArrangeOne :: HappyTable -> [String] -> Int
evalArrangeOne t ss = foldl f 0 zipped
  where
    zipped = zip ss (tail ss)
    f i (a, b) = i + fromJust (Map.lookup a t >>= Map.lookup b)

evalArrange :: HappyTable -> [String] -> Int
evalArrange t ss = (evalArrangeOne t ss) + (evalArrangeOne t (reverse ss))

part1 :: String -> Int
part1 s = maximum $ map (evalArrange table) arranges
  where
    table = parseToTable s
    arranges = allArrangement (Map.keys table)

addMeToTable :: HappyTable -> HappyTable
addMeToTable t = addGuestHappy $ addMyHappy t
  where
    guests = Map.keys t
    addMyHappy :: HappyTable -> HappyTable
    addMyHappy tab = foldl (\tt g -> Map.insert "me" (Map.insert g 0 (Map.findWithDefault Map.empty "me" tt)) tt) tab guests
    addGuestHappy :: HappyTable -> HappyTable
    addGuestHappy tab = foldl (\tt g -> Map.insert g (fromJust $ Map.insert "me" 0 <$> Map.lookup g tt) tt) tab guests

part2 :: String -> Int
part2 s = maximum $ map (evalArrange table) arranges
  where
    table = addMeToTable $ parseToTable s
    arranges = allArrangement (Map.keys table)