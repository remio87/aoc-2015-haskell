module Day13 where

main :: IO ()
main = do
  input <- readFile "inputs/day13.txt"
  return ()

data Line = Line
  { sub :: String,
    obj :: String,
    happy :: Int
  }
  deriving (Show)

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

part1 :: String -> Int
part1 = undefined

part2 :: String -> Int
part2 = undefined