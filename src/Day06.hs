module Day06 where

main :: IO ()
main = do
  input <- readFile "inputs/day06.txt"
  return ()

--   putStrLn $ "Part 1: " ++ show (part1 input)
--   putStrLn $ "Part 2: " ++ show (part2 input)

initArray :: [[Bool]]
initArray = take 1000 $ repeat (take 1000 $ repeat False)

type Coord = (Int, Int)

type Rect = (Coord, Coord)

data Op = On | Off | Toggle
  deriving (Show)

parseCoord :: String -> Coord
parseCoord s =
  let tup = break (== ',') s
   in (read $ fst $ tup, read $ tail $ snd tup)

parseOp :: [String] -> (Op, [String])
parseOp ("turn" : "on" : rest) = (On, rest)
parseOp ("turn" : "off" : rest) = (Off, rest)
parseOp ("toggle" : rest) = (Toggle, rest)
parseOp _ = undefined

parseLine :: String -> (Op, Rect)
parseLine s =
  let (op, rest) = parseOp $ words $ s
   in case rest of
        (lb : _ : ru : _) -> (op, (parseCoord lb, parseCoord ru))
        _ -> undefined

part1 :: String -> Int
part1 = undefined

part2 :: String -> Int
part2 = undefined
