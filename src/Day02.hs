module Day02 where

-- import Data.List
-- import Data.Maybe

main :: IO ()
main = do
  input <- readFile "inputs/day02.txt"
  let parsed = parse input
  return ()

--   putStrLn $ "Part 1: " ++ show (part1 parsed)
--   putStrLn $ "Part 2: " ++ show (part2 parsed)

parse :: String -> [(Int, Int, Int)]
parse str = map getTuple $ lines str
  where
    getTuple s =
      let [(a, rest1)] = reads s
          [(b, rest2)] = reads $ tail rest1
          [(c, _)] = reads $ tail rest2
       in (a, b, c)

--   where
--     getTuple s = (fst $ br s, fst $ br $ rest s, tail $ snd $ br $ rest s)
--     br s = break (== 'x') s
--     rest s = tail $ snd $ br s
--     readTuple (a, b, c) = (read a, read b, read c)

part1 :: String -> Int
part1 = undefined
