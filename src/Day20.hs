{-# LANGUAGE NumericUnderscores #-}

module Day20 where

import Data.Array (Array, accumArray, assocs)

main :: IO ()
main = do
  putStrLn $ "part1: " ++ show part1
  putStrLn $ "part2: " ++ show part2

input :: Int
input = 36_000_000

input1 :: Int
input1 = 3_600_000

houseScores :: Array Int Int
houseScores =
  accumArray
    (+)
    0
    (1, input1)
    [ (house, elf)
    | elf <- [1 .. input1],
      house <- [elf, elf * 2 .. input1]
    ]

houseScores2 :: Array Int Int
houseScores2 =
  accumArray
    (+)
    0
    (1, input)
    [ (house, elf * 11)
    | elf <- [1 .. input],
      house <- [elf, elf * 2 .. min (elf * 50) input]
    ]

-- allFactors :: Int -> [Int]
-- allFactors i = nub $ firstHalf ++ lastHalf
--   where
--     isqrt = floor . sqrt . fromIntegral
--     firstHalf = filter (\divisor -> i `mod` divisor == 0) [1 .. isqrt i]
--     lastHalf = map (\divisor -> i `div` divisor) firstHalf

-- allFactors i = filter f [1 .. i]
--   where
--     f divisor = i `mod` divisor == 0

-- houseScore :: Int -> Int
-- houseScore i = sum $ map (* 10) (allFactors i)

-- houseScoreCum :: [Int]
-- houseScoreCum = scanl1 (+) $ map houseScore [1 ..]

part1 :: Int
part1 = (+ 1) $ length $ takeWhile (\(_, v) -> v < input1) $ assocs houseScores

-- part1 = (+ 1) $ length $ takeWhile (< input) (map houseScore [1 ..])

part2 :: Int
part2 = (+ 1) $ length $ takeWhile (\(_, v) -> v < input) $ assocs houseScores2