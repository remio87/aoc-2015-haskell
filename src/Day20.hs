{-# LANGUAGE NumericUnderscores #-}

module Day20 where

import Data.Array (Array, accumArray, assocs)
import Data.List (nub)

main :: IO ()
main = do
  return ()

input :: Int
input = 3_600_000

-- input = 36_000_000

houseScores :: Array Int Int
houseScores =
  accumArray
    (+)
    0
    (1, input)
    [ (house, elf)
    | elf <- [1 .. input],
      house <- [elf, elf * 2 .. input]
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
part1 = (+ 1) $ length $ takeWhile (\(_, v) -> v < input) $ assocs houseScores

-- part1 = (+ 1) $ length $ takeWhile (< input) (map houseScore [1 ..])