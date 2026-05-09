module Day20 where

import Data.List (nub)

main :: IO ()
main = do
  return ()

input :: Int
input = 36000000

allFactors :: Int -> [Int]
allFactors i = nub $ firstHalf ++ lastHalf
  where
    isqrt = floor . sqrt . fromIntegral
    firstHalf = filter (\divisor -> i `mod` divisor == 0) [1 .. isqrt i]
    lastHalf = map (\divisor -> i `div` divisor) firstHalf

-- allFactors i = filter f [1 .. i]
--   where
--     f divisor = i `mod` divisor == 0

houseScore :: Int -> Int
houseScore i = sum $ map (* 10) (allFactors i)

houseScoreCum :: [Int]
houseScoreCum = scanl1 (+) $ map houseScore [1 ..]

part1 :: Int
part1 = (+ 1) $ length $ takeWhile (< input) (map houseScore [1 ..])