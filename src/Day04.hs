module Day04 where

import Crypto.Hash (Digest, MD5, hash)
import Data.ByteString.Char8 (pack)
import Data.List (isPrefixOf)

main :: IO ()
main = do
  putStrLn $ "Part 1: " ++ show (part1 prefix)
  putStrLn $ "Part 2: " ++ show (part2 prefix)

prefix = "iwrupvqb"

part1 :: String -> Int
part1 pf = head $ filter valid [0 ..]
  where
    valid :: Int -> Bool
    valid i = "00000" `isPrefixOf` md5 (pf ++ (show i))

-- check str = isPrefixOf "00000" str

-- checkNum :: String -> Int -> (Bool, Int)
-- checkNum pf i = (check $ md5 (pf ++ (show i)), i)

md5 :: String -> String
md5 s = show (hash (pack s) :: Digest MD5)

-- part1 :: String -> Int
-- part1 pf = snd $ maybe (False, 0) id $ find (\(b, _) -> b) $ map (checkNum pf) [0 ..]

part2 :: String -> Int
part2 pf = head $ filter valid [0 ..]
  where
    valid :: Int -> Bool
    valid i = "000000" `isPrefixOf` md5 (pf ++ (show i))