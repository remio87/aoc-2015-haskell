module Day07 where

import Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import Data.Char
import qualified Data.Map as Map
import Data.Maybe (fromJust)

main :: IO ()
main = do
  input <- readFile "inputs/day07.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)

--   putStrLn $ "Part 2: " ++ show (part2 input)

data Signal = Value Int | Wire String

data Gate
  = Immediate Signal
  | Not Signal
  | And Signal Signal
  | Or Signal Signal
  | Lshift Signal Int
  | Rshift Signal Int

parseSig :: String -> Signal
parseSig s = if all isDigit s then Value $ read s else Wire s

parseLine :: String -> (String, Gate)
parseLine s = case words s of
  (val : "->" : wire : _) -> (wire, Immediate $ parseSig val)
  ("NOT" : sig : "->" : wire : _) -> (wire, Not $ parseSig sig)
  (sig1 : "AND" : sig2 : "->" : wire : _) -> (wire, And (parseSig sig1) (parseSig sig2))
  (sig1 : "OR" : sig2 : "->" : wire : _) -> (wire, Or (parseSig sig1) (parseSig sig2))
  (sig1 : "LSHIFT" : val : "->" : wire : _) -> (wire, Lshift (parseSig sig1) (read val))
  (sig1 : "RSHIFT" : val : "->" : wire : _) -> (wire, Rshift (parseSig sig1) (read val))
  _ -> undefined

getSigVal :: Signal -> Map.Map String Int -> Int
getSigVal (Value i) _ = i
getSigVal (Wire s) m = m Map.! s

toFunc :: Gate -> (Map.Map String Int -> Int)
toFunc (Immediate sig) = (\m -> getSigVal sig m)
toFunc (Not sig) = (\m -> complement (getSigVal sig m) .&. 0xFFFF)
toFunc (And sig1 sig2) = (\m -> (getSigVal sig1 m .&. getSigVal sig2 m) .&. 0xFFFF)
toFunc (Or sig1 sig2) = (\m -> (getSigVal sig1 m .|. getSigVal sig2 m) .&. 0xFFFF)
toFunc (Lshift sig i) = (\m -> (shiftL (getSigVal sig m) i) .&. 0xFFFF)
toFunc (Rshift sig i) = (\m -> (shiftR (getSigVal sig m) i) .&. 0xFFFF)

rules :: [(String, Gate)] -> Map.Map String (Map.Map String Int -> Int)
rules = Map.fromList . map (\(s, g) -> (s, toFunc g))

results :: Map.Map String (Map.Map String Int -> Int) -> Map.Map String Int
results r = let result = Map.map (\f -> f result) r in result

part1 :: String -> Int
part1 = fromJust . Map.lookup "a" . results . rules . map parseLine . lines

part2 :: String -> Int
part2 = undefined
