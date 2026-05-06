module Day19 where

import Data.List (isPrefixOf, nub, sortBy)
import Data.Maybe (fromJust, listToMaybe, mapMaybe)
import Data.Ord (Down (..), comparing)

main :: IO ()
main = do
  input <- readFile "inputs/day19.txt"
  putStrLn $ "part1: " ++ show (part1 input)
  putStrLn $ "part2: " ++ show (part2 input)

separateInput :: String -> ([String], String)
separateInput = (\(f, s) -> (f, last s)) . break null . lines

parseLine :: String -> (String, String)
parseLine l = case words l of
  (a : "=>" : b : _) -> (a, b)
  _ -> undefined

replacePrefix :: [(String, String)] -> String -> [String]
replacePrefix rules str = concatMap fn rules
  where
    fn :: (String, String) -> [String]
    fn (a, b) =
      if isPrefixOf a str
        then [b ++ (drop (length a) str)]
        else []

allSeparations :: String -> [(String, String)]
allSeparations str = map (`splitAt` str) [0 .. length str]

replaceMiddle :: [(String, String)] -> (String, String) -> [String]
replaceMiddle rules (f, s) = map (f ++) $ replacePrefix rules s

part1 :: String -> Int
part1 str = length $ nub $ concatMap (replaceMiddle rules) $ allSeparations orig
  where
    (ruleLines, orig) = separateInput str
    rules = map parseLine ruleLines

sortRules :: [(String, String)] -> [(String, String)]
sortRules = sortBy (comparing (Down . length . snd))

flipRules :: [(String, String)] -> [(String, String)]
flipRules = map (\(a, b) -> (b, a))

applyRule :: (String, String) -> (String, String) -> Maybe String
applyRule (a, b) (f, s)
  | isPrefixOf a s = Just $ f ++ b ++ (drop (length a) s)
  | otherwise = Nothing

tryRules :: [(String, String)] -> (String, String) -> Maybe String
tryRules rules sep = listToMaybe $ mapMaybe (flip applyRule sep) rules

stepBack :: [(String, String)] -> String -> String
stepBack rules str =
  case mapMaybe (\rule -> tryAllPositions rule str) rules of
    (x : _) -> x
    [] -> error "stuck"
  where
    tryAllPositions rule = listToMaybe . mapMaybe (applyRule rule) . allSeparations

-- stepBack :: [(String, String)] -> String -> String
-- stepBack rules str = head $ mapMaybe (tryRules rules) (allSeparations str)

part2 :: String -> Int
part2 str = length $ takeWhile (/= "e") $ iterate (stepBack rules) orig
  where
    (ruleLines, orig) = separateInput str
    rules = flipRules $ sortRules $ map parseLine ruleLines