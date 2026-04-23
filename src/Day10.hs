module Day10 where

main :: IO ()
main = do
  putStrLn $ "Part 1: " ++ show (part1 "1113122113")

--   putStrLn $ "Part 2: " ++ show (part2 input)

data Accum = Accum {prev :: Int, count :: Int} deriving (Show)

lookAndSay :: [Accum] -> [Int] -> [Accum]
lookAndSay acc [] = acc
lookAndSay [] (i : is) = lookAndSay [Accum i 1] is
lookAndSay ((Accum p c) : as) (i : is) =
  if p == i
    then
      lookAndSay ((Accum p (c + 1)) : as) is
    else
      lookAndSay ((Accum i 1) : (Accum p c) : as) is

accumToResult :: [Accum] -> String
accumToResult = foldl fn ""
  where
    fn :: String -> Accum -> String
    fn str (Accum val cnt) = show cnt ++ show val ++ str

parseLine :: String -> [Int]
parseLine = map (read . (: []))

part1 :: String -> Int
part1 = length . last . take 41 . iterate (accumToResult . lookAndSay [] . parseLine)

part2 :: String -> Int
part2 = undefined
