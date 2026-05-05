module Day18 where

main :: IO ()
main = do
  input <- readFile "inputs/day18-example.txt"
  putStrLn $ "part 1 (WIP): " ++ show (part1 input)

part1 :: String -> [[Bool]]
part1 = map (map conv) . lines
  where
    conv '.' = False
    conv '#' = True
    conv _ = undefined