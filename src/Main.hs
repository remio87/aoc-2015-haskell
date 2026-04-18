import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["01"] -> Day01.main
    ["02"] -> Day02.main
    ["03"] -> Day03.main
    ["04"] -> Day04.main
    ["05"] -> Day05.main
    ["06"] -> Day06.main
    _ -> putStrLn "Usage: aoc2015 <day>"
