import qualified Day01
import qualified Day02
import qualified Day03
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["01"] -> Day01.main
    ["02"] -> Day02.main
    ["03"] -> Day03.main
    _ -> putStrLn "Usage: aoc2015 <day>"
