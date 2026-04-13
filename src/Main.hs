import qualified Day01
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["01"] -> Day01.main
    -- ["02"] -> Day02.main
    _ -> putStrLn "Usage: aoc2015 <day>"
