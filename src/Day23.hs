module Day23 where

main :: IO ()
main = do
  input <- readFile "inputs/day23.txt"
  let parsed = map parseLine $ lines input
  putStrLn $ show parsed

data Register = RegA | RegB
  deriving (Show)

data Op
  = Half Register
  | Triple Register
  | Inc Register
  | Jump Int
  | JumpIfEven Register Int
  | JumpIfOne Register Int
  deriving (Show)

parseLine :: String -> Op
parseLine s = parseWords $ words s

parseRegister :: (Register -> Op) -> String -> Op
parseRegister ctor reg = if reg == "a" then ctor RegA else ctor RegB

parseJumpIf :: (Register -> Int -> Op) -> String -> String -> Op
parseJumpIf ctor reg offset = ctor reg' (parseOffset offset)
  where
    reg' = if reg == "a," then RegA else RegB

parseOffset :: String -> Int
parseOffset (sign : val) = if sign == '+' then read val else negate $ read val
parseOffset _ = undefined

parseWords :: [String] -> Op
parseWords ("hlf" : reg : _) = parseRegister Half reg
parseWords ("tpl" : reg : _) = parseRegister Triple reg
parseWords ("inc" : reg : _) = parseRegister Inc reg
parseWords ("jmp" : offset : _) = Jump (parseOffset offset)
parseWords ("jie" : reg : offset : _) = parseJumpIf JumpIfEven reg offset
parseWords ("jio" : reg : offset : _) = parseJumpIf JumpIfOne reg offset
parseWords _ = undefined