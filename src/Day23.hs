module Day23 where

main :: IO ()
main = do
  input <- readFile "inputs/day23.txt"
  putStrLn $ "part1: " ++ show (part1 input)
  putStrLn $ "part2: " ++ show (part2 input)

data Register = RegA | RegB
  deriving (Show, Eq)

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

data State = State
  { ops :: [Op],
    ptr :: Int,
    regA :: Int, -- non negative
    regB :: Int -- non negative
  }
  deriving (Show)

applyRegOp :: State -> Register -> (Int -> Int) -> State
applyRegOp st reg fn =
  if reg == RegA
    then st {regA = appliedVal, ptr = nextPtr}
    else st {regB = appliedVal, ptr = nextPtr}
  where
    reg' = if reg == RegA then regA else regB
    appliedVal = max (fn (reg' st)) 0
    nextPtr = (ptr st) + 1

halfReg :: State -> Register -> State
halfReg st reg = applyRegOp st reg (\i -> i `div` 2)

tripleReg :: State -> Register -> State
tripleReg st reg = applyRegOp st reg (\i -> i * 3)

incReg :: State -> Register -> State
incReg st reg = applyRegOp st reg (\i -> i + 1)

jumpOp :: State -> Register -> (Int -> Bool) -> Int -> State
jumpOp st reg cond offset =
  if cond $ reg' st
    then st {ptr = (ptr st) + offset}
    else st {ptr = ptrIncremented}
  where
    reg' = if reg == RegA then regA else regB
    ptrIncremented = (ptr st) + 1

jump :: State -> Int -> State
jump st offset = jumpOp st RegA (\_ -> True) offset

jumpIfEven :: State -> Register -> Int -> State
jumpIfEven st reg offset = jumpOp st reg (\i -> even i) offset

jumpIfOne :: State -> Register -> Int -> State
jumpIfOne st reg offset = jumpOp st reg (\i -> i == 1) offset

step :: State -> State
step st =
  if (ptr st) >= (length $ ops st)
    then st
    else case (ops st) !! (ptr st) of
      (Half reg) -> step $ halfReg st reg
      (Triple reg) -> step $ tripleReg st reg
      (Inc reg) -> step $ incReg st reg
      (Jump offset) -> step $ jump st offset
      (JumpIfEven reg offset) -> step $ jumpIfEven st reg offset
      (JumpIfOne reg offset) -> step $ jumpIfOne st reg offset

initState :: [Op] -> State
initState opsInput =
  State
    { ops = opsInput,
      ptr = 0,
      regA = 0,
      regB = 0
    }

part1 :: String -> State
part1 = step . initState . map parseLine . lines

initState2 :: [Op] -> State
initState2 opsInput =
  State
    { ops = opsInput,
      ptr = 0,
      regA = 1,
      regB = 0
    }

part2 :: String -> State
part2 = step . initState2 . map parseLine . lines
