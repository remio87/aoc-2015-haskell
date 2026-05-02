module Day14 where

main :: IO ()
main = do
  input <- readFile "inputs/day14.txt"
  putStrLn $ "part1: " ++ show (part1 input)

data Deer = Deer
  { speed :: Int,
    flyDur :: Int,
    restDur :: Int
  }
  deriving (Show)

data State = Flying | Resting

data DeerState = DeerState
  { state :: State,
    position :: Int,
    secToNext :: Int
  }

-- nextState :: Deer -> DeerState -> (State, Int)
-- nextState deer (DeerState st _ stn)

step :: Deer -> DeerState -> DeerState
step deer (DeerState Flying pos 1) = DeerState Resting (pos + (speed deer)) (restDur deer)
step deer (DeerState Flying pos stn) = DeerState Flying (pos + (speed deer)) (stn - 1)
step deer (DeerState Resting pos 1) = DeerState Flying pos (flyDur deer)
step _ (DeerState Resting pos stn) = DeerState Resting pos (stn - 1)

initState :: Deer -> DeerState
initState deer = DeerState Flying 0 (flyDur deer)

runDeer :: Deer -> Int -> Int
runDeer deer secs = position $ last $ take secs $ iterate (step deer) (initState deer)

wordsToIgnore :: [String]
wordsToIgnore =
  [ "can",
    "fly",
    "km/s",
    "for",
    "seconds,",
    "but",
    "then",
    "must",
    "rest",
    "for",
    "seconds."
  ]

parseLine :: String -> Deer
parseLine str = case filter (\w -> not $ w `elem` wordsToIgnore) $ words str of
  (_ : s : f : r : _) -> Deer (read s) (read f) (read r)
  _ -> undefined

part1 :: String -> Int
part1 = maximum . map (flip runDeer 2503) . map parseLine . lines