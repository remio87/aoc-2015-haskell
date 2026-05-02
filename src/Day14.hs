module Day14 where

main :: IO ()
main = do
  input <- readFile "inputs/day14.txt"
  putStrLn $ "part1: " ++ show (part1 input)
  putStrLn $ "part2: " ++ show (part2 input)

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
    point :: Int,
    secToNext :: Int
  }

-- nextState :: Deer -> DeerState -> (State, Int)
-- nextState deer (DeerState st _ stn)

step :: Deer -> DeerState -> DeerState
step deer (DeerState Flying pos pnt 1) = DeerState Resting (pos + (speed deer)) pnt (restDur deer)
step deer (DeerState Flying pos pnt stn) = DeerState Flying (pos + (speed deer)) pnt (stn - 1)
step deer (DeerState Resting pos pnt 1) = DeerState Flying pos pnt (flyDur deer)
step _ (DeerState Resting pos pnt stn) = DeerState Resting pos pnt (stn - 1)

initState :: Deer -> DeerState
initState deer = DeerState Flying 0 0 (flyDur deer)

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

givePoint :: [DeerState] -> [DeerState]
givePoint dss =
  map
    ( \ds ->
        if (position ds) == maxPos
          then
            DeerState (state ds) (position ds) ((point ds) + 1) (secToNext ds)
          else
            ds
    )
    dss
  where
    positions = map position dss
    maxPos = maximum positions

part2' :: [Deer] -> Int -> Int
part2' deers time = maximum $ map (\ds -> point ds) $ last $ take (time - 1) $ iterate rep $ map stepUncurry $ zip deers $ map initState deers
  where
    stepUncurry = uncurry step
    rep :: [DeerState] -> [DeerState]
    rep = map stepUncurry . zip deers . givePoint

part2 :: String -> Int
part2 input = part2' (map parseLine $ lines input) 2503