module Day06 where

import Data.Array

main :: IO ()
main = do
  input <- readFile "inputs/day06.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

type Lights = Array (Int, Int) Bool

initArray :: Lights
initArray = listArray ((0, 0), (999, 999)) (repeat False)

type Coord = (Int, Int)

type Rect = (Coord, Coord)

data Op = On | Off | Toggle
  deriving (Show)

coordsInsideRect :: Rect -> [Coord]
coordsInsideRect ((lx, by), (rx, ty)) = [(x, y) | x <- [lx .. rx], y <- [by .. ty]]

setInRect :: Bool -> Lights -> Rect -> Lights
setInRect b l rect = l // [(c, b) | c <- coordsInsideRect rect]

-- allOnInRect :: Lights -> Rect -> Lights
-- allOnInRect l rect = l // updates
--   where
--     updates = zip (coordsInsideRect rect) (repeat True)

-- allOffInRect :: Lights -> Rect -> Lights
-- allOffInRect l rect = l // updates
--   where
--     updates = zip (coordsInsideRect rect) (repeat False)

toggleInRect :: Lights -> Rect -> Lights
toggleInRect l rect = l // [(c, not $ l ! c) | c <- coordsInsideRect rect]

-- updates
--   where
--     updates = zip (coordsInsideRect rect) (map f (coordsInsideRect rect))
--     f coord = not $ l ! coord

parseCoord :: String -> Coord
parseCoord s =
  let (l, r) = break (== ',') s
   in (read l, read $ tail r)

parseOp :: [String] -> (Op, [String])
parseOp ("turn" : "on" : rest) = (On, rest)
parseOp ("turn" : "off" : rest) = (Off, rest)
parseOp ("toggle" : rest) = (Toggle, rest)
parseOp _ = undefined

parseLine :: String -> (Op, Rect)
parseLine s =
  let (op, rest) = parseOp $ words s
   in case rest of
        (lb : _ : ru : _) -> (op, (parseCoord lb, parseCoord ru))
        _ -> undefined

step :: Lights -> (Op, Rect) -> Lights
step l (On, rect) = setInRect True l rect
step l (Off, rect) = setInRect False l rect
step l (Toggle, rect) = toggleInRect l rect

type LightsInten = Array (Int, Int) Int

updateInten :: LightsInten -> Rect -> Int -> LightsInten
updateInten l rect inten = l // [(c, max ((l ! c) + inten) 0) | c <- coordsInsideRect rect]

step2 :: LightsInten -> (Op, Rect) -> LightsInten
step2 l (On, rect) = updateInten l rect 1
step2 l (Off, rect) = updateInten l rect (-1)
step2 l (Toggle, rect) = updateInten l rect 2

initArrayInten :: LightsInten
initArrayInten = listArray ((0, 0), (999, 999)) (repeat 0)

part1 :: String -> Int
part1 = length . filter id . elems . foldl step initArray . map parseLine . lines

part2 :: String -> Int
part2 = sum . elems . foldl step2 initArrayInten . map parseLine . lines
