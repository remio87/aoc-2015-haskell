module Day17 where

import Data.Array (Array, array, (!), (//))

main :: IO ()
main = do
  return ()

containers :: [Int]
containers = [50, 44, 11, 49, 42, 46, 18, 32, 26, 40, 21, 7, 18, 43, 10, 47, 36, 24, 22, 40]

initDp :: Array (Int, Int) Int
initDp = array ((0, 0), (20, 150)) [((i, j), if (i, j) == (0, 0) then 1 else 0) | i <- [0 .. 20], j <- [0 .. 150]]

update :: Array (Int, Int) Int -> (Int, Int) -> Array (Int, Int) Int
update dp (i, c) =
  array
    ((0, 0), (20, 150))
    [ ( (i', j),
        if i /= i'
          then dp ! (i', j)
          else
            if j < c
              then dp ! (i' - 1, j)
              else (dp ! (i' - 1, j)) + (dp ! (i' - 1, j - c))
      )
    | i' <- [0 .. 20],
      j <- [0 .. 150]
    ]

updateAll :: Array (Int, Int) Int
updateAll = foldl update initDp (zip [1 ..] containers)

part1 :: Int
part1 = updateAll ! (20, 150)