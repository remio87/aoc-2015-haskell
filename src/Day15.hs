module Day15 where

main :: IO ()
main = do
  putStrLn $ "part1: " ++ show (part1)

data Ingredient = Ingredient
  { capacity :: Int,
    durability :: Int,
    flavor :: Int,
    texture :: Int,
    calories :: Int
  }

sprinkles :: Ingredient
sprinkles = Ingredient 2 0 (-2) 0 3

butterscotch :: Ingredient
butterscotch = Ingredient 0 5 (-3) 0 3

chocolate :: Ingredient
chocolate = Ingredient 0 0 5 (-1) 8

candy :: Ingredient
candy = Ingredient 0 (-1) 0 5 8

calcProp :: (Ingredient -> Int) -> (Int, Int, Int, Int) -> Int
calcProp prop (s, b, ch, ca) = if total < 0 then 0 else total
  where
    total = (prop sprinkles) * s + (prop butterscotch) * b + (prop chocolate) * ch + (prop candy) * ca

calcTotalScore :: (Int, Int, Int, Int) -> Int
calcTotalScore amount = product $ map (flip calcProp amount) [capacity, durability, flavor, texture]

genAllCombination :: [(Int, Int, Int, Int)]
genAllCombination = [(a, b, c, d) | a <- [0 .. 100], b <- [0 .. 100 - a], c <- [0 .. 100 - (a + b)], let d = 100 - (a + b + c)]

part1 :: Int
part1 = maximum $ map calcTotalScore genAllCombination