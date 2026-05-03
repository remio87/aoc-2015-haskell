module Day16 where

import Data.Maybe (fromJust)

main :: IO ()
main = do
  input <- readFile "inputs/day16.txt"
  putStrLn $ "part1: " ++ show (part1 input)
  putStrLn $ "part2: " ++ show (part2 input)

data Sue = Sue
  { children :: Maybe Int,
    cats :: Maybe Int,
    samoyeds :: Maybe Int,
    pomeranians :: Maybe Int,
    akitas :: Maybe Int,
    vizslas :: Maybe Int,
    goldfish :: Maybe Int,
    trees :: Maybe Int,
    cars :: Maybe Int,
    perfumes :: Maybe Int
  }
  deriving (Show)

props :: [(Sue -> Maybe Int)]
props =
  [ children,
    cats,
    samoyeds,
    pomeranians,
    akitas,
    vizslas,
    goldfish,
    trees,
    cars,
    perfumes
  ]

initSue :: Sue
initSue =
  Sue
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

target :: Sue
target =
  Sue
    (Just 3)
    (Just 7)
    (Just 2)
    (Just 3)
    (Just 0)
    (Just 0)
    (Just 5)
    (Just 3)
    (Just 2)
    (Just 1)

parseLine :: String -> (Int, Sue)
parseLine str = (sueNum, sue)
  where
    separated = (\(s, r) -> (s, drop 2 r)) $ break (== ':') str
    sueNum = read $ drop 4 $ fst separated
    sue = parseProp $ snd separated

parseProp :: String -> Sue
parseProp str = foldl updateSue initSue (parsePropToList str)

updateSue :: Sue -> (String, Int) -> Sue
updateSue sue ("children", i) = sue {children = Just i}
updateSue sue ("cats", i) = sue {cats = Just i}
updateSue sue ("samoyeds", i) = sue {samoyeds = Just i}
updateSue sue ("pomeranians", i) = sue {pomeranians = Just i}
updateSue sue ("akitas", i) = sue {akitas = Just i}
updateSue sue ("vizslas", i) = sue {vizslas = Just i}
updateSue sue ("goldfish", i) = sue {goldfish = Just i}
updateSue sue ("trees", i) = sue {trees = Just i}
updateSue sue ("cars", i) = sue {cars = Just i}
updateSue sue ("perfumes", i) = sue {perfumes = Just i}
updateSue _ _ = undefined

parsePropToList :: String -> [(String, Int)]
parsePropToList str = map parsePropStr $ splitProps str

parsePropStr :: String -> (String, Int)
parsePropStr str = (\(name, val) -> (name, read $ tail val)) $ break (== ':') str

splitProps :: String -> [String]
splitProps [] = []
splitProps str =
  let (f, rest) = splitComma str
   in (f : splitProps rest)

splitComma :: String -> (String, String)
splitComma str = dropCommaAndSpace $ break (== ',') str
  where
    dropCommaAndSpace (first, []) = (first, [])
    dropCommaAndSpace (first, rest) = (first, drop 2 rest)

checkSue :: Sue -> Bool
checkSue sue = all id $ map (checkProp sue) props
  where
    checkProp :: Sue -> (Sue -> Maybe Int) -> Bool
    checkProp s p = case p s of
      Nothing -> True
      Just pv -> (fromJust (p target)) == pv

propsWithCond :: [((Sue -> Maybe Int), (Int -> Int -> Bool))]
propsWithCond =
  [ (children, (==)),
    (cats, (>)),
    (samoyeds, (==)),
    (pomeranians, (<)),
    (akitas, (==)),
    (vizslas, (==)),
    (goldfish, (<)),
    (trees, (>)),
    (cars, (==)),
    (perfumes, (==))
  ]

checkSue2 :: Sue -> Bool
checkSue2 sue = all id $ map (checkProp sue) propsWithCond
  where
    checkProp :: Sue -> ((Sue -> Maybe Int), (Int -> Int -> Bool)) -> Bool
    checkProp s (p, cond) = case p s of
      Nothing -> True
      Just pv -> pv `cond` (fromJust (p target))

part1 :: String -> Int
part1 str = fst $ head $ filter (\(_, s) -> checkSue s) $ map parseLine $ lines str

part2 :: String -> Int
part2 str = fst $ head $ filter (\(_, s) -> checkSue2 s) $ map parseLine $ lines str