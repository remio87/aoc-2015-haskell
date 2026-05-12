module Day21 where

main :: IO ()
main = do
  putStrLn $ "part1: " ++ show part1
  putStrLn $ "part2: " ++ show part2

playerHp :: Int
playerHp = 100

bossHp :: Int
bossHp = 100

bossDamage :: Int
bossDamage = 8

bossArmor :: Int
bossArmor = 2

data Weapon = Weapon Int Int Int
  deriving (Show)

data Armor = Armor Int Int Int
  deriving (Show)

data Ring = Ring Int Int Int
  deriving (Show)

weapons :: [Weapon]
weapons =
  [ Weapon 8 4 0,
    Weapon 10 5 0,
    Weapon 25 6 0,
    Weapon 40 7 0,
    Weapon 74 8 0
  ]

armors :: [Armor]
armors =
  [ Armor 13 0 1,
    Armor 31 0 2,
    Armor 53 0 3,
    Armor 75 0 4,
    Armor 102 0 5
  ]

armorsMaybe :: [Maybe Armor]
armorsMaybe = [Nothing] ++ (map Just armors)

unwrapArmor :: Maybe Armor -> Armor
unwrapArmor a = maybe (Armor 0 0 0) id a

rings :: [Ring]
rings =
  [ Ring 25 1 0,
    Ring 50 2 0,
    Ring 100 3 0,
    Ring 20 0 1,
    Ring 40 0 2,
    Ring 80 0 3
  ]

ringsMaybe :: [Maybe Ring]
ringsMaybe = [Nothing] ++ (map Just rings)

pickTwo :: [a] -> [(a, a)]
pickTwo [] = []
pickTwo (a : rest) = [(a, b) | b <- rest] ++ pickTwo rest

ringsWithNone :: [(Maybe Ring, Maybe Ring)]
ringsWithNone = [(Nothing, Nothing)] ++ pickTwo ringsMaybe

unwrapRing :: Maybe Ring -> Ring
unwrapRing r = maybe (Ring 0 0 0) id r

data Equipment = Equipment Weapon (Maybe Armor) (Maybe Ring) (Maybe Ring)
  deriving (Show)

allEquip :: [Equipment]
allEquip = [Equipment w a r1 r2 | w <- weapons, a <- armorsMaybe, (r1, r2) <- ringsWithNone]

equipSpec :: Equipment -> (Int, Int, Int)
equipSpec (Equipment w ma mr1 mr2) = (cost, damage, armor)
  where
    Weapon wc wd wa = w
    Armor ac ad aa = unwrapArmor ma
    Ring rc1 rd1 ra1 = unwrapRing mr1
    Ring rc2 rd2 ra2 = unwrapRing mr2
    cost = wc + ac + rc1 + rc2
    damage = wd + ad + rd1 + rd2
    armor = wa + aa + ra1 + ra2

isWin :: Equipment -> Bool
isWin e = damageToBoss * turn > damageToPlayer * (turn - 1)
  where
    (_, damage, armor) = equipSpec e
    damageToBoss = max (damage - bossArmor) 1
    damageToPlayer = max (bossDamage - armor) 1
    turn = (playerHp + damageToPlayer - 1) `div` damageToPlayer

part1 :: Int
part1 = minimum $ map (\(c, _, _) -> c) $ map equipSpec $ filter isWin allEquip

part2 :: Int
part2 = maximum $ map (\(c, _, _) -> c) $ map equipSpec $ filter (not . isWin) allEquip