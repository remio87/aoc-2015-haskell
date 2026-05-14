{-# LANGUAGE PatternGuards #-}

module Day22 where

import Control.Applicative ((<|>))
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (catMaybes)

main :: IO ()
main = do
  putStrLn $ "part1: " ++ show (part1)

initBossHp :: Int
initBossHp = 55

bossDamage :: Int
bossDamage = 8

data Turn = Player | Boss
  deriving (Eq)

data GameState = GameState
  { turn :: Turn,
    manaConsumed :: Int,
    playerHp :: Int,
    bossHp :: Int,
    manaRemaining :: Int,
    turnForShield :: Int,
    turnForPoison :: Int,
    turnForRecharge :: Int
  }

initialState :: GameState
initialState =
  GameState
    { turn = Player,
      manaConsumed = 0,
      playerHp = 50,
      bossHp = initBossHp,
      manaRemaining = 500,
      turnForShield = 0,
      turnForPoison = 0,
      turnForRecharge = 0
    }

search :: GameState -> Maybe Int -> Maybe Int
search gs bound
  | maybe False (manaConsumed gs >=) bound = Nothing
  | playerDied = Nothing
  | bossDied = Just (manaConsumed gs)
  | otherwise = foldl step' Nothing nextGs
  where
    applied = applyEffect gs
    bossDied = bossHp applied <= 0
    playerDied = playerHp applied <= 0
    nextGs = step applied
    step' best gs' =
      let result = search gs' (best <|> bound)
       in case (best, result) of
            (Nothing, _) -> result
            (_, Nothing) -> best
            (Just b, Just r) -> Just (min b r)

applyEffect :: GameState -> GameState
applyEffect = decrementTurns . applyRecharge . applyPoison

decrementTurns :: GameState -> GameState
decrementTurns gs =
  gs
    { turnForShield = max ((turnForShield gs) - 1) 0,
      turnForPoison = max ((turnForPoison gs) - 1) 0,
      turnForRecharge = max ((turnForRecharge gs) - 1) 0
    }

applyPoison :: GameState -> GameState
applyPoison gs
  | turnForPoison gs > 0 = gs {bossHp = (bossHp gs) - poisonDamage}
  | otherwise = gs
  where
    poisonDamage = 3

applyRecharge :: GameState -> GameState
applyRecharge gs
  | turnForRecharge gs > 0 = gs {manaRemaining = (manaRemaining gs) + rechargeMana}
  | otherwise = gs
  where
    rechargeMana = 101

applyBossAttack :: GameState -> GameState
applyBossAttack gs = gs {playerHp = (playerHp gs) - damage}
  where
    playerArmor
      | turnForShield gs > 0 = 7
      | otherwise = 0
    damage = max (bossDamage - playerArmor) 1

switchTurn :: GameState -> GameState
switchTurn gs
  | turn gs == Boss = gs {turn = Player}
  | turn gs == Player = gs {turn = Boss}
  | otherwise = undefined

step :: GameState -> [GameState]
step gs
  | turn gs == Boss = [switchTurn $ applyBossAttack gs]
  | turn gs == Player = map switchTurn $ catMaybes $ map ($ gs) casts
  | otherwise = undefined
  where
    casts = [castMagicMissile, castDrain, castShield, castPoison, castRecharge]

consumeMana :: GameState -> Int -> GameState
consumeMana gs mana =
  gs
    { manaConsumed = (manaConsumed gs) + mana,
      manaRemaining = (manaRemaining gs) - mana
    }

castMagicMissile :: GameState -> Maybe GameState
castMagicMissile gs
  | turn gs /= Player = error "must Player turn"
  | manaRemaining gs < cost = Nothing
  | otherwise =
      Just $
        gs'
          { bossHp = (bossHp gs') - damage
          }
  where
    gs' = consumeMana gs cost
    cost = 53
    damage = 4

castDrain :: GameState -> Maybe GameState
castDrain gs
  | turn gs /= Player = error "must Player turn"
  | manaRemaining gs < cost = Nothing
  | otherwise =
      Just $
        gs'
          { bossHp = (bossHp gs') - drain,
            playerHp = (playerHp gs') + drain
          }
  where
    gs' = consumeMana gs cost
    cost = 73
    drain = 2

castShield :: GameState -> Maybe GameState
castShield gs
  | turn gs /= Player = error "must Player turn"
  | manaRemaining gs < cost = Nothing
  | turnForShield gs > 0 = Nothing
  | otherwise =
      Just $
        gs'
          { turnForShield = turns
          }
  where
    gs' = consumeMana gs cost
    cost = 113
    turns = 6

castPoison :: GameState -> Maybe GameState
castPoison gs
  | turn gs /= Player = error "must Player turn"
  | manaRemaining gs < cost = Nothing
  | turnForPoison gs > 0 = Nothing
  | otherwise =
      Just $
        gs'
          { turnForPoison = turns
          }
  where
    gs' = consumeMana gs cost
    cost = 173
    turns = 6

castRecharge :: GameState -> Maybe GameState
castRecharge gs
  | turn gs /= Player = error "must Player turn"
  | manaRemaining gs < cost = Nothing
  | turnForRecharge gs > 0 = Nothing
  | otherwise =
      Just $
        gs'
          { turnForRecharge = turns
          }
  where
    gs' = consumeMana gs cost
    cost = 229
    turns = 5

part1 :: Maybe Int
part1 = search initialState Nothing