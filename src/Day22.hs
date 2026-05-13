module Day22 where
import Data.Maybe (maybeToList, listToMaybe, catMaybes)

main :: IO ()
main = do
  return ()

data Turn = Player | Boss

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

search :: GameState -> Maybe Int
search gs =
  if bossDied
    then Just (manaConsumed gs)
    else listToMaybe $ minimum $ catMaybes $ map search nextGs
  where
    applyEffect = undefined
    applied = applyEffect gs
    bossDied = bossHp applied <= 1
    nextGs :: [GameState]
    nextGs = undefined