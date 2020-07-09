{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List
------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show)



matchRoll :: (DieValue, DieValue) -> (Int, Int)
matchRoll (attacker, defender)
  | attacker > defender = (1, 0)
  | otherwise = (0, 1)

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield attackers defenders) = do
  let attackerTroops = attackers -1
  let defenderTroops = max 2 defenders
  aRolls <- replicateM attackerTroops die
  bRolls <- replicateM defenderTroops die
  let aSorted = sort aRolls
  let bSorted = sort bRolls
  let zipped =  zip aRolls bRolls
  let winLosses = map matchRoll zipped
  let attackerLost = sum $ map fst winLosses
  let defenderLost = sum $ map snd winLosses
  return $ Battlefield (attackerTroops - attackerLost) (defenderTroops - defenderLost)

invade :: Battlefield -> Rand StdGen Battlefield
invade battlefield = do
  attempt <- battle battlefield
  let remainingDefender = defenders attempt
  let remainingAttacker = attackers attempt
  if remainingDefender == 0 || remainingAttacker == 0
    then return attempt else invade attempt

successProb :: Battlefield -> Rand StdGen Double
successProb battlefield = do
  let numExperiments = 1000
  experiments <- replicateM numExperiments (invade battlefield)
  let successful = filter (\b -> defenders b == 0) experiments
  return $ fromIntegral (length successful) / fromIntegral numExperiments

-- driver (test) code
main = do
  values <- evalRandIO $ successProb (Battlefield 1000 1000)
  print values
