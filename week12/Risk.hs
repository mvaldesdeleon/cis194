{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Data.List
import Control.Applicative
import Control.Monad.Random

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
    deriving Show

effective :: Battlefield -> Battlefield
effective b = Battlefield (min 3 . subtract 1 . attackers $ b) (min 2 . defenders $ b)

data BattlefieldDice = BattlefieldDice { attackerDice :: [DieValue], defenderDice :: [DieValue] }

roll :: Battlefield -> Rand StdGen (Battlefield, BattlefieldDice)
roll b = replicateM (attackers eb) die >>= \dva ->
         replicateM (defenders eb) die >>= \dvd ->
         return (b, BattlefieldDice dva dvd)
         where
            eb = effective b

resolve :: (Battlefield, BattlefieldDice) -> Battlefield
resolve (b, bd) = Battlefield a d
    where
        pairs = liftA2 zip (reverse . sort . attackerDice) (reverse . sort . defenderDice) bd
        win (a, d) = a > d
        a = attackers b - (length . filter win $ pairs)
        d = defenders b - (length . filter (not . win) $ pairs)

battle :: Battlefield -> Rand StdGen Battlefield
battle = fmap resolve . roll

invade :: Battlefield -> Rand StdGen Battlefield
invade b =
    battle b >>= \b ->
    if finalized b
        then return b
        else invade b
    where
        finalized b = (defenders b) == 0 || (attackers b) < 2

successProb' :: [Battlefield] -> Double
successProb' bs = liftA2 (/) (fromIntegral . length . filter victory) (fromIntegral . length) bs
    where
        victory b = (defenders b) == 0

successProb :: Battlefield -> Rand StdGen Double
successProb = fmap successProb' . mapM invade . replicate 1000
