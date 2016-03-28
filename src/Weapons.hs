module Weapons (Weapon(..), genWeapon, getWeapon, weaponsIn) where

import System.Random
import RpsElements
import GameModes

data Weapon = Rock
            | Paper
            | Scissors
            | Fire
            | Sponge
            | Air
            | Water
            | Lizard
            | Spock
            deriving (Eq, Show, Enum, Bounded)


instance Ord Weapon where
  Rock `compare` Scissors = GT
  Rock `compare` Fire     = GT
  Rock `compare` Sponge   = GT
  Rock `compare` Lizard   = GT

  Paper `compare` Air   = GT
  Paper `compare` Water = GT
  Paper `compare` Spock = GT

  Scissors `compare` Sponge = GT
  Scissors `compare` Air    = GT
  Scissors `compare` Lizard = GT
  Scissors `compare` Rock   = LT

  Fire `compare` Sponge = GT
  Fire `compare` Rock   = LT
  
  Sponge `compare` Air       = GT
  Sponge `compare` Water     = GT
  Sponge `compare` Rock      = LT
  Sponge `compare` Fire      = LT
  Sponge `compare` Scissors  = LT

  Air `compare` Water       = GT
  Air `compare` Scissors    = LT
  Air `compare` Sponge      = LT
  Air `compare` Paper       = LT

  Water `compare` Sponge = LT
  Water `compare` Paper  = LT
  Water `compare` Air    = LT

  Lizard `compare` Spock    = GT
  Lizard `compare` Rock     = LT
  Lizard `compare` Scissors = LT

  Spock `compare` Paper = LT
  Spock `compare` Lizard = LT

  x `compare` y = fromEnum x `compare` fromEnum y


instance Random Weapon where
  random g = let min = fromEnum (minBound :: Weapon)
                 max = fromEnum (maxBound :: Weapon)
                 (r, g') = randomR (min, max) g
              in (toEnum r, g')

  randomR (min,max) g = let (r, g') = randomR (fromEnum min, fromEnum max) g 
                        in  (toEnum r, g')

instance RpsElement Weapon

genWeapon :: GameMode -> IO Weapon
genWeapon gameMode = do
  let max = (subtract 1) . numWeapons $ gameMode
      wps = weaponsIn gameMode
      gen = (wps !!) . fst . randomR (0, max)

  newStdGen >> gen <$> getStdGen
  
getWeapon :: GameMode -> String -> (Maybe Weapon)
getWeapon gameMode = makeFrom (weaponsIn gameMode)

weaponsIn :: GameMode -> [Weapon]
weaponsIn RPSLS = (take 3 allElems) ++ (reverse . take 2 . reverse $ allElems)
weaponsIn gameMode = take (numWeapons gameMode) allElems


numWeapons :: GameMode -> Int
numWeapons RPS = 3
numWeapons RPSLS = 5
numWeapons RPS_7 = 7
