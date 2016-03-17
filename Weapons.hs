module Weapons (Weapon, genWeapon, getWeapon, weaponsIn) where

import System.Random
import RpsElem
import Modes

data Weapon = Rock
            | Paper
            | Scissors
            | Lizard
            | Spock
            deriving (Eq, Show, Bounded, Enum)


instance Ord Weapon where
   Rock `compare` Scissors = GT
   Scissors `compare` Rock = LT

   Paper `compare` Spock = GT
   Spock `compare` Paper = LT

   Lizard `compare` Spock = GT
   Spock `compare` Lizard = LT

   Rock `compare` Lizard = GT
   Lizard `compare` Rock = LT

   Scissors `compare` Lizard = GT
   Lizard `compare` Scissors = LT

   x `compare` y = fromEnum x `compare` fromEnum y


instance Random Weapon where
  random g = let min = fromEnum (minBound :: Weapon)
                 max = fromEnum (maxBound :: Weapon)
                 (r, g') = randomR (min, max) g
              in (toEnum r, g')

  randomR (min,max) g = let (r, g') = randomR (fromEnum min, fromEnum max) g 
                        in  (toEnum r, g')

instance RpsElem Weapon

genWeapon :: GameMode -> IO Weapon
genWeapon gameMode = do
  let max = if gameMode == RPS then Scissors else maxBound
      gen = fst . randomR (minBound, max)

  newStdGen >> gen <$> getStdGen

getWeapon :: GameMode -> IO (Maybe Weapon)
getWeapon gameMode = makeFrom (weaponsIn gameMode) <$> getLine

weaponsIn :: GameMode -> [Weapon]
weaponsIn gameMode = take (numWeapons gameMode) allElems
                     where numWeapons RPS = 3
                           numWeapons RPSLS = 5
