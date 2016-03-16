module Weapons (Weapon, genWeapon, getWeapon, weaponChoices) where

import System.Random
import Modes(GameMode)

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


genWeapon :: GameMode -> IO Weapon
genWeapon gameMode = do
  let maxBound' = if gameMode == "1" then Scissors else maxBound
      gen = fst . randomR (minBound, maxBound')

  newStdGen >> gen <$> getStdGen

getWeapon :: GameMode -> IO (Maybe Weapon)
getWeapon gameMode = do
  let choices = weaponChoices gameMode
      weapons = validNumbers gameMode
      make wp = if wp `elem` weapons
                then Just $ choices !! ((read wp) - 1)
                else Nothing

  make <$> getLine

weaponChoices :: GameMode -> [Weapon]
weaponChoices gameMode = let num = length $ validNumbers gameMode
                         in take num allWeapons


allWeapons :: [Weapon]
allWeapons = [minBound .. maxBound]

validNumbers :: GameMode -> [String]
validNumbers gameMode = let allNumbers = map show $ [1, 2, 3, 4, 5]
                        in if gameMode == "1" 
                           then take 3 allNumbers 
                           else allNumbers
