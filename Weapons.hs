module Weapons where

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
  newStdGen
  gen <- getStdGen
  let min   = Rock
      max   = if gameMode == "1" then Scissors else Spock
      (w,_) = randomR (min,max) gen :: (Weapon, StdGen)
  return w

getWeapon :: GameMode -> IO (Maybe Weapon)
getWeapon gameMode = do
  let choices = weaponChoices gameMode
  let weapons = availableWeapons gameMode

  weaponStr <- getLine
  return $ if weaponStr `elem` weapons
           then Just $ choices !! ((read weaponStr) - 1)
           else Nothing

weaponChoices :: GameMode -> [Weapon]
weaponChoices gameMode = take (length $ availableWeapons gameMode) 
                              [minBound :: Weapon .. maxBound :: Weapon]

availableWeapons :: GameMode -> [String]
availableWeapons gameMode = let allWeapons = map show $ [1, 2, 3, 4, 5]
                            in if gameMode == "1" 
                               then take 3 allWeapons 
                               else allWeapons
