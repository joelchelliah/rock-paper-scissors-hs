module Weapons where

import System.Random

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

genWeapon :: String -> IO Weapon
genWeapon gameMode = do
  newStdGen
  gen <- getStdGen
  let min   = Rock
      max   = if gameMode == "1" then Scissors else Spock
      (w,_) = randomR (min,max) gen :: (Weapon, StdGen)
  return w
