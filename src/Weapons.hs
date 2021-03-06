module Weapons (Weapon(..), genWeapon, getWeapon, weaponsIn) where

import           GameModes
import           RpsElements
import           System.Random

data Weapon = Rock
            | Paper
            | Scissors
            | Fire
            | Sponge
            | Air
            | Water
            | Human
            | Gun
            | Lizard
            | Spock
            deriving (Eq, Show, Enum, Bounded)

instance Ord Weapon where
  Rock `compare` Scissors = GT
  Rock `compare` Fire     = GT
  Rock `compare` Sponge   = GT
  Rock `compare` Lizard   = GT
  Rock `compare` Human    = GT

  Paper `compare` Air   = GT
  Paper `compare` Water = GT
  Paper `compare` Spock = GT
  Paper `compare` Gun   = GT

  Scissors `compare` Sponge = GT
  Scissors `compare` Air    = GT
  Scissors `compare` Lizard = GT
  Scissors `compare` Human  = GT
  Scissors `compare` Rock   = LT

  Fire `compare` Sponge = GT
  Fire `compare` Human  = GT
  Fire `compare` Rock   = LT

  Sponge `compare` Air       = GT
  Sponge `compare` Water     = GT
  Sponge `compare` Gun       = GT
  Sponge `compare` Rock      = LT
  Sponge `compare` Fire      = LT
  Sponge `compare` Scissors  = LT

  Air `compare` Water       = GT
  Air `compare` Gun         = GT
  Air `compare` Scissors    = LT
  Air `compare` Sponge      = LT
  Air `compare` Paper       = LT

  Water `compare` Gun    = GT
  Water `compare` Sponge = LT
  Water `compare` Paper  = LT
  Water `compare` Air    = LT

  Human `compare` Rock     = LT
  Human `compare` Fire     = LT
  Human `compare` Scissors = LT

  Gun `compare` Sponge = LT
  Gun `compare` Paper  = LT
  Gun `compare` Air    = LT
  Gun `compare` Water  = LT

  Lizard `compare` Spock    = GT
  Lizard `compare` Rock     = LT
  Lizard `compare` Scissors = LT

  Spock `compare` Paper  = LT
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
  let max = subtract 1 . numWeapons $ gameMode
      wps = weaponsIn gameMode
      gen = (wps !!) . fst . randomR (0, max)

  newStdGen >> gen <$> getStdGen

getWeapon :: GameMode -> String -> Either String Weapon
getWeapon gameMode input = case makeFrom (weaponsIn gameMode) input of
  Just wpn -> Right wpn
  Nothing  -> Left "Invalid weapon!"

weaponsIn :: GameMode -> [Weapon]
weaponsIn RPSLS = take 3 allElems ++ (reverse . take 2 . reverse $ allElems)
weaponsIn gameMode = take (numWeapons gameMode) allElems


numWeapons :: GameMode -> Int
numWeapons RPS = 3
numWeapons RPSLS = 5
numWeapons RPS_7 = 7
numWeapons RPS_9 = 9
