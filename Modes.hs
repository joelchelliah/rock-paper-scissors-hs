module Modes(GameMode(..), 
             getGameMode,
             genGameMode,
             gameModeNames) where

import System.Random
import RpsElem

data GameMode = RPS 
              | RPSLS
              | RPS_7
              | RANDOM
              deriving (Eq, Enum, Bounded)

instance Random GameMode where
  random g = let min = minBound :: GameMode
                 max = maxBound :: GameMode
                 (r, g') = randomR (fromEnum min, fromEnum max) g
             in (toEnum r, g')

  randomR (min,max) g = let (r, g') = randomR (fromEnum min, fromEnum max) g 
                        in  (toEnum r, g')

instance Show GameMode where
  show RPS    = "Standard RPS"
  show RPSLS  = "Rock-Paper-Scissors-Lizard-Spock"
  show RPS_7  = "RPS-7"
  show RANDOM = "Random"

instance RpsElem GameMode


getGameMode :: IO (Maybe GameMode)
getGameMode = make <$> getLine

genGameMode :: IO GameMode
genGameMode = gen

gameModeNames :: [String]
gameModeNames = let modes = allElems :: [GameMode]
                    strs  = show <$> modes
                    nums  = show . (+1) . fromEnum <$> modes
                in zipWith ((++) . (++ ". ")) nums strs
