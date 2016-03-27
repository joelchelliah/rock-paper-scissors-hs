module GameModes(GameMode(..), 
             getGameMode,
             genGameMode,
             gameModeNames) where

import System.Random
import RpsElements

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

instance RpsElement GameMode


getGameMode :: IO (Maybe GameMode)
getGameMode = do
  gm <- make <$> getLine
  
  case gm of 
    (Just RANDOM) -> Just <$> genGameMode
    _             -> return gm

genGameMode :: IO GameMode
genGameMode = genFrom (minBound, pred maxBound)

gameModeNames :: [String]
gameModeNames = let modes = allElems :: [GameMode]
                    strs  = show <$> modes
                    nums  = show . (+1) . fromEnum <$> modes
                in zipWith ((++) . (++ ". ")) nums strs