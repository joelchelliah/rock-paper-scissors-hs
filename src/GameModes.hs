module GameModes(GameMode(..), getGameMode, genGameMode, gameModeNames) where

import           RpsElements
import           System.Random

data GameMode = RPS
              | RPSLS
              | RPS_7
              | RPS_9
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
  show RPS_9  = "RPS-9"
  show RANDOM = "Random"

instance RpsElement GameMode


getGameMode :: String -> Either String GameMode
getGameMode input = case make input of
  Just gameMode -> Right gameMode
  Nothing       -> Left "Invalid game mode!"

genGameMode :: IO GameMode
genGameMode = genFrom (minBound, pred maxBound)

gameModeNames :: [String]
gameModeNames = let modes = allElems :: [GameMode]
                    strs  = show <$> modes
                    nums  = show . (+1) . fromEnum <$> modes
                in zipWith ((++) . (++ ". ")) nums strs
