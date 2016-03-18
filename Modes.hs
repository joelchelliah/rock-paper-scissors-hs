module Modes(GameMode(RPS, RPSLS, RPS_7), 
             getGameMode, 
             gameModeNames) where

import RpsElem

data GameMode = RPS 
              | RPSLS
              | RPS_7
              deriving (Eq, Enum, Bounded)

instance RpsElem GameMode

instance Show GameMode where
  show RPS = "Standard RPS"
  show RPSLS = "Rock-Paper-Scissors-Lizard-Spock"
  show RPS_7 = "RPS-7"

getGameMode :: IO (Maybe GameMode)
getGameMode = make <$> getLine

gameModeNames :: [String]
gameModeNames = let modes = allElems :: [GameMode]
                    strs  = show <$> modes
                    nums  = show . (+1) . fromEnum <$> modes
                in zipWith (\num name -> num ++ ". " ++ name) nums strs
