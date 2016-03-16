module Modes(GameMode, getGameMode) where

type GameMode = String


getGameMode :: IO (Maybe GameMode)
getGameMode = do
  let make gm = if gm `elem` availableGameModes 
                then Just $ gm 
                else Nothing

  make <$> getLine


availableGameModes :: [GameMode]
availableGameModes = map show $ [1, 2]
