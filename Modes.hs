module Modes where

type GameMode = String

getGameMode :: IO (Maybe GameMode)
getGameMode = do
  gameMode <- getLine

  return (if gameMode `elem` availableGameModes 
          then Just $ gameMode
          else Nothing
         )

availableGameModes :: [GameMode]
availableGameModes = map show $ [1, 2]
