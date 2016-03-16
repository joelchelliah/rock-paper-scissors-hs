module Modes(GameMode(RPS, RPSLS), getGameMode) where

data GameMode = RPS 
              | RPSLS
              deriving (Show, Eq, Enum, Bounded)


getGameMode :: IO (Maybe GameMode)
getGameMode = do
  let validModes = fromGameMode <$> allGameModes
      make mdStr = if mdStr `elem` validModes
                   then Just $ toGameMode mdStr
                   else Nothing

  make <$> getLine


toGameMode :: String -> GameMode
toGameMode s = toEnum $ (read s :: Int) - 1

fromGameMode :: GameMode -> String
fromGameMode = show . (+1) . fromEnum

allGameModes :: [GameMode]
allGameModes = [minBound..maxBound]
