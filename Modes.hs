module Modes(GameMode(RPS, RPSLS), getGameMode) where

import RpsElem

data GameMode = RPS 
              | RPSLS
              deriving (Show, Eq, Enum, Bounded)

instance RpsElem GameMode

getGameMode :: IO (Maybe GameMode)
getGameMode = make allElems <$> getLine
