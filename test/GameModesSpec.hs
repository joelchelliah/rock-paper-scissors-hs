module GameModesSpec(spec) where

import Test.Hspec
import Test.QuickCheck
import GameModes

newtype Input = Input { toStr :: String } deriving Show
type IsValidInput = (String -> Bool)

instance Arbitrary Input where
  arbitrary = do
    i <- choose (0, 10)
    return . Input . show $ (i :: Int)

gameModes = [RPS, RPSLS, RPS_7, RANDOM]


spec :: IO ()
spec = hspec $ do
  describe "GameModes" $ do

    describe "getGameMode" $ do
     let validInput = (`elem` (show <$> [1 .. length gameModes]))
         expectedGameMode = (gameModes !!) . subtract 1 . read

     it "gets the game mode for given input" $ property $
       \(Input i) -> if validInput i
                     then getGameMode i == Right (expectedGameMode i)
                     else getGameMode i == Left  ( "Invalid game mode!" )
