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


spec :: IO ()
spec = hspec $ do
  describe "GameModes" $ do
    describe "getGameMode" $ do
      it "gets the game mode for given input" $ do
        pending
