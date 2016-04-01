module GameModesSpec(spec) where

import           GameModes
import           Test.Hspec
import           Test.QuickCheck

newtype Input = Input { toStr :: String } deriving Show

instance Arbitrary Input where
  arbitrary = do
    i <- choose (0, 10)
    return . Input . show $ (i :: Int)

type IsValidInput = (Input -> Bool)
type GetGameModeFunc  = (Input -> Either String GameMode)
type ExpectedGameMode = (Input -> GameMode)


getGameModeProp :: IsValidInput -> GetGameModeFunc -> ExpectedGameMode -> Input -> Bool
getGameModeProp isValid getGameModeFunc expectedGameMode i
  | isValid i = getGameModeFunc i == Right (expectedGameMode i)
  | otherwise = getGameModeFunc i == Left  "Invalid game mode!"


spec :: IO ()
spec = hspec $
  describe "GameModes" $ do
    let gameModes = [minBound .. maxBound]

    describe "getGameMode" $ do
      let validInput = (`elem` (show <$> [1 .. length gameModes])) . toStr
          getGameModeFunc = getGameMode . toStr
          expectedGameMode = (gameModes !!) . subtract 1 . read . toStr

      it "gets a game mode based on given input" $ property $
        getGameModeProp validInput getGameModeFunc expectedGameMode

    describe "genGameMode" $ do
      it "generates a random game mode" $ do
        gameMode <- genGameMode
        gameMode `shouldSatisfy` (`elem` init gameModes)

      it "does not generate the 'RANDOM' game mode" $ do
        gameMode <- genGameMode
        gameMode `shouldSatisfy` (/= RANDOM)

    describe "gameModeNames" $
      it "produces a numbered list of all game modes" $
        gameModeNames `shouldBe` ["1. Standard RPS",
                                  "2. Rock-Paper-Scissors-Lizard-Spock",
                                  "3. RPS-7",
                                  "4. RPS-9",
                                  "5. Random"]
