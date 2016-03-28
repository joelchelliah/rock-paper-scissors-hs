import Test.Hspec
import Test.QuickCheck

import GameModes
import Weapons

main :: IO ()
main = hspec $ do
  describe "Weapons" $ do
    describe "getWeapon" $ do
      context "when the game mode is Standard RPS" $ do
        let get = getWeapon RPS

        it "generates a weapon for valid input" $ do
          get "1" `shouldBe` (Just Rock)
          get "2" `shouldBe` (Just Paper)
          get "3" `shouldBe` (Just Scissors)

        it "generates nothing for invalid input" $ do
          get "4" `shouldBe` Nothing

      context "when the game mode is Rock-paper-scissors-lizard-spock" $ do
        let get = getWeapon RPSLS

        it "generates a weapon for valid input" $ do
          get "1" `shouldBe` (Just Rock)
          get "2" `shouldBe` (Just Paper)
          get "3" `shouldBe` (Just Scissors)
          get "4" `shouldBe` (Just Lizard)
          get "5" `shouldBe` (Just Spock)

        it "generates nothing for invalid input" $ do
          get "6" `shouldBe` Nothing

      context "when the game mode is RPS-7" $ do
        let get = getWeapon RPS_7

        it "generates a weapon for valid input" $ do
          get "1" `shouldBe` (Just Rock)
          get "2" `shouldBe` (Just Paper)
          get "3" `shouldBe` (Just Scissors)
          get "4" `shouldBe` (Just Fire)
          get "5" `shouldBe` (Just Sponge)
          get "6" `shouldBe` (Just Air)
          get "7" `shouldBe` (Just Water)

        it "generates nothing for invalid input" $ do
          get "8" `shouldBe` Nothing

    describe "genWeapon" $ do
      it "generates a random weapon for Standard RPS" $ do
        weapon <- genWeapon RPS
        weapon `shouldSatisfy` (`elem` rpsWeapons)

      it "generates a random weapon for Rock-paper-scissors-lizard-spock" $ do
        weapon <- genWeapon RPSLS
        weapon `shouldSatisfy` (`elem` rpslsWeapons)

      it "generates a random weapon for RPS-7" $ do
        weapon <- genWeapon RPS_7
        weapon `shouldSatisfy` (`elem` rps7Weapons)

    describe "weaponsIn" $ do
      it "provides all available weapons for Standard RPS" $ do
        weaponsIn RPS `shouldBe` rpsWeapons

      it "provides all available weapons for Rock-paper-scissors-lizard-spock" $ do
        weaponsIn RPSLS `shouldBe` rpslsWeapons

      it "provides all available weapons for RPS-7" $ do
        weaponsIn RPS_7 `shouldBe` rps7Weapons


rpsWeapons   = [Rock, Paper, Scissors]
rpslsWeapons = [Rock, Paper, Scissors, Lizard, Spock]
rps7Weapons  = [Rock, Paper, Scissors, Fire, Sponge, Air, Water]
