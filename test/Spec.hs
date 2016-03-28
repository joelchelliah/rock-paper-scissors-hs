import Test.Hspec
import Test.QuickCheck

import GameModes
import Weapons

main :: IO ()
main = hspec $ do
  describe "Weapons" $ do
    describe "genWeapon" $ do
      it "generates a random weapon for Standard RPS" $ do
        weapon <- genWeapon RPS
        weapon `shouldSatisfy` (`elem` rpsWeapons)

      it "generates a random weapon for Rock-paper-scissors-lizard-spock" $ do
        weapon <- genWeapon RPS
        weapon `shouldSatisfy` (`elem` rpslsWeapons)

      it "generates a random weapon for RPS-7" $ do
        weapon <- genWeapon RPS
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
