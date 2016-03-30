module WeaponsSpec(spec) where

import Test.Hspec
import Test.QuickCheck
import GameModes
import Weapons

newtype Input = Input { toStr :: String } deriving Show
type IsValidInput = (String -> Bool)

instance Arbitrary Input where
  arbitrary = do
    i <- choose (0, 50)
    return . Input . show $ (i :: Int)

rpsWeapons   = [Rock, Paper, Scissors]
rpslsWeapons = [Rock, Paper, Scissors, Lizard, Spock]
rps7Weapons  = [Rock, Paper, Scissors, Fire, Sponge, Air, Water]


get_weapon_prop :: Input -> IsValidInput -> (String -> Maybe Weapon) -> (String -> Weapon) -> Bool
get_weapon_prop (Input i) isValid getWpn expectedWeapon =
            if isValid i
            then getWpn i == Just (expectedWeapon i)
            else getWpn i == Nothing


spec :: IO ()
spec = hspec $ do
  describe "Weapons" $ do
    describe "getWeapon" $ do
      let validInput weapons = (`elem` (show <$> [1 .. length weapons]))

      context "when the game mode is Standard RPS" $ do
        let getWeaponFunc  = getWeapon RPS
            validInputFunc = validInput rpsWeapons
            expectedWeapon = (rpsWeapons !!) . subtract 1 . read

        it "gets a weapon based on given input" $ property $
          \i -> get_weapon_prop i validInputFunc getWeaponFunc expectedWeapon

      context "when the game mode is Rock-paper-scissors-lizard-spock" $ do
        let getWeaponFunc  = getWeapon RPSLS
            validInputFunc = validInput rpslsWeapons
            expectedWeapon = (rpslsWeapons !!) . subtract 1 . read

        it "gets a weapon based on given input" $ property $
          \i -> get_weapon_prop i validInputFunc getWeaponFunc expectedWeapon

      context "when the game mode is RPS-7" $ do
        let getWeaponFunc  = getWeapon RPS_7
            validInputFunc = validInput rps7Weapons
            expectedWeapon = (rps7Weapons !!) . subtract 1 . read

        it "gets a weapon based on given input" $ property $
          \i -> get_weapon_prop i validInputFunc getWeaponFunc expectedWeapon

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
