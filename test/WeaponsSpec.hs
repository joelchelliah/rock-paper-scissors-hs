module WeaponsSpec(spec) where

import Test.Hspec
import Test.QuickCheck
import Data.List((\\))
import GameModes
import Weapons

newtype Input = Input { toStr :: String } deriving Show

instance Arbitrary Input where
  arbitrary = do
    i <- choose (0, 50)
    return . Input . show $ (i :: Int)

type IsValidInput   = (Input -> Bool)
type GetWeaponFunc  = (Input -> Either String Weapon)
type ExpectedWeapon = (Input -> Weapon)


getWeaponProp :: IsValidInput -> GetWeaponFunc -> ExpectedWeapon -> Input -> Bool
getWeaponProp isValid getWeaponFunc expectedWeapon i
  | isValid i = getWeaponFunc i == Right (expectedWeapon i)
  | otherwise = getWeaponFunc i == Left  "Invalid weapon!"


spec :: IO ()
spec = hspec $
  describe "Weapons" $ do
    let rpsWeapons   = [Rock, Paper, Scissors]
        rpslsWeapons = [Rock, Paper, Scissors, Lizard, Spock]
        rps7Weapons  = [Rock, Paper, Scissors, Fire, Sponge, Air, Water]
        allWeapons   = [minBound .. maxBound]

    describe "getWeapon" $ do
      let validInputFor weapons = (`elem` (show <$> [1 .. length weapons])) . toStr
          weaponForGameMode gm  = getWeapon gm . toStr
          expectedFrom weapons  = (weapons !!) . subtract 1 . read . toStr

      context "when the game mode is Standard RPS" $ do
        let validInputFunc = validInputFor rpsWeapons
            getWeaponFunc  = weaponForGameMode RPS
            expectedWeapon = expectedFrom rpsWeapons

        it "gets a weapon based on given input" $ property $
          getWeaponProp validInputFunc getWeaponFunc expectedWeapon

      context "when the game mode is Rock-paper-scissors-lizard-spock" $ do
        let validInputFunc = validInputFor rpslsWeapons
            getWeaponFunc  = weaponForGameMode RPSLS
            expectedWeapon = expectedFrom rpslsWeapons

        it "gets a weapon based on given input" $ property $
          getWeaponProp validInputFunc getWeaponFunc expectedWeapon

      context "when the game mode is RPS-7" $ do
        let validInputFunc = validInputFor rps7Weapons
            getWeaponFunc  = weaponForGameMode RPS_7
            expectedWeapon = expectedFrom rps7Weapons

        it "gets a weapon based on given input" $ property $
          getWeaponProp validInputFunc getWeaponFunc expectedWeapon

    describe "genWeapon" $ do

      context "when the game mode is Standard RPS" $ do
        let gameMode = RPS
            validWeapons = rpsWeapons
            invalidWeapons = allWeapons \\ rpsWeapons

        it "generates a random weapon" $ do
          weapon <- genWeapon gameMode
          weapon `shouldSatisfy` (`elem` validWeapons)

        it "does not generate a weapon from another game mode" $ do
          weapon <- genWeapon gameMode
          weapon `shouldSatisfy` not . (`elem` invalidWeapons)

      context "when the game mode is Rock-paper-scissors-lizard-spock" $ do
        let gameMode = RPSLS
            validWeapons = rpslsWeapons
            invalidWeapons = allWeapons \\ rpslsWeapons

        it "generates a random weapon" $ do
          weapon <- genWeapon gameMode
          weapon `shouldSatisfy` (`elem` validWeapons)

        it "does not generate a weapon from another game mode" $ do
          weapon <- genWeapon gameMode
          weapon `shouldSatisfy` not . (`elem` invalidWeapons)

      context "when the game mode is Standard RPS-7" $ do
        let gameMode = RPS_7
            validWeapons = rps7Weapons
            invalidWeapons = allWeapons \\ rps7Weapons

        it "generates a random weapon" $ do
          weapon <- genWeapon gameMode
          weapon `shouldSatisfy` (`elem` validWeapons)

        it "does not generate a weapon from another game mode" $ do
          weapon <- genWeapon gameMode
          weapon `shouldSatisfy` not . (`elem` invalidWeapons)

    describe "weaponsIn" $ do
      it "provides all available weapons for Standard RPS" $
        weaponsIn RPS `shouldBe` rpsWeapons

      it "provides all available weapons for Rock-paper-scissors-lizard-spock" $
        weaponsIn RPSLS `shouldBe` rpslsWeapons

      it "provides all available weapons for RPS-7" $
        weaponsIn RPS_7 `shouldBe` rps7Weapons
