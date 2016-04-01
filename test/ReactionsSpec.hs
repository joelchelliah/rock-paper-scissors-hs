module ReactionsSpec(spec) where

import           Reactions
import           Test.Hspec
import           Test.QuickCheck
import           Weapons

spec :: IO ()
spec = hspec $
  describe "Reactions" $

    describe "getReaction" $ do
      it "gets the interaction between two weapons" $ do
        getReaction Paper Rock      `shouldBe` "PAPER COVERS ROCK"
        getReaction Scissors Lizard `shouldBe` "SCISSORS DECAPITATE LIZARD"
        getReaction Air Fire        `shouldBe` "AIR BLOWS OUT FIRE"
        getReaction Human Sponge    `shouldBe` "HUMAN CLEANS WITH SPONGE"

      it "gets the default interaction for similar weapons" $ do
        let defaultReaction = ". . ."

        getReaction Paper Paper `shouldBe` defaultReaction
        getReaction Spock Spock `shouldBe` defaultReaction
        getReaction Water Water `shouldBe` defaultReaction
