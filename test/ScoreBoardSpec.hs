module ScoreBoardSpec(spec) where

import           ScoreBoard
import           Test.Hspec
import           Test.QuickCheck

instance Eq Score where
  (Score s1) == (Score s2) = s1 == s2

spec :: IO ()
spec = hspec $
  describe "ScoreBoard" $ do

    describe "initScore" $
      it "creates an empty score board" $
        initScore `shouldBe` [mempty]

    describe "updateScore" $ do
      let scoreBoard = [Score (5,5,5)]

      it "updates the score board with a win" $
        updateScore GT scoreBoard `shouldBe` Score (6,5,5) : scoreBoard

      it "updates the score board with a tie" $
        updateScore EQ scoreBoard `shouldBe` Score (5,6,5) : scoreBoard

      it "updates the score board with a loss" $
        updateScore LT scoreBoard `shouldBe` Score (5,5,6) : scoreBoard
