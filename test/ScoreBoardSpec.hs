module ScoreBoardSpec(spec) where

import Test.Hspec
import Test.QuickCheck
import ScoreBoard

instance Eq Score where
  (Score s1) == (Score s2) = s1 == s2

spec :: IO ()
spec = hspec $
  describe "ScoreBoard" $ do
    describe "initScore" $
      it "creates an empty score board" $
        initScore `shouldBe` Score (0,0,0)

    describe "updateScore" $ do
      let score = Score (0,0,0)

      it "updates the score board with a win" $
        updateScore LT score `shouldBe` Score (1,0,0)

      it "updates the score board with a tie" $
        updateScore EQ score `shouldBe` Score (0,1,0)

      it "updates the score board with a loss" $
        updateScore GT score `shouldBe` Score (0,0,1)
