module ScoreBoardSpec(spec) where

import Test.Hspec
import Test.QuickCheck
import ScoreBoard

spec :: IO ()
spec = hspec $
  describe "ScoreBoard" $
    describe "initScore" $ do
      it "creates an empty score board" $ do
        pending
