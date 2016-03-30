module ScoreBoardSpec(spec) where

import Test.Hspec
import Test.QuickCheck
import ScoreBoard

spec :: IO ()
spec = hspec $ do
  describe "ScoreBoard" $ do
    describe "initScore" $ do
      it "creates an empty score board" $ do
        pending
