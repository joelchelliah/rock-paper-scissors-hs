module ScoreBoard where

newtype Score = Score (Int, Int, Int)

instance Show Score where
  show (Score (wins, ties, loss)) = let wrap result = "[ " ++ result ++ " ] "
                                        display 1 s = wrap $ show 1 ++ " " ++ (head . words) s
                                        display p s = wrap $ show p ++ " " ++ (last . words) s
                                    in (display wins "win wins")
                                    ++ (display ties "tie ties")
                                    ++ (display loss "loss losses")

instance Monoid Score where
  mempty = Score (0, 0, 0)
  Score (w1, t1, l1) `mappend` Score (w2, t2, l2) = Score (w1 + w2, t1 + t2, l1 + l2)


initScore :: Score
initScore = mempty

updateScore :: Ordering -> Score -> Score
updateScore LT = mappend $ Score (1, 0, 0)
updateScore EQ = mappend $ Score (0, 1, 0)
updateScore GT = mappend $ Score (0, 0, 1)
