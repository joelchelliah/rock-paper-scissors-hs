module ScoreBoard (ScoreBoard, Score(..), initScore, updateScore, getScore) where

newtype Score = Score (Int, Int, Int)

instance Show Score where
  show (Score (wins, ties, loss)) = let wrap result = "[ " ++ result ++ " ] "
                                        display 1 s = wrap $ show 1 ++ " " ++ head s
                                        display p s = wrap $ show p ++ " " ++ last s
                                    in display wins ["win   ", "wins  "]
                                    ++ display ties ["tie   ", "ties  "]
                                    ++ display loss ["loss  ", "losses"]

instance Monoid Score where
  mempty = Score (0, 0, 0)
  Score (w1, t1, l1) `mappend` Score (w2, t2, l2) = Score (w1 + w2, t1 + t2, l1 + l2)

type ScoreBoard = [Score]


initScore :: ScoreBoard
initScore = [mempty]

updateScore :: Ordering -> ScoreBoard -> ScoreBoard
updateScore outcome board = let score' = update outcome . head $ board
                            in  score' : board

getScore :: ScoreBoard -> Score
getScore = head


update :: Ordering -> Score -> Score
update GT = mappend $ Score (1, 0, 0)
update EQ = mappend $ Score (0, 1, 0)
update LT = mappend $ Score (0, 0, 1)
