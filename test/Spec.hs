import qualified GameModesSpec  as GameModes
import qualified ReactionsSpec  as Reactions
import qualified ScoreBoardSpec as ScoreBoard
import qualified WeaponsSpec    as Weapons

main :: IO ()
main = do
  GameModes.spec
  Reactions.spec
  ScoreBoard.spec
  Weapons.spec
