import qualified GameModesSpec as GameModes
import qualified WeaponsSpec as Weapons
import qualified ScoreBoardSpec as ScoreBoard

main :: IO ()
main = do
  GameModes.spec
  Weapons.spec
  ScoreBoard.spec
