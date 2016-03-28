import GameModes(getGameMode)
import Weapons(Weapon, getWeapon, genWeapon)
import qualified Printer as Print
import ScoreBoard

main = Print.header 
    >> play initScore
    >> Print.footer


play :: Score -> IO ()
play score = do
  Print.modeSelection

  maybeGameMode <- getGameMode

  case maybeGameMode of
    Nothing -> restart "Invalid game mode!" score
    (Just gameMode) -> do
      Print.weaponsSelection gameMode

      maybeYourWeapon <- getWeapon gameMode
      opponentsWeapon <- genWeapon gameMode

      case maybeYourWeapon of
        Nothing -> restart "Invalid weapon!" score
        (Just yourWeapon) -> do
          let outcome  = yourWeapon `compare` opponentsWeapon
              newScore = updateScore outcome score

          Print.battleSequence yourWeapon opponentsWeapon outcome
          Print.score newScore

          getLine >>= retry newScore

retry :: Score -> String -> IO ()
retry score ans = if ans == "y"
                  then play score
                  else return ()

restart :: String -> Score -> IO ()
restart reason score = do 
  putStrLn $ reason ++ " ... Try again"
  play score
