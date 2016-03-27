import Control.Monad(when)
import GameModes(getGameMode)
import Weapons(Weapon, getWeapon, genWeapon)
import qualified Printer as Print

main = do
  Print.header
  Print.modeSelection

  maybeGameMode <- getGameMode

  case maybeGameMode of
    Nothing -> restart "Invalid game mode!"
    (Just gameMode) -> do
      Print.weaponsSelection gameMode

      maybeYourWeapon <- getWeapon gameMode
      opponentsWeapon <- genWeapon gameMode

      case maybeYourWeapon of
        Nothing -> restart "Invalid weapon!"
        (Just yourWeapon) -> do
          Print.battleSequence yourWeapon opponentsWeapon

          getLine >>= retry


retry :: String -> IO()
retry ans = when (ans == "y") main

restart :: String -> IO ()
restart reason = do putStrLn $ reason ++ " ... Try again"
                    main
