import Modes(getGameMode)
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

          retry <- getLine
          if retry == "y" then main else return ()


restart :: String -> IO ()
restart reason = if null reason
                 then main
                 else do 
                    putStrLn $ reason ++ " ... Try again"
                    main
