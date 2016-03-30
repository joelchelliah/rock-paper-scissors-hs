module RPS(runRps) where

import GameModes(GameMode(RANDOM), getGameMode, genGameMode)
import Weapons(getWeapon, genWeapon)
import qualified Printer as Print
import ScoreBoard

runRps = Print.header 
      >> play initScore
      >> Print.footer


play :: Score -> IO ()
play score = do
  Print.modeSelection

  gmFromInput <- getGameMode <$> getLine

  case gmFromInput of
    Left  errorMsg -> restart errorMsg score
    Right gameMode -> do
      gm <- if gameMode == RANDOM then genGameMode else return gameMode

      Print.weaponsSelection gm

      maybeYourWeapon <- getWeapon gm <$> getLine
      opponentsWeapon <- genWeapon gm

      case maybeYourWeapon of
        Left  errorMsg   -> restart errorMsg score
        Right yourWeapon -> do
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
