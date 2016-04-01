module RPS(rps) where

import           Control.Monad (when)
import           GameModes     (GameMode(RANDOM), genGameMode, getGameMode)
import           ScoreBoard
import           Weapons       (genWeapon, getWeapon)
import qualified Printer       as Print

rps = Print.header
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
retry score ans = when (ans == "y") $ play score

restart :: String -> Score -> IO ()
restart reason score = do
  putStrLn $ reason ++ " ... Try again"
  play score
