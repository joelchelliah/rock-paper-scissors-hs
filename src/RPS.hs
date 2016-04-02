module RPS(rps) where

import           GameModes     (GameMode(RANDOM), genGameMode, getGameMode)
import           Weapons       (genWeapon, getWeapon)
import qualified Printer       as Print
import           ScoreBoard    (ScoreBoard, initScore, updateScore, getScore)

rps :: IO ()
rps = Print.header
   >> play initScore
  >>= Print.scoreLog
   >> Print.footer


play :: ScoreBoard -> IO ScoreBoard
play scoreBoard = do
  Print.modeSelection

  gmFromInput <- getGameMode <$> getLine

  case gmFromInput of
    Left  errorMsg -> restart scoreBoard errorMsg
    Right gameMode -> do
      gm <- if gameMode == RANDOM then genGameMode else return gameMode

      Print.weaponsSelection gm

      maybeYourWeapon <- getWeapon gm <$> getLine
      opponentsWeapon <- genWeapon gm

      case maybeYourWeapon of
        Left  errorMsg   -> restart scoreBoard errorMsg
        Right yourWeapon -> do
          let outcome  = yourWeapon `compare` opponentsWeapon
              newBoard = updateScore outcome scoreBoard

          Print.battleSequence yourWeapon opponentsWeapon outcome
          Print.score $ getScore newBoard

          getLine >>= retry newBoard

retry :: ScoreBoard -> String -> IO ScoreBoard
retry scoreBoard ans = if ans == "y"
                       then play scoreBoard
                       else return scoreBoard

restart :: ScoreBoard -> String -> IO ScoreBoard
restart scoreBoard msg = do
  putStrLn $ msg ++ " ... Try again"
  play scoreBoard
