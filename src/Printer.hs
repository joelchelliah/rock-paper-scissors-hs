module Printer(header, footer, modeSelection, weaponsSelection, battleSequence,
               score, scoreLog) where

import           GameModes  (GameMode, gameModeNames)
import           Reactions  (getReaction)
import           ScoreBoard (ScoreBoard, Score)
import           Weapons    (Weapon, weaponsIn)

header :: IO ()
header = printText ["########  ########   ####### ",
                    "##     ## ##     ## ###    ##",
                    "########  ########   ######  ",
                    "##    ##  ##       ##    ### ",
                    "##     ## ##        #######  "]

footer :: IO ()
footer = printText ["- Game over -"]

modeSelection :: IO ()
modeSelection = let formatted = indent . padRight <$> gameModeNames
                in printText $ "- Choose a game mode -"
                             : divider
                             : formatted

weaponsSelection :: GameMode -> IO ()
weaponsSelection gameMode = let numberWeapon wpn num = show num ++ ". " ++ show wpn
                                weapons   = weaponsIn gameMode
                                numbered  = zipWith numberWeapon weapons [1..]
                                formatted = indent . padRight <$> numbered
                            in printText $ ("<> " ++ show gameMode ++ " <>")
                                         : divider
                                         : "- Choose your weapon -"
                                         : divider
                                         : formatted

battleSequence :: Weapon -> Weapon -> Ordering -> IO ()
battleSequence w1 w2 outcome = printText ["You pick:         " ++ show w1 ++ "!",
                                          "Your enemy picks: " ++ show w2 ++ "!",
                                          "",
                                          getReaction w1 w2,
                                          eval outcome]

score :: Score -> IO ()
score s = printText $ "- Current score -"
                    : ""
                    : show s
                    : divider
                    : ""
                    : ["Play again? (y/n)"]

scoreLog :: ScoreBoard -> IO ()
scoreLog scores = let reversed   = tail . reverse $ scores
                      numLog s n = "Round" ++ space n ++ show n ++ ": " ++ show s
                      numbered   = zipWith numLog reversed [1..]
                      formatted  = indent . padRight <$> numbered
                  in printText $ "- Score log -"
                               : ""
                               : formatted

eval :: Ordering -> String
eval GT = "YOU WIN!"
eval EQ = "IT'S A TIE!"
eval LT = "YOU LOSE!"

printText :: [String] -> IO ()
printText txt = let formatted = "" : divider2 : txt ++ [divider]
                in  mapM_ putStrLn $ padCenter <$> formatted

padCenter :: String -> String
padCenter s
  | length s < gameWidth = padCenter $ " " ++ s ++ " "
  | otherwise            = s

padRight :: String -> String
padRight s
  | length s < gameWidth = padRight $ s ++ " "
  | otherwise            = s

indent :: String -> String
indent = ("  " ++)

space :: Int -> String
space n
  | n < 10    = "  "
  | n < 100   = " "
  | otherwise = ""

divider :: String
divider = mkDivider " -"

divider2 :: String
divider2 = mkDivider " ="

mkDivider :: String -> String
mkDivider s = const s `concatMap` [1 .. gameWidth `div` 2]

gameWidth :: Int
gameWidth = 50
