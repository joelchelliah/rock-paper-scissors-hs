module Printer(header, modeSelection, weaponsSelection, battleSequence) where

import GameModes(GameMode, gameModeNames)
import Weapons(Weapon, weaponsIn)
import Reactions(getReaction)

header :: IO ()
header = printText [divider,
                    "########  ########   ####### ",
                    "##     ## ##     ## ###    ##",
                    "########  ########   ######  ",
                    "##    ##  ##       ##    ### ",
                    "##     ## ##        #######  "]

modeSelection :: IO ()
modeSelection = let formatted = indent <$> padRight <$> gameModeNames
                in printText $ "- Choose a game mode -"
                             : divider
                             : formatted

weaponsSelection :: GameMode -> IO ()
weaponsSelection gameMode = let numberWpn = \wpn num -> show num ++ ". " ++ show wpn
                                weapons   = weaponsIn gameMode
                                numbered  = zipWith numberWpn weapons [1..]
                                formatted = indent <$> padRight <$> numbered
                            in printText $ ("<> " ++ show gameMode ++ " <>")
                                         : divider
                                         : "- Choose your weapon -"
                                         : divider
                                         : formatted

battleSequence :: Weapon -> Weapon -> IO ()
battleSequence wx wy = printText [("You pick:         " ++ show wx ++ "!"),
                                  ("Your enemy picks: " ++ show wy ++ "!"),
                                   divider, divider,
                                   getReaction wx wy,
                                   eval wx wy,
                                   divider, divider,
                                   "Play again? (y/n)"]


eval :: Weapon -> Weapon -> String
eval x y = let results = ["YOU LOSE!",
                          "IT'S A TIE!",
                          "YOU WIN!"]
           in results !! fromEnum (x `compare` y)

printText :: [String] -> IO ()
printText txt = let dividers  = [divider, divider]
                    formatted = "" : dividers ++ txt ++ dividers
                in mapM_ putStrLn $ padCenter <$> formatted

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

divider :: String
divider = concat $ map (\_ -> " -") [1 .. gameWidth `div` 2]

gameWidth :: Int
gameWidth = 50
