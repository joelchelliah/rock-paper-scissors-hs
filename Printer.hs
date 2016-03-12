module Printer where

import Modes(GameMode)
import Weapons(Weapon, weaponChoices)

header :: IO ()
header = printText [divider,
                    "    ########  ########   ###### \n\
                    \    ##     ## ##     ## ##    ##\n\ 
                    \    ########  ########   ###### \n\ 
                    \    ##    ##  ##        ##    ##\n\ 
                    \    ##     ## ##         ######"]

modeSelection :: IO ()
modeSelection = printText ["       - Choose a game mode -",
                           divider,
                           "1. Standard RPS",
                           "2. Rock-Paper-Scissors-Lizard-Spock"]

weaponsSelection :: GameMode -> IO ()
weaponsSelection gameMode = let numberW = \weapon num -> show num ++ ". " ++ show weapon
                                choices = weaponChoices gameMode
                                weapons = zipWith numberW choices [1..]
                            in printText $ ["         Choose your weapon!", divider] ++ weapons

battleSequence :: Weapon -> Weapon -> IO ()
battleSequence wx wy = printText [("You pick:         " ++ show wx ++ "!"),
                                  ("Your enemy picks: " ++ show wy ++ "!"),
                                   divider,
                                   eval wx wy,
                                   divider, divider,
                                   "          Play again? (y/n)"]

eval :: Weapon -> Weapon -> String
eval x y = let results = ["              YOU LOSE!", 
                          "            IT'S A TIE!", 
                          "               YOU WIN!"]
           in results !! fromEnum (x `compare` y)

divider :: String
divider = " - - - - - - - - - - - - - - - - - - "

printText :: [String] -> IO ()
printText txt = let formatted = "" : divider : txt ++ [divider]
                in mapM_ putStrLn formatted
