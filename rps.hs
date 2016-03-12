import System.Random
import Modes(getGameMode)
import Weapons(Weapon, weaponChoices, getWeapon, genWeapon)

main = do
  printText header
  printText modeSelection

  maybeGameMode <- getGameMode

  case maybeGameMode of
    Nothing -> restart "Invalid game mode!"
    (Just gameMode) -> do

      printText $ weaponsSelection gameMode

      maybeYourWeapon <- getWeapon gameMode
      opponentsWeapon <- genWeapon gameMode

      case maybeYourWeapon of
        Nothing -> restart "Invalid weapon!"
        (Just yourWeapon) -> do
          printText $ battleSequence yourWeapon opponentsWeapon

          retry <- getLine
          if retry == "y" then main else return ()


eval :: Weapon -> Weapon -> String
eval x y = let results = ["              YOU LOSE!", 
                          "            IT'S A TIE!", 
                          "               YOU WIN!"]
           in results !! fromEnum (x `compare` y)

restart :: String -> IO ()
restart reason = if null reason
                 then main
                 else do 
                    mapM putStrLn [divider, reason ++ " ... Try again", divider]
                    main

modeSelection :: [String]
modeSelection = ["       - Choose a game mode -",
                 divider,
                 "1. Standard RPS",
                 "2. Rock-Paper-Scissors-Lizard-Spock"]

weaponsSelection :: String -> [String]
weaponsSelection gameMode = let numberW = \weapon num -> show num ++ ". " ++ show weapon
                                choices = weaponChoices gameMode
                                weapons = zipWith numberW choices [1..]
                            in ["         Choose your weapon!", divider] ++ weapons

battleSequence :: Weapon -> Weapon -> [String]
battleSequence wx wy = [("You pick:         " ++ show wx ++ "!"),
                        ("Your enemy picks: " ++ show wy ++ "!"),
                         divider,
                         eval wx wy,
                         divider, divider,
                         "          Play again? (y/n)"
                        ]

printText :: [String] -> IO [()]
printText txt = let formatted = "" : divider : txt ++ [divider]
                in mapM putStrLn formatted

header :: [String]
header = [divider,
          "    ########  ########   ###### \n\
          \    ##     ## ##     ## ##    ##\n\ 
          \    ########  ########   ###### \n\ 
          \    ##    ##  ##        ##    ##\n\ 
          \    ##     ## ##         ######"]

divider :: String
divider = " - - - - - - - - - - - - - - - - - - "
