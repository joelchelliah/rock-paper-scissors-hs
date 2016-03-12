import System.Random
import Weapons

main = do
  let availableGameModes = map show [1, 2]

  printText gameModeSelection
  gameModeChoice <- getLine

  if not $ gameModeChoice `elem` availableGameModes
  then restart "Invalid game mode!"
  else do
    let allWeapons = map show $ [1, 2, 3, 4, 5]
    let availableWeapons = if gameModeChoice == "1" then take 3 allWeapons else allWeapons
    let weaponChoices = take (length availableWeapons) [minBound :: Weapon .. maxBound :: Weapon]
    let weaponChoicesStr = zipWith (\weapon num -> show num ++ ". " ++ show weapon) weaponChoices [1..]

    printText $ weaponSelection weaponChoicesStr
    weaponStr <- getLine

    if not $ weaponStr `elem` availableWeapons
    then restart "Invalid weapon!"
    else do
      opponentWeapon <- genWeapon gameModeChoice
      let weaponIndex = (read weaponStr) - 1
      let yourWeapon  = weaponChoices !! weaponIndex

      printText $ battleSequence yourWeapon opponentWeapon

      putStrLn "\nPlay again? (y/n)"

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

gameModeSelection :: [String]
gameModeSelection = ["        - Choose a game mode -",
                   divider,
                   "1. Standard RPS",
                   "2. Rock-Paper-Scissors-Lizard-Spock"]

weaponSelection :: [String] -> [String]
weaponSelection weapons = ["         Choose your weapon!", divider] ++ weapons

battleSequence :: Weapon -> Weapon -> [String]
battleSequence wx wy = [("You pick:         " ++ show wx ++ "!"),
                        ("Your enemy picks: " ++ show wy ++ "!"),
                         divider,
                         eval wx wy]

printText :: [String] -> IO [()]
printText txt = let formatted = "" : divider : txt ++ [divider]
                in mapM putStrLn formatted

divider :: String
divider = " - - - - - - - - - - - - - - - - - - "
