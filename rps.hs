import System.Random

data Weapon = Rock | Paper | Scissors deriving (Eq, Show, Bounded, Enum)

instance Ord Weapon where
  Rock `compare` Scissors = GT
  x `compare` y = fromEnum x `compare` fromEnum y

instance Random Weapon where
    random g = let min = fromEnum (minBound :: Weapon)
                   max = fromEnum (maxBound :: Weapon)
                   (r, g') = randomR (min, max) g
               in (toEnum r, g')

    randomR (min,max) g = let (r, g') = randomR (fromEnum min, fromEnum max) g in (toEnum r, g')


main = do
  printText weaponSelection

  weaponStr <- getLine

  if not $ weaponStr `elem` ["1", "2", "3"]
  then do
    putStrLn "\nInvalid weapon! ... try again\n"
    main
  else do
    opponentWeapon <- genWeapon
    let weaponIndex = (read weaponStr) - 1
    let yourWeapon  = [Rock, Paper, Scissors] !! weaponIndex

    printText $ battleSequence yourWeapon opponentWeapon

    putStrLn "\nPlay again? (y/n)"

    retry <- getLine

    if retry == "y" then main else return ()



genWeapon :: IO Weapon
genWeapon = do
  newStdGen
  gen <- getStdGen
  let (w,_) = random gen :: (Weapon, StdGen)
  return w

eval :: Weapon -> Weapon -> String
eval x y = case x `compare` y of
            GT -> "         YOU WIN!"
            LT -> "        YOU LOSE!"
            EQ -> "      IT'S A TIE!"

weaponSelection :: [String]
weaponSelection = ["  Choose your weapon!",
                   divider,
                   "1. Rock",
                   "2. Paper",
                   "3. Scissors"]

battleSequence :: Weapon -> Weapon -> [String]
battleSequence wx wy = [("You pick:         " ++ show wx ++ "!"),
                        ("Your enemy picks: " ++ show wy ++ "!"),
                         divider,
                         eval wx wy]

printText :: [String] -> IO [()]
printText txt = let formatted = "" : divider : txt ++ [divider]
                in mapM putStrLn formatted

divider :: String
divider = " - - - - - - - - - - - - "
