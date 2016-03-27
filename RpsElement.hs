module RpsElement where

import System.Random

class (Eq r, Enum r, Bounded r, Random r) => RpsElement r where
  make :: String -> Maybe r
  make = makeFrom allElems

  makeFrom :: [r] -> String -> Maybe r
  makeFrom rs str = let i = read str :: Int
                    in if i `elem` [1 .. length rs]
                       then Just $ rs !! (i - 1)
                       else Nothing

  gen :: IO r
  gen = genFrom (minBound, maxBound)

  genFrom :: (r, r) -> IO r
  genFrom rng = let gen = fst . randomR rng
                in newStdGen >> gen <$> getStdGen

  allElems :: [r]
  allElems = [minBound .. maxBound]
