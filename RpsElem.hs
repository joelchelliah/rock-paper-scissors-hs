module RpsElem where

class (Eq r, Enum r, Bounded r) => RpsElem r where
  make :: String -> Maybe r
  make = makeFrom allElems

  makeFrom :: [r] -> String -> Maybe r
  makeFrom rs str = let i = read str :: Int
                    in if i `elem` [1 .. length rs]
                       then Just $ rs !! (i - 1)
                       else Nothing

  allElems :: [r]
  allElems = [minBound .. maxBound]
