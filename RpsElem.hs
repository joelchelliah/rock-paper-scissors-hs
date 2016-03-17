module RpsElem where

class (Eq r, Enum r, Bounded r) => RpsElem r where
  make :: String -> Maybe r
  make = makeFrom allElems

  makeFrom :: [r] -> String -> Maybe r
  makeFrom rs str = let i = (read str :: Int) - 1
                    in if i `elem` (fromEnum <$> rs)
                       then Just $ rs !! i
                       else Nothing

  allElems :: [r]
  allElems = [minBound .. maxBound]
