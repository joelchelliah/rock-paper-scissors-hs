module RpsElem where

class (Eq r, Enum r, Bounded r) => RpsElem r where

  make :: [r] -> String -> Maybe r
  make rs str = let i = (read str :: Int) - 1
                in if i `elem` (fromEnum <$> rs)
                   then Just $ rs !! i
                   else Nothing

  allElems :: [r]
  allElems = [minBound .. maxBound]
