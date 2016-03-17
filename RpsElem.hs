module RpsElem() where

class (Eq r, Enum r, Bounded r) => RpsElem r where
  availables :: [r]
  availables = [minBound..maxBound]

  funkar :: String -> r
  funkar str = let i = (read str :: Int) - 1
                   x = availables
               in  if i `elem` (fromEnum <$> x)
                   then toEnum i
                   else x !! 0

  funkarInte :: String -> Maybe r
  funkarInte str = let i = (read str :: Int) - 1
                       x = availables
                   in  if i `elem` (fromEnum <$> x)
                       then Just $ toEnum i
                       else Nothing

  --toRpsElem :: String -> Maybe r
  --toRpsElem str = let i = (read str :: Int) - 1
  --                    x = availables
  --                in if i `elem` [0 .. 2]
  --                   then Just $ toEnum i
  --                   else Nothing

  --make :: String -> Maybe r
  --make str = let mr = toRpsElem str
  --           in mr
             --in if r `elem` availables
             --   then Just r
             --   else Nothing



-- Testing:

data RpsTest = A | B | C deriving (Eq, Show, Enum, Bounded)

instance RpsElem RpsTest where
--  availables = [minBound .. maxBound]
