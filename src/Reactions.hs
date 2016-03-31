module Reactions(getReaction) where

import Data.Generics.Aliases(orElse)
import Data.Maybe(maybe)
import Data.Char(toUpper)
import Data.List(find)
import Weapons

data Reaction = Reaction { subject :: Weapon, verb :: String, object :: Weapon }

instance Show Reaction where
  show r = let s = (show . subject) r
               v = verb r
               o = (show . object) r
           in map toUpper . unwords $ [s, v, o]


getReaction :: Weapon -> Weapon -> String
getReaction w1 w2 = let m1 (Reaction s _ v) = w1 == s && w2 == v
                        m2 (Reaction s _ v) = w1 == v && w2 == s
                        maybeFoundReaction  = find m1 reactions `orElse` find m2 reactions
                    in maybe ". . ." show maybeFoundReaction

reactions :: [Reaction]
reactions = concat [makeReactions Rock     "crushes"       [Scissors, Sponge, Lizard],
                    makeReactions Rock     "pounds out"    [Fire],
                    makeReactions Paper    "covers"        [Rock],
                    makeReactions Paper    "fans"          [Air],
                    makeReactions Paper    "floats on"     [Water],
                    makeReactions Paper    "disproves"     [Spock],
                    makeReactions Scissors "cut"           [Paper, Sponge],
                    makeReactions Scissors "swish through" [Air],
                    makeReactions Scissors "decapitate"    [Lizard],
                    makeReactions Fire     "melts"         [Scissors],
                    makeReactions Fire     "burns"         [Paper, Sponge],
                    makeReactions Sponge   "soaks"         [Paper],
                    makeReactions Sponge   "absorbs"       [Air, Water],
                    makeReactions Air      "blows out"     [Fire],
                    makeReactions Air      "erodes"        [Rock],
                    makeReactions Air      "evaporates"    [Water],
                    makeReactions Water    "erodes"        [Rock],
                    makeReactions Water    "puts out"      [Fire],
                    makeReactions Water    "rusts"         [Scissors],
                    makeReactions Lizard   "poisons"       [Spock],
                    makeReactions Lizard   "eats"          [Paper]
                   ]

makeReactions :: Weapon -> String -> [Weapon] -> [Reaction]
makeReactions s v = map (Reaction s v)
