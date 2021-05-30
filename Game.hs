module Game where

import Data.Array

type Color = Word
type Slot = Word
nColors, nSlots :: Num a => a
nColors = 6
nSlots = 4
colors = [1..nColors]
slots = (1,nSlots)

type Guess = [Color]

data State = FindColors { knownColors :: [(Color, Bool)]
                        , unknownColors :: [Color]
                        }
           | FindOrder  { untriedPerms :: [Guess]
                        }

initialState :: State
initialState = FindColors { unknownColors = colors, knownColors = [] }

makeGuess :: State -> Maybe Guess
makeGuess FindColors    { unknownColors = []
                        , knownColors = kcs
                        }
        = makeGuess $ FindOrder { untriedPerms = perms }
        where perms = sequence $ replicate nSlots correctColors
              correctColors = [ c | (c,True) <- kcs ]
makeGuess FindColors    {unknownColors = c:_}
        = Just $ replicate nSlots c
makeGuess FindOrder     {untriedPerms = g:gs}
        = Just g
