module Player where

import Cards
import World


-- | A player looks at the state and performs a move
playerMove :: State -> Player -> Action
playerMove state pl = Invest 0
