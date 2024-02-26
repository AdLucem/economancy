module Player where

import Cards
import World


-- | A player looks at the state and performs a move
playerMove :: State -> Player -> Action
playerMove (State d ph sh pls plI) pl =
  case ph of
    Investing -> Invest 0
    otherwise -> Noop
