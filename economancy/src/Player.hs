{-# LANGUAGE OverloadedRecordDot #-}

module Player where

import Cards
import World


-- | A player looks at the state and performs a move
playerMove :: State -> Action
playerMove (State d ph sh pls plI) =
  case ph of
    Investing -> Invest 0
    otherwise -> Noop
