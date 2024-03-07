{-# LANGUAGE OverloadedRecordDot #-}

module Player where

import Cards
import World

-- | Get your own cardset (as indicated by PlayerIndex)
getOwnCards :: State -> [PlayerCard]
getOwnCards (State _ _ _ pls plI) =
  let
    self = pls !! plI
  in
    self.cardSet

    
-- | A player looks at the state and performs a move
playerMove :: State -> Action
playerMove (State d ph sh pls plI) =
  case ph of
    Investing -> Invest 0
    (Attacking idx card) ->
      -- if you are attacking 
      case (idx == plI) of
        True ->
          -- then if you have >1 card 
          if ((length (getOwnCards (State d ph sh pls plI))) > 1) 
          -- attack with second card
          then (Attack 1)
          -- else don't attack
          else (Attack (-1))
        -- if you are not attacking, then send noop
        False -> Noop
    otherwise -> Noop
          
          
      
