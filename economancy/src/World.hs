{-# LANGUAGE OverloadedRecordDot #-}

module World where

import qualified Data.Map as Dict

import Cards


data Player = Player {coins :: Int,
                      buys  :: Int,
                      cardSet :: [PlayerCard]
                     } deriving (Show, Read, Eq)

type AttackerIndex = Int
type AttackingCard = Maybe Int
type WinnerIndex = Maybe Int

data Phase = Earning
           | Investing
           | Attacking AttackerIndex AttackingCard
           | Defending AttackerIndex AttackingCard
           | Buying
           | End WinnerIndex
           deriving (Show, Read, Eq)


type Shop = Dict.Map PlayerCard Int

data State = State {day         :: Day,
                    phase       :: Phase,
                    shop        :: Shop,
                    players     :: [Player],
                    playerIndex :: Int
                   } deriving (Show, Eq)


data Action = Invest Int
            | Attack Int
            | Defend Int
            | Buy (Maybe PlayerCard)
            | Noop
            deriving (Show, Read, Eq)

-- | Get a card from a list by index (if the index is valid)
-- | if index = -1 then return nothing
getCard :: [PlayerCard] -> Int -> Maybe PlayerCard
getCard cardlist index =
  if ((index >= 0) && ((length cardlist) < index))
  then Nothing
  else Just (cardlist !! index)
  
-- | Get money invested from investment action
getMoney :: Action -> Int
getMoney (Invest x) = x
getMoney _ = error "Tried to get money from a non-investment action"

-- | Get card index from an attack/defend action
getCardIndex :: Action -> Int
getCardIndex (Attack x) = x
getCardIndex (Defend x) = x
getCardIndex _ = error "Tried to get card index from a non attack/defend action"
