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


-- | Increment day
incrementDay :: Day -> Day
incrementDay 1 = 2
incrementDay 2 = 3
incrementDay 3 = 1
incrementDay _ = error "Day is reaching a weird value"

-- Refresh state i.e: reset all player cards
incrementState :: State -> State
incrementState state =
  if (state.day == 3)
  then (State state.day state.phase state.shop (refreshCards state.players) state.playerIndex)
  else state

-- refresh player cards
refreshCards :: [Player] -> [Player]
refreshCards players =
  let
    refreshPlayer (Player cns bys crds) = Player cns bys (map refreshed crds)
  in
    map refreshPlayer players
    
-- | get who is attacking from Attack or Defend phase
whoIsAttacking :: Phase -> Int
whoIsAttacking (Attacking i _) = i
whoIsAttacking (Defending i _) = i
whoIsAttacking _ = error "Tried to run whoIsAttacking on non-Attack/Defend phase"

-- | Get current player from state
currentPlayer :: State -> Player
currentPlayer state = state.players !! state.playerIndex

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
getCardIndex Noop = -1
getCardIndex _ = error "Tried to get card index from a non attack/defend action"

-- | Get a Maybe Playercard from a buy action
getMaybeCard :: Action -> Maybe PlayerCard
getMaybeCard (Buy x) = x
getMaybeCard _ = error "Tried to get a Maybe Playercard from a non buy action"

-- | If phase is end state, get winner index from end state
getWinner :: Phase -> Maybe Int
getWinner (End wi) = wi
getWinner _ = Nothing
