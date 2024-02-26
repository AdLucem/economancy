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

data Phase = Investing
           | Attacking AttackerIndex AttackingCard
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

-- | Get money invested from investment action
getMoney :: Action -> Int
getMoney (Invest x) = x
getMoney _ = error "Tried to get money from a non-investment action"

