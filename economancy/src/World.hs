module World where

import qualified Data.Map as Dict

import Cards


data Player = Player {coins :: Int,
                      buys  :: Int,
                      cardSet :: [PlayerCard]
                     } deriving (Show, Read, Eq)
              
data Phase = Investing
           -- do not ever do this- "use of partial record selector" -
           -- in haskell
           | Attacking
           | Buying
           -- however, for making-it-like-elm reasons, I commit this sin
           | End
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
            | Buy (Maybe String)
            deriving (Show, Read, Eq)

