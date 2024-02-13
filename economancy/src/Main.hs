module Main (main) where

import Data.Map


data Day = Day1 | Day2 | Day3
           deriving (Show, Read, Eq)

data Card = Card {name :: String,
                  uses  :: Int
                 } deriving (Show, Read, Eq)

cardnames :: [String]
cardnames = ["Sorcerer's Stipend", "Board of Monopoly", "Incantation", "Worker", "Magic Bean Stock", "Bubble", "Ghost", "Senior Worker", "Gold Fish"]

data Player = Player {coins :: Int,
                      buys  :: Int,
                      cards :: [Card]
                     } deriving (Show, Read, Eq)
              
data Phase = Investing
           | Attacking {attacker :: Int,
                        card :: Maybe (Card)}
           | Buy
           | End {winner :: Maybe (Int)}
           deriving (Show, Read, Eq)

type Shop = Map String Int

data State = State {day         :: Day,
                    phase       :: Phase,
                    shop        :: Shop,
                    players     :: [Player],
                    playerIndex :: Int
                   } deriving (Show, Read, Eq)


main :: IO ()
main = do
  putStrLn "hello world"
