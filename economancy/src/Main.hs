{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import qualified Data.Map as Dict
import qualified Data.List as L
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as B

import Cards
import World
import GameMachine
import Player
import ValidMoves
import MCTS
import API

{- ########## FUNCTIONS ########## -}

-- | Extract current player from state
currentPlayer :: State -> Player
currentPlayer (State _ _ _ pls i) = pls !! i


{- ################ Sample Types ############### -}

cardlist :: [PlayerCard]
cardlist = 
  (map initcard [boardOfMonopoly, incantation,
                 worker, ghost, seniorWorker, goldFish]) ++
  (map initcard [magicBeanStock]) ++
  (map initcard [bubble])
  
initshop :: Int -> Shop
initshop numPlayers =
  let
    numCards (Simple card _) = numPlayers * (perPlayer card)
    numCards (MBS (MagicBeanStock card) _) = numPlayers * (perPlayer card)
    numCards (B (Bubble card) _) = numPlayers * (perPlayer card)
  in
    Dict.fromList $ [(key, (numCards key)) | key <- cardlist]

initplayer :: Player
initplayer = Player 0 0 [Simple sorcerersStipend 0]

initstate :: State
initstate = State 1 Investing (initshop 2) [initplayer, initplayer] 0

player1 :: Player
player1 = Player 10 0 [(Simple sorcerersStipend 0), (Simple seniorWorker 0)]

state11 :: State
state11 = State 1 (Attacking 0 Nothing) (initshop 2)
               [player1, player1] 0

state12 :: State
state12 = State 1 (Defending 0 Nothing) (initshop 2)
               [player1, player1] 1

state21 :: State
state21 = State 1 Investing (initshop 2) [player1, player1] 0

{- ################ Run The Game ################ -}


type Agent = State -> Action

initagents :: [Agent]
initagents = [playerMove, playerMove]

-- | Step 1: GameMachine processes the current state
-- | Before sending it to players
pretransition :: State -> State
pretransition state = case state.phase of
  Earning -> gameMachine state []
  otherwise -> state
  
-- | GameMachine sends State to players
-- | Players all send an action to GameMachine
getAction :: State -> [Agent] -> [Action]
getAction state agents = map (\f -> f state) agents

-- | GameMachine makes a state transition
transition :: State -> [Action] -> State
transition state actions = gameMachine state actions

-- | Combine getAction and transition
step :: State -> [Agent] -> State
step state agents =
  let
    state' = pretransition state
    actions = getAction state' agents
    nextstate = transition state actions
  in
    nextstate

-- | repeatedly run step until end phase is reached
-- runStep :: State -> State
-- runStep (State d (End w) sh pl pli) = State d (End w) sh pl pli
-- runStep state = step state


main :: IO ()
main = do
  print $ validMoves state11
  print $ validMoves state21
  print $ validMoves state12
