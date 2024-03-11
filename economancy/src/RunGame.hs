{-# LANGUAGE OverloadedRecordDot #-}

module RunGame where

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
import API


type Agent = State -> Action

data GameResult = Win | Lose | Draw deriving (Show, Read, Eq)


-- | Step 1: GameMachine processes the current state
-- | Before sending it to players
pretransition :: State -> State
pretransition state = case state.phase of
  Earning -> gameMachine state []
  otherwise -> state
  
-- | GameMachine sends State to players
-- | Players all send an action to GameMachine
getAction :: State -> [Agent] -> [Action]
getAction (State d ph sh pl plI) agents =
    map (\(ag, i) -> ag (State d ph sh pl i))
        (zip agents [0..((length agents)-1)])

-- | GameMachine makes a state transition
transition :: State -> [Action] -> State
transition state actions = gameMachine state actions

-- | Combine getAction and transition
step :: State -> [Agent] -> State
step state agents =
  let
    state' = pretransition state
    actions = getAction state' agents
    nextstate = transition state' actions
  in
    nextstate

-- | Take the id of the winning player, and a list of players
-- | And return Win/Lose for each player
toGameResult :: Int -> [Player] -> [GameResult]
toGameResult winnerID players =
  map (\x -> if (x==winnerID) then Win else Lose)
      [0..((length players) - 1)]

-- | Return Draw for each player in list of players 
allDraws :: [Player] -> [GameResult]
allDraws players = [Draw | x <- players]

-- | Follow a game trajectory until the end
-- | Takes a custom transition function as argument
trajectory :: (State -> State) -> [State] -> [GameResult]
trajectory _ [] = error "Trajectory function needs an initial state"
trajectory transitionFn [s] = trajectory transitionFn ((transitionFn s):[s])
trajectory transitionFn (s0:s1:sx)
  -- force stop if too long
  | ((length sx) > 200) = (allDraws s0.players)
  -- if no state change over two plays, then return draw
  | (s0 == s1) = (allDraws s0.players)
  | otherwise = case (getWinner s0.phase) of
      -- if end state, then return GameStates
      Just wID -> toGameResult wID s0.players
      Nothing -> trajectory transitionFn ((transitionFn s0):s0:s1:sx)

