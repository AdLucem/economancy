module Main (main) where

import qualified Data.Map as Dict
import qualified Data.List as L

import Cards
import World
import GameMachine
import Player

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

{- ################ Run The Game ################ -}

-- | Get actions from each of our agents
-- | and run transition to next world
step :: State -> State
step state =
  let
    actions = map (playerMove state) (players state)
  in
    gameMachine state actions

    
main :: IO ()
main = do
  print $ initstate
  print $ step initstate
