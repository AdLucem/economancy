
module MDP where

import Text.Printf


{- ############ MAIN MODULE ########### -}

{- Typeclasses are very nice when you want to do math over a lot of different    types of objects.

   This should be a c-type tensor, but... we'll get to that later -}
class Num n => Reward n where
  -- scalar multiplication
  (*+) :: (Num a) => a -> n -> n


{- Typeclasses are also very nice when you want to write a prototype example
   and then roll out your algorithm on your full example later. -}
class MDP state actions where
  transition :: state -> actions -> Maybe state
  reward :: (Reward r) => state -> actions -> r 

  
{- Now the thing about the transition function is that it might fail. We can
   handle this with a 'Maybe'.
   However, a function with a 'Maybe' output -}

{- ############ okay now this bit is just for our sample MDP ########### -}

wall :: String
wall = "||"

data Cell = Cell {x :: Int, y :: Int}
  deriving (Show, Read, Eq)


data GridWorld = GridWorld {grid :: [[String]],
                            length :: Int,
                            breadth :: Int,
                            start :: (Int, Int),
                            end :: [(Int, Int)]
                           } deriving (Show, Read, Eq)


data Agent = Agent {p :: Int, q :: Int}
  deriving (Show, Read, Eq)


data State = State {
  world :: GridWorld,
  agent :: Agent
  } deriving (Eq)


addAgent :: [[String]] -> Agent -> [[String]]
addAgent matrix (Agent x y) =
  let
    insertInRow row = (take y row) ++ ["#"] ++ (drop (y + 1) row)
    agentRow = head $ drop x matrix
  in
    (take x matrix) ++ [(insertInRow agentRow)] ++ (drop (x + 1) matrix) 

  
instance Show State where
  show (State world agent) =
    let
      showRow row = (concatMap (\x -> printf "%3s" x) row) ++ "\n"
      worldWithAgent = addAgent (grid world) agent
    in
      concatMap showRow worldWithAgent

    
data Action = North | South | East | West

moveAgent :: Agent -> Action -> Agent
moveAgent (Agent p q) action =
  case action of
    North -> Agent (p - 1) q
    South -> Agent (p + 1) q
    East  -> Agent p (q - 1)
    West  -> Agent p (q + 1)


validAgent :: GridWorld -> Agent -> Bool
validAgent (GridWorld g r c s e) (Agent x y) =
  xCorrect && yCorrect && notInWall
  where
    xCorrect = (x >= 0) && (x < c)
    yCorrect = (y >= 0) && (y < r)
    notInWall = ((g !! x) !! y) /= wall


transfunc :: State -> Action -> Maybe State
transfunc (State world agent) action =
  let
    newAgent = moveAgent agent action
  in
    case (validAgent world newAgent) of
      True  -> Just $ State world newAgent
      False -> Nothing


sampleGrid = [["+", "+", "+", "+1"],
              ["+", wall, "+", "-1"],
              ["s", "+", "+", "+"]]
sampleWorld = GridWorld sampleGrid 4 3 (2, 0) [(0, 3), (1, 3)]
sampleState = State sampleWorld (Agent 0 0)

    


