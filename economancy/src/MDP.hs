
module MDP where


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
  } deriving (Show, Read, Eq)


data Action = North | South | East | West



moveAgent :: Agent -> Action -> Agent
moveAgent (Agent x y) action =
  case action of
    North -> Agent x (y - 1)
    South -> Agent x (y + 1)
    East  -> Agent (x - 1) y
    West  -> Agent (x + 1) y


validAgent :: GridWorld -> Agent -> Bool
validAgent (GridWorld g r c s e) (Agent x y) =
  xCorrect && yCorrect && (not inWall)
  where
    xCorrect = (x >= 0) && (x < r)
    yCorrect = (y >= 0) && (y < r)
    inWall = ((g !! y) !! x) /= wall


transHelper :: State -> Action -> Maybe State
transHelper (State world agent) action =
  let
    newAgent = moveAgent agent action
  in
    case (validAgent world newAgent) of
      True  -> Just $ State world newAgent
      False -> Nothing
    


