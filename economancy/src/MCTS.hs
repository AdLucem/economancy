{-# LANGUAGE OverloadedRecordDot #-}
module MCTS where

import qualified Data.Tree as T
import System.Random
import qualified Data.List as L
import Data.Time.Clock

import Cards
import World
import GameMachine
import Player
import ValidMoves
import RunGame

-- value of each node
-- let's currently take this as (wins + draws) / simulations
-- all the information in a node
data TreeNode =
  TreeNode {state  :: State,
            action :: Action,
            value  :: (Int, Int)} deriving (Eq)

instance Show TreeNode where
  show (TreeNode _ ac v) = (show ac) ++ "**" ++ (show $ (fromIntegral $ fst v)/(fromIntegral $ snd v))
  
-- okay at least implementing trees is not our headache
-- we're just using the library
type MCTree = T.Tree TreeNode


-- | check if node is a leaf node i.e: has no children 
isLeaf :: MCTree -> Bool
isLeaf (T.Node _ []) = True
isLeaf _ = False

-- | Get state from MCTree node
getState :: MCTree -> State
getState (T.Node node _) = node.state

-- | Heuristic to randomly select a node from a list of nodes
-- | (note: and carry on random seed)
randomNode :: StdGen -> [MCTree] -> (MCTree, StdGen)
randomNode g [] = error "Empty list of nodes given!!!"
randomNode g [a] = (a, g)
randomNode randomGen nodes =
  let
    (n, gen') = uniformR (0, length nodes) randomGen  
  in
    (nodes !! n, gen')

-- | Heuristic to select node with the highest value
-- | from a list of nodes
-- | the StdGen is only there for record-keeping purposes
bestNode :: StdGen -> [MCTree] -> (MCTree, StdGen)
bestNode g [] = error "Empty list of nodes given!!!"
bestNode g [a] = (a, g)
bestNode g nodes =
  let
    valueTuples = [node.rootLabel.value | node <- nodes]
    integralDiv a b = (fromIntegral a)/(fromIntegral b)
    values = [integralDiv (fst v) (snd v) | v <- valueTuples]
    maxValue = maximum values
    maxValueIndex = L.elemIndex maxValue values
  in
    case maxValueIndex of
      Nothing -> (head nodes, g)
      Just n -> (nodes !! n, g)

-- | MCTS step 1: Selection
-- | Start from root and pick children until we find a leaf node
-- | Function takes a heuristic fn. as argument (see above)
selection :: (StdGen ->[MCTree] -> (MCTree, StdGen))
          -> StdGen -> MCTree -> MCTree
selection heuristic randomGen root =
  case (isLeaf root) of
    True -> root
    False -> let
      (bestChild, gen') = heuristic randomGen root.subForest
      in
        selection heuristic gen' bestChild

-- | MCTS Step 2.1: Generating search space
expandLeaf :: StdGen -> MCTree -> MCTree
expandLeaf gen leaf =
  let
    currentState = getState leaf
    actions = validMoves currentState
    newStates = map (\ac -> partTransition gen currentState ac) actions
    childnodes = map (\(s, a) -> TreeNode s a (0, 0)) (zip newStates actions)
    children = map (\a -> T.Node a []) childnodes
  in
    T.Node leaf.rootLabel children
  
-- | MCTS Step 2.2: Randomly sample from search space
-- | Randomly sample from children of node to get a child node 
randomChild :: StdGen -> MCTree -> (MCTree, StdGen)
randomChild randomGen (T.Node node children) =
  randomNode randomGen children

-- | MCTS Step 2.3: Complete one full playout from C
playout :: StdGen -> MCTree -> GameResult
playout gen tree = head $ trajectory (instanTransition gen) [(getState tree)]

-- | A partly instantiated transition function
-- | where the only non-autonomous agent (i.e: provides its own actions) is agent 0
partTransition :: StdGen -> State -> Action -> State
partTransition gen state action =
  let
    state' = pretransition state
    actions = action : (getAction state' [randomPlayer gen])
    nextState = transition state actions
  in
    nextState 

-- | get index of best child if exists, else 0
fromJust :: Maybe Int -> Int
fromJust (Just x) = x
fromJust Nothing = 0


-- | Generate and randomly sample from search space
sampleSearchSpace :: StdGen -> MCTree -> (MCTree, MCTree, StdGen)
sampleSearchSpace gen (T.Node node children) =
  let
    expandedNode = expandLeaf gen (T.Node node children)
    (child, gen') = randomChild gen expandedNode
  in
    (expandedNode, child, gen')


-- | Full expand function: generate search space, select
-- | random node from search space, and expand 
expand :: StdGen -> MCTree -> (MCTree, StdGen)
expand randomGen (T.Node node children) =
  let
        (expandedNode, child, gen') = sampleSearchSpace randomGen (T.Node node children)
        -- get index of randomly selected child i.e rci
        rci = fromJust $ L.elemIndex child expandedNode.subForest
        -- 2.3: playout
        result = playout gen' (T.Node node children)
        toVal x = if (x == Lose) then 0 else (if (x == Win) then 2 else 1)
        value' = (((fst node.value) + (toVal result)), ((snd node.value) + 1))
        -- educate ur child
        child' = T.Node (TreeNode child.rootLabel.state child.rootLabel.action value') []
        -- update your searchSpace with new child 
        childs' = (take rci expandedNode.subForest) ++ [child'] ++ (drop (rci + 1) expandedNode.subForest)
      in
        -- 3: update child node
        ((T.Node expandedNode.rootLabel childs'), gen')


-- | MCTS Step 3: once end state is reached, update child node
-- | Then backpropagate (update values) all the way from child node
-- | to root R
-- | We integrate all the steps into this function
expandBackprop :: StdGen -> MCTree -> MCTree
expandBackprop randomGen (T.Node node children) =
  case (isLeaf (T.Node node children)) of
    True -> fst $ expand randomGen (T.Node node children)
    False ->
      let
        -- pick best node among children
        (bestChild, gen') = bestNode randomGen children
        -- get index of best child i.e bci
        bci = fromJust $ L.elemIndex bestChild children
        -- expandBackprop over this best node
        updatedChild = expandBackprop gen' bestChild
        -- update your own subtree with new child
        children' = (take bci children) ++ [updatedChild] ++ (drop (bci + 1) children)
        -- get total wins and playouts of subtree
        totalWins = sum [(fst c.rootLabel.value) | c <- children']
        totalPlayouts = sum [(snd c.rootLabel.value) | c <- children']
        value' = (totalWins, totalPlayouts)
      in
        T.Node (TreeNode node.state node.action value') children'

-- | Take a monte carlo tree, run n iterations of MCTS on it
-- | yes this is stateful and yes we put it inside the IO monad
-- | because lazy. this should be its own monad
-- | IO monad: the duct tape of monads
runMCTS :: Int -> MCTree -> IO (MCTree)
runMCTS 0 tree = return tree
runMCTS n tree =
  do
    -- get current time to use as a random seed
    t <- getCurrentTime
    let seed = floor t.utctDayTime
    -- let tree' = expandLeaf (mkStdGen seed) tree
    -- run expandBackprop with random seed
    let tree' = expandBackprop (mkStdGen seed) tree
    -- let (tree', gen) = expand (mkStdGen seed) tree
    -- print $ playout gen tree'
    
    putStrLn $ "Loop: " ++ (show n)
    print tree'
    putStrLn $ T.drawTree $ fmap show tree' 
    -- loop
    runMCTS (n - 1) tree'

-- | transition function instantiated for our specific agents
instanTransition :: StdGen -> State -> State
instanTransition randomGen state =
  let
    (s1, gen') = uniformR (0, 100000) randomGen
    (s2, _) = uniformR (0, 100000) gen'
  in
    step state [(randomPlayer (mkStdGen s1)), (randomPlayer (mkStdGen s2))]
