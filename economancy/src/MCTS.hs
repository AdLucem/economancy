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
  show (TreeNode _ ac v) =
    -- (show ac) ++ " * " ++
    (show $ intDiv v)
  
-- okay at least implementing trees is not our headache
-- we're just using the library
type MCTree = T.Tree TreeNode


{- ########## UTILS ########## -}

-- | check if node is a leaf node i.e: has no children 
isLeaf :: MCTree -> Bool
isLeaf (T.Node _ []) = True
isLeaf _ = False

-- | Get state from MCTree node
getState :: MCTree -> State
getState (T.Node node _) = node.state

-- | convert from result to score
score :: GameResult -> Int
score result = case result of
  Win  -> 2
  Draw -> 1
  Lose -> 0

-- | update the current node according to score from 1 playout
updateValue :: MCTree -> Int -> MCTree
updateValue (T.Node (TreeNode s a (n, d)) ch) score =
  (T.Node (TreeNode s a ((n + score), (d + 1))) ch)

-- | Divide two Int values in a tuple- fst / snd
intDiv :: (Int, Int) -> Float
intDiv (a, b) = (fromIntegral a) / (fromIntegral b)

-- | Update a node with new children
-- | where (value node) = sum (values children)
updateChildren :: MCTree -> [MCTree] -> MCTree
updateChildren node children =
  let
    tnode = node.rootLabel
    value' = foldl (\(x1, y1) (x2, y2) -> ((x1 + x2), (y1 + y2)))
                   (0, 0) [tnode.value | c <- children]
  in
    T.Node (TreeNode tnode.state tnode.action value') children

-- | Update value of node from children
updateValueChildren :: MCTree -> MCTree
updateValueChildren node = updateChildren node node.subForest

-- | Over tree
overTree :: (MCTree -> MCTree) -> MCTree -> MCTree
overTree f tree =
  case (isLeaf tree) of
    True  -> tree
    False ->
      let
        children' = map f tree.subForest
      in
        updateChildren tree children'

{- ########## HEURISTICS ########## -}

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


{- ########## MCTS ########## -}

-- | Complete one full playout from node and return the result
playout :: (MCTree, StdGen) -> (GameResult, StdGen)
playout (tree, gen) =
  let
    result = head $ trajectory (instanTransition gen) [(getState tree)]
    (_, gen') = genWord8 gen
  in
    (result, gen')
    
-- | Update node value according to playout from node
playoutNode :: (MCTree, StdGen) -> (MCTree, StdGen)
playoutNode (tree, gen) =
  let
    (result, gen') = playout (tree, gen)
  in
    ((updateValue tree (score result)), gen')

-- | Populate node children with all action nodes
expand :: (MCTree, StdGen) -> (MCTree, StdGen)
expand (leaf, gen) =
  let
    actions = validMoves (getState leaf)
    newStates = map (\ac -> partTransition gen (getState leaf) ac) actions
    children = [(T.Node (TreeNode s a (0, 0)) []) | (s, a) <- (zip newStates actions)]
    (_, gen') = genWord8 gen
  in
    ((T.Node leaf.rootLabel children), gen')

-- | Select random child of node
randomChild :: (MCTree, StdGen) -> (MCTree, StdGen)
randomChild (node, gen) =
  let
    (i, gen') = uniformR (0, ((length node.subForest)-1)) gen
  in
    (((node.subForest) !! i), gen')

-- | Select best child of node
bestChild :: (MCTree, StdGen) -> (MCTree, StdGen)
-- if leaf node then return node itself
bestChild ((T.Node node []), gen) = ((T.Node node []), gen)  
bestChild (node, gen) =
  let 
    values = [(intDiv child.rootLabel.value) | child <- node.subForest]
    maxValueIndex = L.elemIndex (maximum values) values
  in
    case maxValueIndex of
      Nothing -> ((head node.subForest), gen)
      Just i -> ((node.subForest !! i), gen)
 

-- | Putting above functions together- i.e:
-- | Expand, playout and update a single node
expandPlayoutUpdate :: (MCTree, StdGen) -> (MCTree, StdGen)
expandPlayoutUpdate (tree, gen) =
  let
    (expandedTree, gen') = expand (tree, gen)
    (child, gen'') = randomChild (expandedTree, gen')
    rest = L.delete child expandedTree.subForest
    (child', gen''') = playoutNode (child, gen'')
    children' = child' : rest
    tree' = T.Node expandedTree.rootLabel children'
  in
    ((updateValueChildren tree'), gen''')


-- | Run a single iteration of MCTS
-- | Starting from root
iteration :: (MCTree, StdGen) -> IO (MCTree, StdGen)
iteration (root, gen) = do
  case (isLeaf root) of
      True  -> do
        -- print (root.rootLabel.value)
        return $ expandPlayoutUpdate (root, gen)
      False -> do
        let (bestchild, gen') = bestChild (root, gen)
        let restchilds = L.delete bestchild root.subForest
        -- RECURSIVE STEP
        (child', gen'') <- iteration (bestchild, gen')
        -- REJOINING STEP
        let children' =  child' : restchilds
        -- print (root.rootLabel.value)
        return ((updateChildren root children'), gen'')
    

{- ########## MCTS Runtime Functions ########## -}

pickmove :: (MCTree, StdGen) -> Action
pickmove (T.Node _ [], _) = Noop
pickmove (tree, gen) =
  let
    (bestchild, _) = bestChild ((head $ tree.subForest), gen)
  in
    bestchild.rootLabel.action

    
-- | Take a monte carlo tree, run n iterations of MCTS on it
-- | yes this is stateful and yes we put it inside the IO monad
-- | because lazy. this should be its own monad
-- | IO monad: the duct tape of monads
runMCTS :: Int -> MCTree -> IO (MCTree)
runMCTS 0 tree = return tree
runMCTS n tree =
  do
    t <- getCurrentTime
    let seed = floor t.utctDayTime
    
    (tree', gen'') <- iteration (tree, (mkStdGen seed))
    runMCTS (n-1) tree'
    -- let moves = validMoves (getState tree')
    -- print $ head moves
    -- print $ (getState tree').phase
    -- let state' = partTransition gen' (getState tree') (head moves)
    -- print $ state'.phase
    -- print tree'
    -- putStrLn $ T.drawTree $ fmap show tree'
    -- print $ pickmove ((head $ tree'.subForest), gen'')
    -- print "----------------------------"
    -- runMCTS (n - 1) tree'
    {-
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
    -}
