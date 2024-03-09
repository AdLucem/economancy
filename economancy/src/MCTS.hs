{-# LANGUAGE OverloadedRecordDot #-}
module MCTS where

import qualified Data.Tree as T
import qualified System.Random as Random

import Cards
import World
import GameMachine
import Player
import ValidMoves

-- value of each node
-- let's currently take this as (wins + draws) / simulations
type Value = (Int, Int)

-- all the information in a node
data TreeNode =
  TreeNode {state  :: State,
            action :: Action,
            value  :: Value} deriving (Show, Eq)

type MCTree = T.Tree TreeNode

-- randomly sample from search space at a specific state

-- okay at least implementing trees is not our headache
-- newtype MCTree = Tree 

