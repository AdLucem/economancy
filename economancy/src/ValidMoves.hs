{-# LANGUAGE OverloadedRecordDot #-}

{- | To generate the set of valid moves for a player at a given step -}

module ValidMoves where

import qualified Data.Map as Dict
import qualified Data.List as L
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as B

import Cards
import World

-- | There is no valid move in the Earning state
-- | Player should not be able to access this state
movesEarning :: State -> [Action]
movesEarning _ = [Noop]


-- | Investing: Invest x, where x <= player.coins
movesInvesting :: State -> [Action]
movesInvesting state = map (\x -> Invest x) [0..playerCoins]
  where
    playerCoins = (currentPlayer state).coins


-- | Attacking: if attacking player, then attack with a card
-- | in your inventory where uses != 0. otherwise, Noop
youAreAttacking :: Player -> [Action]
youAreAttacking player =
  let
    enumCards = zip [0..((length player.cardSet) - 1)] player.cardSet
    attackCards = filter (\(i, crd) -> ((attack crd) /= Nothing) && ((uses crd) > 0)) enumCards
  in
    map (\x -> Attack x) [i | (i, crd) <- attackCards]
    
movesAttacking :: State -> [Action]
movesAttacking state =
  case (state.playerIndex == (whoIsAttacking state.phase)) of
    True ->
      case (youAreAttacking (currentPlayer state)) of
        [] -> [Noop]
        something -> something
    False -> [Noop]


-- | Defending: if defending player, then defend with card
-- | in your inventory. otherwise, noop
youAreDefending :: Player -> [Action]
youAreDefending player =
  let
    enumCards = tail $ zip [0..((length player.cardSet) - 1)] player.cardSet
    defendCards = filter (\(i, crd) -> ((defend crd) /= Nothing)
                           && (defend crd /= (Just 0))
                           && ((uses crd) > 0)) enumCards
  in
    map (\x -> Defend x) [i | (i, crd) <- defendCards]

movesDefending :: State -> [Action]
movesDefending state =
  case (state.playerIndex == (whoIsAttacking state.phase)) of
    True -> [Noop]
    False ->
      case (youAreDefending (currentPlayer state)) of
        [] -> [Noop]
        something -> something 


-- | Buy phase: if you have zero coins, Buy Nothing
-- | Else: buy anything from the shop that you can afford
buyFromShop :: Shop -> Int -> [Action]
buyFromShop shop coins =
  let
    -- all available cards in shop (i.e: num != 0)
    avlCards = map (\(k, v) -> k) $ filter (\(k, v) -> (v /= 0)) (Dict.toList shop)
    -- all buyable cards in shop
    affordCards = [c | c <- avlCards, ((costPlayerCard c) <= coins)]
  in
    -- actions: buy any affordable card
    map (\x -> Buy (Just x)) affordCards

movesBuying :: State -> [Action]
movesBuying state =
  let
    curPlayer = (currentPlayer state)
  in
    if (curPlayer.coins == 0)
    then [(Buy Nothing)]
    else
      case (buyFromShop state.shop curPlayer.coins) of
        [] -> [Buy Nothing]
        something -> something


-- | End phase: only valid move is Noop
movesEnd :: State -> [Action]
movesEnd _ = [Noop]


-- | putting it all together
validMoves :: State -> [Action]
validMoves state = case state.phase of
  Earning         -> movesEarning state
  Investing       -> movesInvesting state
  (Attacking _ _) -> movesAttacking state
  (Defending _ _) -> movesDefending state
  Buying          -> movesBuying state
  (End _)         -> movesEnd state


