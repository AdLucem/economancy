module GameMachine where

import Data.List as L

import Cards
import World



{- ########## Earn Phase Handlers ########## -}

earningT :: [PlayerCard] -> Coins -> Day -> Coins
earningT cards currentCoins day =
  let
    addedCoins = map (\c -> earn c currentCoins day) cards
  in
    foldl (+) currentCoins addedCoins

playerEarningT :: Day -> Player -> Player
playerEarningT day (Player cns bys crds) =
  Player (earningT crds cns day) bys crds

stateEarningT :: State -> State
stateEarningT (State d ph sh pls plI) =
  State d ph sh (map (playerEarningT d) pls) plI

{- ########## Investment Phase Handlers ########## -}

-- | Takes the money invested by all players
-- | And returns the index of attacking player
-- | If there exists attacking player 
type MoneyInvested = Int
compareInvestment :: State -> [MoneyInvested] -> Maybe Int
compareInvestment (State _ _ _ pls _) moneys =
  let
    -- get max. amount of money invested
    maxMoney = maximum moneys
    -- if two/more players invested that amount
    n = length $ filter (\x -> x == maxMoney) moneys
  in
    if (n > 1)
    then Nothing
    else (L.elemIndex maxMoney moneys)

-- | Take the money invested by all players
-- | And return new state with Attack Phase
-- | And index of attacking player
investmentT :: State -> [MoneyInvested] -> State
investmentT (State d ph sh pls plI) ls =
  case (compareInvestment (State d ph sh pls plI) ls) of
    Nothing -> State d ph sh pls plI
    Just idx -> State d Attacking sh pls idx

{- ########## Attack Phase Handlers ########## -}


-- if any defending card has a higher attack than the
-- attacking card's defence, then attacking card faints

-- if the attacking card's attack is higher than the
-- defending car's defence, the defending card faints

-- | attacking card attacks defending card.
-- | defending card wins or faints or invalidMode
fight :: PlayerCard -> PlayerCard -> Maybe PlayerCard
fight atk def =
  case ((attack atk), (defend def)) of
    -- invalid movesets 
    (Nothing, _) -> Nothing
    (_, Nothing) -> Nothing
    (Just a, Just d) ->
      case (compare a d) of
        -- if attack less than defence, nothing happens to defender
        LT -> Just def
        -- if attack greater than defence, defender faints
        GT -> Just $ faint def
        -- else if both are equal, nothing happens to defender
        EQ -> Just def
                          
-- | Takes two cards, and both attack the other
match :: PlayerCard -> PlayerCard -> Maybe (PlayerCard, PlayerCard)
match card1 card2 =
  let
    card2' = fight card1 card2
    card1' = fight card2 card1
  in
    case (card1', card2') of
      (Nothing, _) -> Nothing
      (_, Nothing) -> Nothing
      (Just x, Just y) -> Just $ (x, y)
      
-- | Takes the attacking card
-- | And all the defending cards played by players
-- | And returns unaffected/fainted cards
cardFight :: PlayerCard -> [PlayerCard] -> [PlayerCard]
cardFight attacker [] = []
cardFight attacker (d:ds) =
  case (match attacker d) of
    Nothing -> d : (cardFight attacker ds)
    Just (a', d') -> d' : (cardFight a' ds) 
    
stateAttackingT :: State -> [PlayerCard] -> State
stateAttackingT (State d ph sh pls plI) cards =
  let
    attackingCard = 
-- stateBuyT :: State -> Action  -> State

{- ########## MAIN ########## -}

gameMachine :: State -> [Action] -> State
gameMachine (State d ph sh pls plI) als = State d ph sh pls plI

