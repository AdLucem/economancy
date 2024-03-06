{-# LANGUAGE OverloadedRecordDot #-}

module GameMachine where

import qualified Data.List as L

import Cards
import World


{- ########## Earn Phase Handlers ########## -}

earningT :: [PlayerCard] -> Coins -> Day -> Coins
earningT cards currentCoins currentDay =
  let
    addedCoins = map (\c -> earn c currentCoins currentDay) cards
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
    Nothing -> State d Buying sh pls plI
    Just idx -> State d (Attacking idx Nothing) sh pls idx

{- ########## Attack Phase Handlers ########## -}

-- | Get attacker Index from phase
getAttackerIndex :: Phase -> Maybe Int
getAttackerIndex (Attacking i _) = Just i
getAttackerIndex _ = Nothing

stateAttackT :: State -> [Action] -> State
stateAttackT (State d ph sh pls plI) atkIdx atkCard =
  let
    plCards = (pls !! atkIdx).cardSet
    atkCard = plCards !! atkCard
    phase' = Defending atkIdx (Just atkCard)
  in
    State d phase' sh pls plI
      
{- ########## Defense Phase Handlers ########## -}


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
cardFight _ [] = []
cardFight attacker (d:ds) =
  case (match attacker d) of
    Nothing -> d : (cardFight attacker ds)
    Just (a', d') -> d' : (cardFight a' ds) 

-- replaceCard :: [PlayerCard] -> PlayerCard -> [PlayerCard]

-- stateAttackingT :: State -> [PlayerCard] -> State
-- stateAttackingT (State d ph sh pls plI) cards =
--  let
--    attacking = cards !! plI
--    defending = [c | c <- cards, c /= attacking]
--    postAttack = cardFight attacking defending
--  in
    
-- stateBuyT :: State -> Action  -> State

{- ########## Buy Phase Handlers ########## -}

-- stateBuyT :: State -> [Maybe PlayerCard] -> State

{- ########## Phase-Action-Player Contracts ########## -}

-- | Check if action is legal in the current phase 
isActionLegal :: Phase -> Action -> Bool
isActionLegal _ Noop = True
isActionLegal Earning _ = True
isActionLegal Investing (Invest _) = True
isActionLegal (Attacking _ _) (Attack _) = True
isActionLegal (Defending _ _) (Defend _) = True
isActionLegal Buying (Buy _) = True
isActionLegal _ _ = False


-- | Player can only invest a positive amount that is
-- | less than/eq to the number of coins they have
investCheck :: State -> Action -> Player -> Bool
investCheck _ (Invest x) (Player coins _ _) =
  (x >= 0) && (x <= coins)
investCheck _ _ _ = error "Argument error in investCheck"

-- | Player can only attack if they're the attacking player
-- | And they can only attack with a card in their deck with non-zero uses
attackCheck :: State -> Action -> Player -> Bool
attackCheck (State _ _ _ players plIndex) (Attack x) (Player c b cards) =
  let
    isAttackingPlayer = ((players !! plIndex) == (Player c b cards))
    cardInDeck = (x < (length cards))
    nonZeroUses = ((uses (cards !! x)) > 0)
  in
    isAttackingPlayer && cardInDeck && nonZeroUses
attackCheck _ _ _ = error "Argument error in attackCheck"

-- | Player can only defend if not attacking player
-- | And they can only defend with a card in their deck with non-zero uses
defendCheck :: State -> Action -> Player -> Bool
defendCheck (State _ _ _ players plIndex) (Defend x) (Player c b cards) =
  let
    isDefendingPlayer = ((players !! plIndex) /= (Player c b cards))
    cardInDeck = (x < (length cards))
    nonZeroUses = ((uses (cards !! x)) > 0)
  in
    isDefendingPlayer && cardInDeck && nonZeroUses
defendCheck _ _ _ = error "Argument error in defendCheck"

-- | Player can only buy if they own that many coins
buyCheck :: State -> Action -> Player -> Bool
buyCheck _ (Buy Nothing) _ = True
buyCheck _ (Buy (Just card)) (Player coins _ _) =
  (costPlayerCard card) <= coins
buyCheck _ _ _ = error "Argument error in buyCheck"

-- | Check if action is legal for that player in that state
-- | Main driver function for above checks
isPlayerActionLegal :: State ->  Player -> Action -> Bool
isPlayerActionLegal state player action =
  case action of
    (Invest x) -> investCheck state action player
    (Attack x) -> attackCheck state action player 
    (Defend x) -> defendCheck state action player
    (Buy x) -> buyCheck state action player
    Noop -> True
  
{- ########## MAIN ########## -}

-- | Check if (state, actions) combination is legal
runChecks :: State -> [Action] -> Bool
runChecks state actions =
  let
    allTrue ls = foldl (&&) True ls
    currentPhase = phase state
    currentPlayer = (players state) !! (playerIndex state)
    phaseActionsCheck = map (isActionLegal currentPhase) actions
    playerActionsCheck = map (isPlayerActionLegal state currentPlayer) actions
  in
    (allTrue phaseActionsCheck) && (allTrue playerActionsCheck)

-- | Run game machine
gameMachine :: State -> [Action] -> State
gameMachine state actions =
  case (runChecks state actions) of
    True -> case (phase state) of
      Earning -> stateEarningT state
      Investing -> investmentT state (map getMoney actions)
      (Attacking atk atkCard) -> stateAttackT state actions 
        --let
        --  cardSets = map cardSet (players state)
        --  cardsInPlay = map (\x -> 
        -- in
      Buying -> state
      (End _) -> state
    False -> state

