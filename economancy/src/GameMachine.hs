{-# LANGUAGE OverloadedRecordDot #-}

module GameMachine where

import qualified Data.List as L
import qualified Data.Map as Dict

import Cards
import World
import ValidMoves

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
  State d Investing sh (map (playerEarningT d) pls) plI

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

-- | Given a player and an attacking action
-- | Get an attacking card index (if it exists)
-- | Else return a Nothing
getAttackingCard :: Player -> Action -> AttackingCard
getAttackingCard (Player _ _ cards) attack =
  let
    -- card index (extracted from action)
    atkCardIndex = getCardIndex attack
    -- attacker's card (wrapped in a Maybe)
    atkCardMaybe = getCard cards atkCardIndex
  in
    case atkCardMaybe of
      Nothing -> Nothing
      Just _ -> Just atkCardIndex
  
  
stateAttackT :: State -> [Action] ->
                AttackerIndex -> State
stateAttackT (State d ph sh pls plI) actions atkIdx =
  let
    -- cards of attacking player
    atkPlayerCards = (pls !! atkIdx).cardSet
    -- action of attacking player
    atkAction = actions !! atkIdx
    phase' = Defending atkIdx (getAttackingCard (pls !! atkIdx) atkAction)
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
-- | And returns unaffected/fainted defending, attacking cards
-- | updated (used-up) attacking card is last element in cardlist
cardFight :: PlayerCard -> [PlayerCard] -> [PlayerCard]
cardFight attacker [] = [faint attacker]
cardFight attacker (d:ds) =
  case (match attacker d) of
    Nothing -> d : (cardFight attacker ds)
    Just (a', d') -> d' : (cardFight a' ds) 

-- | Make a list of (defendingPlayerID, cardID) pairs
defenceLineup :: [Int] -> [Int] -> [(Int, Int)]
defenceLineup plIds cardIds =
  let
    -- zip player and card IDs
    plCards = zip plIds cardIds
  in
    -- filter out (player, card) pairs with no valid defending card
    [pr | pr <- plCards, (snd pr) /= -1]
    

-- | replace card at index <ID> in player cardset with new card
replaceCard :: Player -> Int -> PlayerCard -> Player
replaceCard player cardID card =
  let
    cards = player.cardSet
    cards' = (take cardID cards) ++ [card] ++ (drop (cardID+1) cards)
  in
    Player player.coins player.buys cards'

-- | take: player list, player ID, card ID, card
-- | and replace card for player at <cardID> with <card>
-- | return new list of players
replacePlayer :: [Player] -> Int -> Int -> PlayerCard -> [Player]
replacePlayer players plID cardID card =
  let
    player = players !! plID
    player' = replaceCard player cardID card
  in
    (take plID players) ++ [player'] ++ (drop (plID+1) players)


fromMaybeCard :: Maybe PlayerCard -> PlayerCard
fromMaybeCard Nothing = error "Something wrong in the defence handler, returned a non-valid defending card ID"
fromMaybeCard (Just c) = c


-- eliminate player at <id> from players
yeetPlayer :: [Player] -> Int -> [Player]
yeetPlayer players i =
  (take i players) ++ (drop (i+1) players)

-- eliminate players who have -1 as their defence card
-- ie: players who do not give any defence card 
eliminate :: [Player] -> [Int] -> [Player]
eliminate players cardIds =
  let
    pids = [0..((length players) - 1)]
    -- get playerids of to-be-yeeted players
    toYeet = [(fst pid_cid) | pid_cid <- (zip pids cardIds), (snd pid_cid) == -1]
  in
    foldl yeetPlayer players toYeet


stateDefendT :: State -> [Action] ->
                AttackerIndex -> AttackingCard -> State
stateDefendT (State d ph sh pls plI) actions atkIdx cardIdx =
  -- is there an attacking card?
  case cardIdx of
    -- no? okay everybody go home
    Nothing -> State d Buying sh pls plI
    Just (-1) -> State d Buying sh pls plI
    -- yes? okay let's get into it
    Just atkCardID ->
      let
        -- get all card IDS from actions
        cardIds = map getCardIndex actions
        -- get (playerID, defendingCardID) pairs
        defence = defenceLineup [0..((length pls)-1)] cardIds
        -- get defending card for each player in defence
        getDefCard (pid, cid) = fromMaybeCard $ getCard (pls !! pid).cardSet cid
        defCards = map getDefCard defence
        -- okay now get attacking card
        atkCard = (pls !! atkIdx).cardSet !! atkCardID 
        -- fight! result: [defendingcards : attackingcard]
        battlefield = cardFight atkCard defCards
        -- defending cards: all cards except last
        defCards' = init battlefield
        -- attacking card: last card
        atkCard' = last battlefield
        -- replace all defending cards
        f players (pid, cid, crd) = replacePlayer players pid cid crd
        pid_cid_crd = zip3 (map fst defence) (map snd defence) defCards'
        players' = foldl f pls pid_cid_crd
        -- now replace the attacking card too
        players'' = replacePlayer players' atkIdx atkCardID (faint atkCard)
        -- AND FINALLY: yeet players whose card IDs are -1
        -- wait first set attacking player's card ID to not -1
        -- cardIdsMod = (take atkIdx cardIds) ++ [100] ++ (drop (atkIdx+1) cardIds)
        -- players''' = eliminate players'' cardIdsMod
      in
        State d (Attacking atkIdx Nothing) sh players'' plI
      
  
{- ########## Buy Phase Handlers ########## -}

-- | Return updated Player and Shop after card is bought
buyCard :: Player -> Shop -> PlayerCard -> (Player, Shop)
buyCard player shop crd =
  let
    decrement x = x - 1
    shop' = Dict.adjust decrement crd shop
    player' = Player (player.coins - (costPlayerCard crd)) player.buys (crd : player.cardSet)
  in
    (player', shop')

-- | buyCard (above) but it takes a list of players and a playerID
-- | and returns updated list of players
buyCardID :: [Player] -> Shop -> Int -> PlayerCard
          -> ([Player], Shop)
buyCardID players shop plID crd =
  let
    player = players !! plID
    (player', shop') = buyCard player shop crd
    players' = (take plID players) ++ [player'] ++ (drop (plID+1) players)
  in
    (players', shop')

-- | BuyCardID folded over multiple playerIDs
buyCardPlayers :: [Player] -> Shop -> [Int] -> PlayerCard
               -> ([Player], Shop)
buyCardPlayers players shop [] crd = (players, shop)
buyCardPlayers players shop (i:ids) crd =
  let
    (players', shop') = buyCardID players shop i crd
  in
    buyCardPlayers players' shop' ids crd

-- | If element not in dict, then insert, else update
insertOrUpdate :: Ord k => k -> a -> Dict.Map k [a] -> Dict.Map k [a]
insertOrUpdate key value dict =
  case (Dict.lookup key dict) of
    Nothing -> Dict.insert key [value] dict
    Just xs -> Dict.adjust (\ls -> [value] ++ ls) key dict
    
-- | Make a hashmap of cards : player IDs buying it
numBuyers :: [PlayerCard] -> [Int] -> Dict.Map PlayerCard [Int]
          -> ([PlayerCard], [[Int]])
numBuyers [] [] accum =
  let
    accumlist = Dict.toList accum
  in
    ((map fst accumlist), (map snd accumlist))
numBuyers (crd:cs) (p:playerIds) accum =
  let
    accum' = insertOrUpdate crd p accum
  in
    numBuyers cs playerIds accum'

-- | if player is buying card, return card, playerID
-- | else don't return
cardsFromMaybe :: [Maybe PlayerCard] -> Int -> [(PlayerCard, Int)]
cardsFromMaybe [] _ = []
cardsFromMaybe (c:cs) i =
  case c of
    Nothing -> []
    Just c' -> (c', i) : (cardsFromMaybe cs (i+1))

-- | If num cards >= num players buying, buy card
-- | else delete card from shop
tryToBuyCard :: Shop -> [Player] -> PlayerCard -> [Int]
             -> ([Player], Shop)
tryToBuyCard shop players crd buyers =
  case ((shop Dict.! crd) >= (length buyers)) of
    True -> buyCardPlayers players shop buyers crd
    False ->
      let
        shop' = Dict.delete crd shop
      in
        (players, shop')

-- | Above function folded over all cards
tryToBuyAll :: Shop -> [Player] -> [PlayerCard] -> [[Int]]
            -> ([Player], Shop)
tryToBuyAll shop players [] [] = (players, shop)
tryToBuyAll shop players (c:cards) (b:buyers) =
  let
    (players', shop') = tryToBuyCard shop players c b
  in
    tryToBuyAll shop' players' cards buyers
    

stateBuyT :: State -> [Maybe PlayerCard] -> State
stateBuyT state buycards =
  let
    -- list of tuples of (Cards, PlayerId buying)
    buys = cardsFromMaybe buycards 0
    cardsToBuy = map fst buys
    buyers = map snd buys
    -- [Card], [[PlayerID]]
    (crds, listbuyers) = numBuyers cardsToBuy buyers Dict.empty
    (players', shop') = tryToBuyAll state.shop state.players crds listbuyers
  in
    State (incrementDay state.day) Earning shop' players' state.playerIndex
  

{- ########## Eliminate Players ####### -}

-- eliminate :: State -> State
{- ########## End phase check ########## -}

-- | get number of victory points every player has
playerVictoryPoints :: Player -> Int
playerVictoryPoints player = sum $ map getVictoryPoints player.cardSet  

-- | Does any player have enough victory points to win?
vpWin :: [Player] -> Maybe Int
vpWin players =
  let
    vp_all = map playerVictoryPoints players
    maxpoints = maximum vp_all
  in
  if (maxpoints < 7)
  then Nothing
  else
    if ((length [x | x <- vp_all, x == maxpoints]) > 1)
    then Nothing
    else L.elemIndex maxpoints vp_all   
      

winHandlerT :: State -> State
winHandlerT (State d ph sh pl plI) =
  -- can someone win because of victory points?
  case (vpWin pl) of
    Nothing ->
      -- no? well, do we have 0 or only one player left?
      case pl of
        []  -> State d (End Nothing) sh pl plI
        [p] -> State d (End $ Just 0) sh pl plI
        -- still no? well carry on then
        _   -> State d ph sh pl plI
    (Just i) -> State d (End $ Just i) sh pl plI

{- ########## MAIN ########## -}


-- | Check if action is legal for current player
-- | (as defined by state)
runCheckForPlayer :: State -> Action -> Bool
runCheckForPlayer state action =
  action `elem` (validMoves state)

-- | Check if actions are legal
runChecks :: State -> [Action] -> Bool
runChecks s actions =
  let
    _runCheck (state', ac) = runCheckForPlayer state' ac
    states' = map (\i -> State s.day s.phase s.shop s.players i) [0..((length s.players) - 1)]
  in
    L.and $ map _runCheck $ zip states' actions


-- | Run game machine
gameMachine :: State -> [Action] -> State
gameMachine state actions =
  case (runChecks state actions) of
    True -> 
        let
          s' = case (phase state) of
                 Earning -> stateEarningT state
                 Investing ->
                   investmentT state (map getMoney actions)
                 (Attacking atk atkCard) ->
                   stateAttackT state actions atk 
                 (Defending atk atkCard) ->
                   stateDefendT state actions atk atkCard 
                 Buying ->
                   incrementState $ stateBuyT state (map getMaybeCard actions)
                 (End _) -> state
         in
          winHandlerT s'
    False -> state

