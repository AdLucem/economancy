
module Cards where


{- ########## TYPES ########## -}

type Coins = Int
type Day = Int

data BasicCard = BasicCard {name :: String,
                            attackScore :: Int,
                            defenceScore :: Int,
                            cost :: Int,
                            perPlayer :: Int,
                            victoryPoints :: Int,
                            earnings :: (Int, Int, Int)
                           } deriving (Show, Read, Eq, Ord)

data MagicBeanStock = MagicBeanStock BasicCard deriving (Show, Read, Eq, Ord)

data Bubble = Bubble BasicCard deriving (Show, Read, Eq, Ord)

type Uses = Int
data PlayerCard = Simple BasicCard Uses
                | MBS MagicBeanStock Uses
                | B Bubble Uses
                deriving (Show, Read, Eq, Ord)

{- ########## TYPECLASSES ########## -}

class Card cardType where
  _initcard :: cardType -> PlayerCard
  _earn :: cardType -> Coins -> Day -> Coins
  _attack :: cardType -> Maybe Int
  _defend :: cardType -> Maybe Int

{- ########## CLASS INSTANCES ########## -}


instance Card BasicCard where

  _initcard card = Simple card 1
  
  _earn (BasicCard _ _ _ _ _ _ (x, _, _)) _ 1 = x 
  _earn (BasicCard _ _ _ _ _ _ (_, y, _)) _ 2 = y
  _earn (BasicCard _ _ _ _ _ _ (_, _, z)) _ 3 = z

  _attack (BasicCard _ at _ _ _ _ _) = Just at

  _defend (BasicCard _ _ df _ _ _ _) = Just df


instance Card MagicBeanStock where

  _initcard card = MBS card 1
  
  _earn mbs coins _ = coins `div` 3

  _attack (MagicBeanStock basiccard) = _attack basiccard
  _defend (MagicBeanStock basiccard) = _defend basiccard


instance Card Bubble where

  _initcard card = B card 1
  
  _earn _ _ _ = 0

  _attack _ = Nothing
  _defend (Bubble basiccard) = _defend basiccard

{- ########## FUNCTIONS ########## -}

-- | Make a type of card into a playercard
-- | General form of _initcard
initcard :: (Card a) => a -> PlayerCard
initcard = _initcard

-- | Get the card object from a PlayerCard
-- fCard :: (Card a) => PlayerCard -> PlayerCard
-- fCard f (Simple card u) = f card
-- fCard f (MBS card u) = f card
-- fCard f (B card u) = f card

-- | Return number of coins a card gives you
-- | Generalizes _earn over any PlayerCard
earn :: PlayerCard -> Coins -> Day -> Coins
earn (Simple card u) cns d = _earn card cns d
earn (MBS card u) cns d = _earn card cns d
earn (B card u) cns d = _earn card cns d

-- | Return attack value of a card if able to attack
-- | Else returns Nothing
-- | Generalizes _attack over any PlayerCard
attack :: PlayerCard -> Maybe Int
attack (Simple card u) = _attack card
attack (MBS card u) = _attack card
attack (B card u) = _attack card

-- | Return defense value of a card
-- | Generalizes _defend over any PlayerCard
defend :: PlayerCard -> Maybe Int
defend (Simple card u) = _defend card
defend (MBS card u) = _defend card
defend (B card u) = _defend card

-- | Return a card but fainted i.e: 0 uses
faint :: PlayerCard -> PlayerCard
faint (Simple card u) = Simple card 0
faint (MBS card u) = MBS card 0
faint (B card u) = B card 0

{- ########## OBJECTS i.e: the actual cards ########## -}

sorcerersStipend, boardOfMonopoly, incantation :: BasicCard
worker, ghost, seniorWorker, goldFish :: BasicCard
sorcerersStipend = BasicCard "Sorcerer's Stipend" 0 0 0 1 0 (2, 1, 1) 
boardOfMonopoly = BasicCard "Board Of Monopoly" 1 1 2 2 1 (0, 0, 0)
incantation = BasicCard "Incantation" 1 1 4 3 3 (0, 0, 0)
worker = BasicCard "Worker" 1 2 1 2 0 (0, 1, 1)
ghost = BasicCard "Ghost" 3 2 2 2 0 (0, 0, 1)
seniorWorker = BasicCard "Senior Worker" 2 2 2 2 0 (1, 1, 1)
goldFish = BasicCard "Gold Fish" 1 2 3 1 0 (0, 0, 4)


magicBeanStock =
  MagicBeanStock $ BasicCard "Magic Bean Stock" 1 1 1 1 0 (0, 0, 0) 
                  
bubble = Bubble $ BasicCard "Bubble" 9 2 2 1 0 (0, 0, 0)

  

