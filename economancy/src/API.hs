{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module API where

import Control.Applicative
import Data.Aeson
import qualified Data.Text as T
import GHC.Generics
import qualified Data.Map as Dict

import Cards
import World



data CardJSON = CardJSON {name :: String,
                          uses :: Int}
                deriving (Generic, Show, Read)
instance FromJSON CardJSON


data PlayerJSON = PlayerJSON {coins :: Int,
                              buys :: Int,
                              cards :: [CardJSON]}
                  deriving (Generic, Show, Read)
instance FromJSON PlayerJSON


data FalseOrIndex = None | Index Int
                    deriving (Generic, Show, Read)
data AttackingJSON =
  AttackingJSON {attackPhase :: String,
                 attacker :: Int,
                 attackerCard :: FalseOrIndex}
  deriving (Generic, Show, Read)
data EndJSON = EndJSON {endPhase :: String,
                        winner :: FalseOrIndex}
               deriving (Generic, Show, Read)

data PhaseName = PhaseName {phaseName :: String}
                 deriving (Generic, Show, Read)

data PhaseJSON = InvestingJ PhaseName
               | AttackingJ AttackingJSON
               | BuyJ PhaseName
               | EndJ EndJSON
               deriving (Generic, Show, Read)

instance FromJSON FalseOrIndex where
  parseJSON = withText "FalseOrIndex" $ \text ->
    case text of
      "false" -> return None
      x       -> return $ Index $ read $ T.unpack x   

instance FromJSON AttackingJSON where
  parseJSON = withObject "AttackingJSON" $ \obj -> do
    attackPhase <- obj .: "name"
    attacker <- obj .: "attacker"
    attackerCard <- obj .: "attacker-card"
    return (AttackingJSON attackPhase attacker attackerCard)

instance FromJSON EndJSON where
  parseJSON = withObject "EndJSON" $ \obj -> do
    endPhase <- obj .: "name"
    winner <- obj .: "winner"
    return (EndJSON endPhase winner)

instance FromJSON PhaseName where
  parseJSON = withObject "PhaseName" $ \obj -> do
    phaseName <- obj .: "name"
    return (PhaseName phaseName)
    
instance FromJSON PhaseJSON


type ShopJSON = Dict.Map String Int


data StateJSON = StateJSON {day :: Int,
                            phase :: PhaseJSON,
                            shop :: ShopJSON,
                            players :: [PlayerJSON],
                            player :: Int}
                 deriving (Generic, Show, Read)

instance FromJSON StateJSON where
  parseJSON = withObject "StateJSON" $ \obj -> do
    day <- obj .: "day"
    phase <- obj .: "phase"
    shop <- obj .: "shop"
    players <- obj .: "players"
    player <- obj .: "player"
    return (StateJSON day phase shop players player)


instance ToJSON PlayerCard where
  toJSON playerCard =
    object ["name" .= (Cards.playerCardName playerCard),
            "uses" .= (Cards.uses playerCard)]


instance ToJSON Player where
  toJSON (Player coins buys cards) =
    object ["coins" .= coins,
            "buys"  .= buys,
            "cards" .= (map toJSON cards)]

 
instance ToJSON Phase where
  toJSON Earning = object ["name" .= (T.pack "investing")]
  toJSON Investing =
    object ["name" .= (T.pack "investing")]
  toJSON (Attacking idx Nothing) =
    object ["name" .= (T.pack "attacking"),
            "attacker" .= idx,
            "attacker-card" .= (T.pack "false")]
  toJSON (Attacking idx (Just x)) =
    object ["name" .= (T.pack "attacking"),
            "attacker" .= idx,
            "attacker-card" .= x]
  toJSON (Defending idx (Just x)) =
    object ["name" .= (T.pack "attacking"),
            "attacker" .= idx,
            "attacker-card" .= x]
  toJSON Buying =
    object ["name" .= (T.pack "buy")]
  toJSON (End Nothing) =
    object ["name" .= (T.pack "end"),
            "winner" .= (T.pack "false")]
  toJSON (End (Just wi)) =
    object ["name" .= (T.pack "end"),
            "winner" .= wi]

shopToJSON :: Shop -> ShopJSON
shopToJSON shop = Dict.mapKeys playerCardName shop

fromShopJSON :: ShopJSON -> Shop
fromShopJSON shopjson =
  Dict.mapKeys (\s -> if (s == "Sorcerer's Stipend") then (strToCard s 0) else (strToCard s 1)) shopjson

instance ToJSON State where
  toJSON (State day phase shop pl plI) =
    object ["day" .= day,
            "phase" .= phase,
            "shop" .= (shopToJSON shop),
            "players" .= pl,
            "player" .= plI]


fromCardJSON :: CardJSON -> PlayerCard
fromCardJSON (CardJSON card uses) = strToCard card uses

fromPlayerJSON :: PlayerJSON -> Player
fromPlayerJSON (PlayerJSON coins buys cards) =
  Player coins buys (map fromCardJSON cards)

fromFalseOrIndex :: FalseOrIndex -> Maybe Int
fromFalseOrIndex None = Nothing
fromFalseOrIndex (Index i) = Just i

fromPhaseName :: PhaseName -> String
fromPhaseName (PhaseName name) = name

fromAttackingJSON :: AttackingJSON -> Phase
fromAttackingJSON (AttackingJSON name idx cardidx) =
  let
    isAttackingCard = fromFalseOrIndex cardidx
  in
    if (isAttackingCard == Nothing)
    then (Attacking idx Nothing)
    else (Defending idx isAttackingCard)

fromEndJSON :: EndJSON -> Phase
fromEndJSON (EndJSON name winneridx) =
  End (fromFalseOrIndex winneridx)

fromPhaseJSON :: PhaseJSON -> Phase
fromPhaseJSON (InvestingJ _) = Investing
fromPhaseJSON (AttackingJ p) = fromAttackingJSON p
fromPhaseJSON (BuyJ _) = Buying
fromPhaseJSON (EndJ p) = fromEndJSON p


fromStateJSON :: StateJSON -> State
fromStateJSON (StateJSON d ph sh pls pli) =
  State d (fromPhaseJSON ph) (fromShopJSON sh) (map fromPlayerJSON pls) pli

