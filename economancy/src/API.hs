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
instance ToJSON CardJSON


data PlayerJSON = PlayerJSON {coins :: Int,
                              buys :: Int,
                              cards :: [CardJSON]}
                  deriving (Generic, Show, Read)

instance FromJSON PlayerJSON
instance ToJSON PlayerJSON


data FalseOrIndex = None | Index Int
                    deriving (Generic, Show, Read)

instance FromJSON FalseOrIndex where
  parseJSON = withText "FalseOrIndex" $ \text ->
    case text of
      "false" -> return None
      x       -> return $ Index $ read $ T.unpack x   
instance ToJSON FalseOrIndex where
  toJSON None = "false"
  toJSON (Index x) = toJSON x


data AttackingJSON =
  AttackingJSON {attackPhase :: String,
                 attacker :: Int,
                 attackerCard :: FalseOrIndex}
  deriving (Generic, Show, Read)

instance FromJSON AttackingJSON where
  parseJSON = withObject "AttackingJSON" $ \obj -> do
    attackPhase <- obj .: "name"
    attacker <- obj .: "attacker"
    attackerCard <- obj .: "attacker-card"
    return (AttackingJSON attackPhase attacker attackerCard)

instance ToJSON AttackingJSON where
  toJSON (AttackingJSON ap atk atkC) =
    object ["name" .= (T.pack ap),
            "attacker" .= atk,
            "attacker-card" .= atkC]


data EndJSON = EndJSON {endPhase :: String,
                        winner :: FalseOrIndex}
               deriving (Generic, Show, Read)

instance FromJSON EndJSON where
  parseJSON = withObject "EndJSON" $ \obj -> do
    endPhase <- obj .: "name"
    winner <- obj .: "winner"
    return (EndJSON endPhase winner)

instance ToJSON EndJSON where
  toJSON (EndJSON endPhase winner) =
    object ["name" .= (T.pack endPhase),
            "winner" .= winner]


data PhaseName = PhaseName {phaseName :: String}
                 deriving (Generic, Show, Read)

instance FromJSON PhaseName where
  parseJSON = withObject "PhaseName" $ \obj -> do
    phaseName <- obj .: "name"
    return (PhaseName phaseName)

instance ToJSON PhaseName where
  toJSON (PhaseName pn) =
    object ["name" .= (T.pack pn)]


data PhaseJSON = InvestingJ PhaseName
               | AttackingJ AttackingJSON
               | BuyJ PhaseName
               | EndJ EndJSON
               deriving (Generic, Show, Read)
    
instance FromJSON PhaseJSON
instance ToJSON PhaseJSON


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

instance ToJSON StateJSON where
  toJSON (StateJSON d ph sh pls pli) =
    object ["day" .= d,
            "phase" .= ph,
            "shop" .= sh,
            "players" .= pls,
            "player" .= pli]


toCardJSON :: PlayerCard -> CardJSON
toCardJSON card =
  CardJSON (Cards.playerCardName card) (Cards.uses card)

  
toPlayerJSON :: Player -> PlayerJSON
toPlayerJSON (Player coins buys cards) =
  PlayerJSON coins buys (map toCardJSON cards)

 
toPhaseJSON :: Phase -> PhaseJSON
toPhaseJSON Earning = InvestingJ (PhaseName "investing")
toPhaseJSON Investing = InvestingJ (PhaseName "investing")
toPhaseJSON (Attacking idx Nothing) =
  AttackingJ (AttackingJSON "attacking" idx None)
toPhaseJSON (Attacking idx (Just x)) =
  error "attacking phase with (just x)"
toPhaseJSON (Defending idx Nothing) =
  error "Defending phase with Nothing"
toPhaseJSON (Defending idx (Just x)) =
  AttackingJ (AttackingJSON "attacking" idx (Index x))
toPhaseJSON Buying = BuyJ (PhaseName "buy")
toPhaseJSON (End Nothing) = EndJ (EndJSON "end" None)
toPhaseJSON (End (Just wi)) = EndJ (EndJSON "end" (Index wi))


toShopJSON :: Shop -> ShopJSON
toShopJSON shop = Dict.mapKeys playerCardName shop

fromShopJSON :: ShopJSON -> Shop
fromShopJSON shopjson =
  Dict.mapKeys (\s -> if (s == "Sorcerer's Stipend") then (strToCard s 0) else (strToCard s 1)) shopjson


toStateJSON :: State -> StateJSON
toStateJSON (State day phase shop pl plI) =
  StateJSON day (toPhaseJSON phase) (toShopJSON shop) (map toPlayerJSON pl) plI


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

