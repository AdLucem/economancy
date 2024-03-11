{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module API where

import Control.Applicative
import Data.Aeson
import qualified Data.Text as T
import GHC.Generics
import qualified Data.Map as Dict

import Cards
import World




instance ToJSON PlayerCard where
  toJSON playerCard =
    object ["name" .= (playerCardName playerCard),
            "uses" .= (uses playerCard)]

instance FromJSON PlayerCard where
  parseJSON = withObject "PlayerCard" $ \ v ->
    strToCard <$> v .: "name"
              <*> v .: "uses"

instance ToJSON Player where
  toJSON (Player coins buys cards) =
    object ["coins" .= coins,
            "buys"  .= buys,
            "cards" .= (map toJSON cards)]

instance FromJSON Player where
  parseJSON = withObject "Player" $ \v ->
    Player <$> v .: "coins"
           <*> v .: "buys"
           <*> v .: "cards"

 
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
    object ["name" .= (T.pack "buying")]
  toJSON (End Nothing) =
    object ["name" .= (T.pack "end"),
            "winner" .= (T.pack "false")]
  toJSON (End (Just wi)) =
    object ["name" .= (T.pack "end"),
            "winner" .= wi]

{-
instance FromJSON Phase where
  parseJSON (Object obj) =  do
    name <- obj .: "name"
    case name of
      "investing" -> return Investing
      "attacking" -> do
        attackerID   <- obj .: "attacker" 
        attackerCard <- obj .: "attacker-card"
        case attackerCard of
          "false" -> return $ Attacking attackerID Nothing
          x       -> return $ Defending attackerID (Just $ read x)
      "buy" -> return Buying
      "end" -> do
        win <- obj .: "winner"
        case win of
          "false" -> return $ End Nothing
          x       -> return $ End (Just $ read x)
-}

type ShopJSON = Dict.Map String Int

shopToJSON :: Shop -> ShopJSON
shopToJSON shop = Dict.mapKeys playerCardName shop

instance ToJSON State where
  toJSON (State day phase shop pl plI) =
    object ["day" .= day,
            "phase" .= phase,
            "shop" .= (shopToJSON shop),
            "players" .= pl,
            "player" .= plI]

