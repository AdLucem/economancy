{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import qualified Data.Map as Dict
import qualified Data.List as L
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Tree as T
import Data.Time.Clock
import System.Random
import GHC.Conc.IO
import System.IO


import Cards
import World
import GameMachine
import Player
import ValidMoves
import RunGame
import MCTS
import API

{- ########## FUNCTIONS ########## -}

-- | Extract current player from state
currentPlayer :: State -> Player
currentPlayer (State _ _ _ pls i) = pls !! i


{- ################ Sample Types ############### -}

cardlist :: [PlayerCard]
cardlist = 
  (map initcard [boardOfMonopoly, incantation,
                 worker, ghost, seniorWorker, goldFish]) ++
  (map initcard [magicBeanStock]) ++
  (map initcard [bubble])
  
initshop :: Int -> Shop
initshop numPlayers =
  let
    numCards (Simple card _) = numPlayers * (perPlayer card)
    numCards (MBS (MagicBeanStock card) _) = numPlayers * (perPlayer card)
    numCards (B (Bubble card) _) = numPlayers * (perPlayer card)
  in
    Dict.fromList $ [(key, (numCards key)) | key <- cardlist]

initplayer :: Player
initplayer = Player 0 0 [Simple sorcerersStipend 0]

initstate :: State
initstate = State 1 Earning (initshop 2) [initplayer, initplayer] 0

player1 :: Player
player1 = Player 10 0 [(Simple sorcerersStipend 0), (Simple seniorWorker 0)]

state11 :: State
state11 = State 1 (Attacking 0 Nothing) (initshop 2)
               [player1, player1] 0

state12 :: State
state12 = State 1 (Defending 0 Nothing) (initshop 2)
               [player1, player1] 1

state21 :: State
state21 = State 1 Earning (initshop 2) [player1, player1] 0

initagents :: [Agent]
initagents = [playerMove, playerMove]

initnode :: MCTree
initnode = T.Node (TreeNode initstate Noop (0,0)) []

tonode :: State -> MCTree
tonode state = T.Node (TreeNode state Noop (0,0)) []
-- | repeatedly run step until end phase is reached
-- runStep :: State -> State
-- runStep (State d (End w) sh pl pli) = State d (End w) sh pl pli
-- runStep state = step state
{-

f :: State -> Int -> State
f (State d ph sh pls plI) i = State d ph sh pls i


randomstep :: State -> IO State
randomstep state = do
  
  t1 <- getCurrentTime
  threadDelay 1000
  t2 <- getCurrentTime
  
  let mkSeed x = (floor (x * 1000000000)) `mod` 1000000
  let seed1 = mkSeed t1.utctDayTime
  let seed2 = mkSeed t2.utctDayTime 

  let state' = pretransition state
  let agents = [(randomPlayer (mkStdGen seed1)), (randomPlayer (mkStdGen seed2))]
  print $ validMoves (f state' 0)
  print $ validMoves (f state' 1)
  let actions = getAction state' agents
  print actions
  
  return $ step state' agents


mkSeed :: DiffTime -> Int
mkSeed x = (floor (x * 1000000000)) `mod` 1000000




fromMaybeState :: Maybe State -> State
fromMaybeState Nothing = error "decoding error!"
fromMaybeState (Just s) = s
-- main :: IO (State)
-}

mkSeed :: DiffTime -> Int
mkSeed x = (floor (x * 1000000000)) `mod` 1000000

getSeed :: IO Int
getSeed = do
  threadDelay 1000
  t1 <- getCurrentTime
  return $ mkSeed t1.utctDayTime

randomTF :: Int -> State -> State
randomTF seed state = step state [(randomPlayer (mkStdGen seed)), (randomPlayer (mkStdGen (seed*2)))]


f :: State -> Int -> State
f (State d ph sh pls plI) i = State d ph sh pls i

randomstep :: State -> IO State
randomstep state = do
  
  seed1 <- getSeed
  seed2 <- getSeed 

  let state' = pretransition state
  let agents = [(randomPlayer (mkStdGen seed1)), (randomPlayer (mkStdGen seed2))]
  print $ validMoves (f state' 0)
  print $ validMoves (f state' 1)
  let actions = getAction state' agents
  print actions
  
  return $ step state' agents


fromMaybeJSON :: (FromJSON a) => Maybe a -> a
fromMaybeJSON Nothing = error "decoding error"
fromMaybeJSON (Just s) = s

encodeState :: State -> B.ByteString
encodeState = encode . toStateJSON

decodeState :: B.ByteString -> State
decodeState = fromStateJSON . fromMaybeJSON . decode

prettystate :: State -> String
prettystate state = B.unpack $ encodePretty $ toStateJSON state


getFalse :: String -> String -> String
getFalse [] acc = reverse acc
getFalse (s:xs) acc =
  case s of
    'f' ->
      let
        alse = take 4 xs
      in
        if (alse == "alse")
        then (getFalse (drop 4 xs) ("\"eslaf\"" ++ acc))
        else (getFalse xs (s : acc))
    otherwise -> getFalse xs (s : acc)


loop = do
  line <- getLine
  let state = decodeState $ B.pack $ getFalse line ""
  tree <- runMCTS 50 (tonode state)
  let move = pickmove (tree, (mkStdGen 137))
  putStrLn $ toShowAction state move
  hFlush stdout
  loop
  
main = loop


{-
s :: String
s = "{\"day\":3,\"phase\":{\"attacker\":1,\"attacker_card\":1,\"name\":\"attacking\"},\"player\":0,\"players\":[{\"buys\":1,\"cards\":[{\"name\":\"Sorcerer's Stipend\",\"uses\":0},{\"name\":\"Magic Bean Stock\",\"uses\":0}],\"coins\":0},{\"buys\":1,\"cards\":[{\"name\":\"Sorcerer's Stipend\",\"uses\":0},{\"name\":\"Worker\",\"uses\":1},{\"name\":\"Gold Fish\",\"uses\":0}],\"coins\":4}],\"shop\":{\"Board of Monopoly\":4,\"Bubble\":2,\"Ghost\":4,\"Gold Fish\":1,\"Incantation\":6,\"Magic Bean Stock\":1,\"Senior Worker\":4,\"Worker\":3}}"

diff :: String -> String -> String
diff s1 s2 = [if elem x s2 then '_' else x | x <- s1]
-}
{-
{"day":3,
"phase":{"attacker":1,"attacker_card":1,"name":"attacking"},
"player":0,
"players":[
{"buys":1,"cards":[{"name":"Sorcerer's Stipend","uses":0},{"name":"Magic Bean Stock","uses":0}],"coins":0},
{"buys":1,"cards":[{"name":"Sorcerer's Stipend","uses":0},{"name":"Worker","uses":1},{"name":"Gold Fish","uses":0}],"coins":4}],
"shop":
{"Board of Monopoly":4,
"Bubble":2,
"Ghost":4,
"Gold Fish":1,
"Incantation":6,
"Magic Bean Stock":1,
"Senior Worker":4,
"Worker":3}}
-}
{-
inv :: StateJSON
inv =
  let
    phase = AttackingJSON "attacking" 1 (Index 1)
    card1 = CardJSON "Sorcerer's Stipend" 0
    card2 = CardJSON "Worker" 0
    card3 = CardJSON "Gold Fish" 0
    card4 = CardJSON "Magic Bean Stock" 0
    player1 = PlayerJSON 0 1 [card1, card4]
    player2 = PlayerJSON 4 1 [card1, card2, card3]
    shop = Dict.fromList [("Board of Monopoly", 4),
                          ("Bubble", 2),
                          ("Ghost", 4),
                          ("Gold Fish", 1),
                          ("Incantation", 6),
                          ("Magic Bean Stock", 1),
                          ("Senior Worker", 4),
                          ("Worker", 3)]
  in
    StateJSON 3 (AttackingJ phase) shop [player1, player2] 0
    
sdec :: Maybe StateJSON
sdec = decode $ B.pack $ getFalse s ""

inv' :: Maybe StateJSON
inv' = decode $ encode inv

svalue :: Maybe Value
svalue = decode $ B.pack $ getFalse s ""

main = do
  print sdec
--  print $ decodeFromValue svalue
-}
