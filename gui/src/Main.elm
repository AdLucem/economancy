

module Main exposing
            (Phase, State)

type Day = Day1 | Day2 | Day3

type alias Card = {name : String,
                   uses : Int}

cardnames : List String
cardnames = ["Sorcerer's Stipend", "Board of Monopoly", "Incantation", "Worker", "Magic Bean Stock", "Bubble", "Ghost", "Senior Worker", "Gold Fish"]

type alias Player = {coins : Int,
                     buys  : Int,
                     cards : List Card}


type Phase = Investing
           | Attacking {attacker : Int, card : Maybe Card}
           | Buy
           | End {winner : Maybe Int}

type Shop = Dict String Int

type alias State = {day : Day, phase : Phase, shop : Shop,
                    players : List Player, playerIndex : Int}