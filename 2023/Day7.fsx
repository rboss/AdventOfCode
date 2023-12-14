#load "Shared.fsx"

open System
open Shared

let exampleInput =
    [| "32T3K 765"; "T55J5 684"; "KK677 28"; "KTJJT 220"; "QQQJA 483" |]

type Card =
    | Ace
    | King
    | Queen
    | Jack
    | Ten
    | Nine
    | Eight
    | Seven
    | Six
    | Five
    | Four
    | Three
    | Two

type Hand = Card list

type Rank =
    | FiveOfAKind of Hand
    | FourOfAKind of Hand
    | FullHouse of Hand
    | ThreeOfAKind of Hand
    | TwoPairs of Hand
    | OnePair of Hand
    | HighCard of Hand

type Bid = int
type Play = Hand * Bid

let cards = [ Two; King; Ace; Ten; Three ]

[ [ Five; King ]; [ Queen; Ten ]; [ Queen; Two ] ] |> List.sort

// "KK677" |> parseHand // TwoPairs (King * King * Six * Seven * Seven)

open Parser
// let pFiveOfAKind =
let pcard =
    [ 'A', Ace
      'K', King
      'Q', Queen
      'J', Jack
      'T', Ten
      '9', Nine
      '8', Eight
      '7', Seven
      '6', Six
      '5', Five
      '4', Four
      '3', Three
      '2', Two ]
    |> List.map (fun (char, res) -> pchar char |>> (fun _ -> res))
    |> choice

let phand =
    pcard .>>. pcard .>>. pcard .>>. pcard .>>. pcard
    |>> (fun ((((A, B), C), D), E) -> [A; B; C; D; E])

Parser.run phand "QQQJA"

let hand = [ Queen; Queen; Queen; Jack; Ace ]
List.distinctBy id hand

let v =
    match List.countBy id hand with 
    | [_] -> FiveOfAKind hand
    | [(_, a); (_, b)] when a = 4 || b = 4 -> FourOfAKind hand
    | [(_, a); (_, b)] when a = 3 || b = 3 -> FullHouse hand
    | [(_, _); (_, _)] -> TwoPairs hand
    | [(_, a); (_, b); (_, c)] when a = 3 || b = 3 || c = 3 -> ThreeOfAKind hand
    | [(_); (_); (_); (_)] -> OnePair hand
    | _  -> HighCard hand
