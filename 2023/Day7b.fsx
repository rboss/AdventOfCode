type Card =
    | Ace
    | King
    | Queen
    | Ten
    | Nine
    | Eight
    | Seven
    | Six
    | Five
    | Four
    | Three
    | Two
    | Joker

type Cards = Card list

type Hand =
    | FiveOfAKind of Cards
    | FourOfAKind of Cards
    | FullHouse of Cards
    | ThreeOfAKind of Cards
    | TwoPairs of Cards
    | OnePair of Cards
    | HighCard of Cards

type Bid = int
type Play = Hand * Bid

let charToCardMap =
    [ 'A', Ace
      'K', King
      'Q', Queen
      'J', Joker
      'T', Ten
      '9', Nine
      '8', Eight
      '7', Seven
      '6', Six
      '5', Five
      '4', Four
      '3', Three
      '2', Two ]
    |> Map.ofList

let parseCards str =
    str |> Seq.map (fun c -> Map.find c charToCardMap) |> Seq.toList

let parseHand cards =
    let noJokerCardsSorted =
        cards
        |> List.filter (fun card -> card <> Joker)
        |> List.countBy id
        |> List.map snd
        |> List.sortDescending

    match noJokerCardsSorted with
    | []
    | [ _ ] -> FiveOfAKind cards
    | [ _; 1 ] -> FourOfAKind cards
    | [ _; _ ] -> FullHouse cards
    | [ 2; 2; 1 ] -> TwoPairs cards
    | [ _; _; _ ] -> ThreeOfAKind cards
    | [ _; _; _; _ ] -> OnePair cards
    | _ -> HighCard cards

parseHand [ Queen; Queen; King; King; Joker ]

let parsePlay (row: string) : Play =
    let [| strHand; strBid |] = row.Split(' ', 2)

    let hand = strHand |> parseCards |> parseHand
    hand, int strBid

let problem2 data =
    data
    |> Array.map parsePlay
    |> Array.sortDescending
    |> Array.indexed
    |> Array.map (fun (index, (_, bid)) -> (index + 1) * bid)
    |> Array.sum

let exampleInput =
    [| "32T3K 765"; "T55J5 684"; "KK677 28"; "KTJJT 220"; "QQQJA 483" |]

problem2 exampleInput |> printfn "Day7 2.ex: %i"

let input = System.IO.File.ReadAllLines "day7_input.txt"
problem2 input |> (=) 249515436
