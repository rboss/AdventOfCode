
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
    let (jokers, rest) = List.partition (fun card -> card = Joker) cards
    let restCountSorted = List.countBy id rest |> List.map snd |> List.sortDescending

    match restCountSorted, jokers.Length with
    | [ _ ], 0 -> FiveOfAKind cards
    | [], 5 -> FiveOfAKind cards
    | [ _ ], _ -> FiveOfAKind cards

    | [ 4; _ ], 0 -> FourOfAKind cards
    | [ _; 1 ], _ -> FourOfAKind cards

    | [ 3; 2 ], 0 -> FullHouse cards
    | [ 2; 2 ], 1 -> FullHouse cards

    | [ 3; _; _ ], 0 -> ThreeOfAKind cards
    | [ _; _; _ ], x when x > 0 -> ThreeOfAKind cards

    | [ 2; 2; _ ], 0 -> TwoPairs cards

    | [ 2; _; _; _ ], 0 -> OnePair cards
    | [ _; _; _; _ ], 1 -> OnePair cards
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
problem2 input |> printfn "Day7 2: %i" // 249515436
