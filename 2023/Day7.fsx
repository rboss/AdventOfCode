let exampleInput =
    [| "32T3K 765"; "T55J5 684"; "KK677 28"; "KTJJT 220"; "QQQJA 483" |]

let input = System.IO.File.ReadAllLines "day7_input.txt"

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
    |> Map.ofList

let parseCards str =
    str |> Seq.map (fun c -> Map.find c charToCardMap) |> Seq.toList

let parseHand cards =
    let countSorted = List.countBy id cards |> List.map snd |> List.sortDescending

    match countSorted with
    | [ 5 ] -> FiveOfAKind cards
    | [ 4; _ ] -> FourOfAKind cards
    | [ 3; _ ] -> FullHouse cards
    | [ 2; 2; _ ] -> TwoPairs cards
    | [ 3; _; _ ] -> ThreeOfAKind cards
    | [ 2; _; _; _ ] -> OnePair cards
    | _ -> HighCard cards

let parsePlay (row: string) : Play =
    let [| strHand; strBid |] = row.Split(' ', 2)

    let hand = strHand |> parseCards |> parseHand
    hand, int strBid

let problem1 data =
    data
    |> Array.map parsePlay
    |> Array.sortDescending
    |> Array.indexed
    |> Array.map (fun (index, (_, bid)) -> (index + 1) * bid)
    |> Array.sum

problem1 exampleInput |> printfn "Day7 1.ex: %i"
problem1 input |> printfn "Day7 1.1: %i"