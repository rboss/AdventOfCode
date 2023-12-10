#load "Shared.fsx"

open Shared
open System

let exampleInput =
    [| "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
       "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
       "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
       "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
       "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
       "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11" |]

let input = System.IO.File.ReadAllLines "day4_input.txt"

let parseCard (row: string) =
    let numbersToSet (str: string) =
        str.Split(' ', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
        |> Set.ofArray

    match row with
    | Regex "^Card\s+(\d+): (.*) \| (.*)$" [ gameIdStr; winningStr; myStr ] ->
        let winningSet = numbersToSet winningStr
        let mySet = numbersToSet myStr

        (int) gameIdStr, winningSet, mySet
    | r -> failwithf "could not parse card row %s" r

let calculateMatches (_, winningSet, mySet) =
    Set.intersect winningSet mySet |> Set.count

let calculateScore matches = pown 2 (matches - 1)

let problem1 data =
    data
    |> Array.map parseCard
    |> Array.map calculateMatches
    |> Array.map calculateScore
    |> Array.sum

problem1 exampleInput |> tracePrint "Day4.1.ex: %i"

problem1 input |> tracePrint "Day4.1.1: %i"

let foldMatches (arr: int array) (gameId, winningSet, mySet) =
    let matches = Set.intersect winningSet mySet |> Set.count
    let cardCount = arr[gameId - 1]

    let stopIndex = min (gameId + matches - 1) (arr.Length - 1)

    for i in gameId..stopIndex do
        arr[i] <- arr[i] + cardCount
    arr

let problem2 data =
    data
    |> Array.map parseCard
    |> Array.fold foldMatches (Array.create data.Length 1)
    |> Array.sum

problem2 exampleInput |> tracePrint "Day4.2 ex: %i"

problem2 input |> tracePrint "Day4.2: %i"
