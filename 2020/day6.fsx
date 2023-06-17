#load "helpers.fsx"

open Helpers
let input = System.IO.File.ReadAllLines "./day6_input.txt" |> Array.toList

type Answers = Set<char>
type GroupAnswers = Answers list

let parseAnswers answers : GroupAnswers list =
    let rec parseRow row acc =
        match row with
        | [] -> acc
        | "" :: tail -> parseRow tail ([] :: acc)
        | row :: rest ->
            let newHead = (row |> Set.ofSeq) :: acc.Head
            parseRow rest (newHead :: acc.Tail)

    parseRow answers [ [] ]

let groupUniqueAnswers group = group |> Set.unionMany |> Set.count

parseAnswers input
|> List.map groupUniqueAnswers
|> List.reduce (+)
|> tracePrint "problem1 %i"


let groupSameAnswers group =
    group |> List.reduce Set.intersect |> Set.count

parseAnswers input
|> List.map groupSameAnswers
|> List.reduce (+)
|> tracePrint "problem2 %i"
