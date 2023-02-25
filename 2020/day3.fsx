#load "helpers.fsx"

open System
open Helpers

let testInput =
    Array.toList
        [| "..##......."
           "#...#...#.."
           ".#....#..#."
           "..#.#...#.#"
           ".#...##..#."
           "..#.##....."
           ".#.#.#....#"
           ".#........#"
           "#.##...#..."
           "#...##....#"
           ".#..#...#.#" |]

let input = System.IO.File.ReadAllLines "./day3_input.txt" |> Array.toList

let parseInput input =
    let rec parseRow rows acc =
        match rows with
        | [] -> acc |> List.rev |> List.toArray
        | row :: tail ->
            let rowSeq =
                seq {
                    while true do
                        for x in row -> x
                }

            parseRow tail (rowSeq :: acc)

    parseRow input []

let traverse step forest =
    let forestHeight = Array.length forest

    let isTree (x, y) =
        forest.[y - 1] |> Seq.skip (x - 1) |> Seq.head |> (=) '#'

    let rec move (x, y) acc =
        if y > forestHeight then
            acc
        else
            let newAcc = if isTree (x, y) then acc + 1 else acc
            let newPos = step (x, y)
            move newPos newAcc

    move (1, 1) 0

let step (xStep, yStep) = (fun (x, y) -> (x + xStep, y + yStep))

input 
|> parseInput 
|> traverse (step (3, 1)) 
|> tracePrint "problem1 %i" // 205

input
|> parseInput
|> (fun forest ->
    [ step (1, 1)
      step (3, 1)
      step (5, 1)
      step (7, 1)
      step (1, 2) ]
    |> List.map traverse
    |> List.map (fun traverse -> traverse forest))
    |> List.map uint
    |> List.fold (*) 1u
|> tracePrint "problem2 %A" // 205
