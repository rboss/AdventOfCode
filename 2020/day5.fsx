#load "helpers.fsx"

open Helpers
let input = System.IO.File.ReadAllLines "./day5_input.txt" |> Array.toList

let rec charToBinary c =
    match c with
    | 'F' -> '0'
    | 'B' -> '1'
    | 'R' -> '1'
    | 'L' -> '0'
    | _ -> failwithf "unexpected char %c" c

let stringToBinary str =
    str |> List.map charToBinary |> List.map string |> List.fold (+) "0b"

let seatToRowColumn (seat: string) =
    let row = seat |> Seq.toList |> List.take 7 |> stringToBinary |> (int)

    let column = seat |> Seq.toList |> List.skip 7 |> stringToBinary |> (int)

    row, column

let score (row, column) = row * 8 + column

input
|> List.map seatToRowColumn
|> List.map score
|> List.max
|> tracePrint "problem1 %i"


input
|> List.map seatToRowColumn
|> List.map score
|> List.sort
|> List.windowed 2
|> List.skipWhile (fun ([ l; r ]) -> r - l = 1)
|> List.head
|> tracePrint "problem2 %A" //705
