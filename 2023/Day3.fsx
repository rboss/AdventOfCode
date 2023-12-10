#load "Shared.fsx"

open Shared

let input = System.IO.File.ReadAllLines "day3_input.txt"

let exampleInput =
    [| "467..114.."
       "...*......"
       "..35..633."
       "......#..."
       "617*......"
       ".....+.58."
       "..592....."
       "......755."
       "...$.*...."
       ".664.598.." |]

let inputIndexed = Array.indexed input

let getAdjacentPos set (y, x) =
    [ (-1, -1); (0, -1); (1, -1); (-1, 0); (1, 0); (-1, 1); (0, 1); (1, 1) ]
    |> List.fold (fun innerSet (dy, dx) -> Set.add (y + dy, x + dx) innerSet) set

let getRowSymbols criteria (rowIndex, row) =
    row
    |> Seq.indexed
    |> Seq.choose (fun (colIndex, char) -> if criteria char then Some(rowIndex, colIndex) else None)
    |> Seq.toArray

let isNotDigitOrDot char =
    char <> '.' && not (System.Char.IsDigit(char))

let symbolsAdjacentPos =
    inputIndexed
    |> Array.map (getRowSymbols isNotDigitOrDot)
    |> Array.collect id
    |> Array.fold getAdjacentPos Set.empty

let findNumbers (rowIndex, line) =
    line
    |> Seq.indexed
    |> Seq.fold
        (fun state (i, c) ->
            if System.Char.IsDigit(c) then
                match state with
                | (indexes, str) :: rest -> ((rowIndex, i) :: indexes, str + c.ToString()) :: rest
                | [] -> [ [ (rowIndex, i) ], c.ToString() ]
            else
                match state with
                | [] -> state
                | ([], "") :: rest -> state
                | _ -> ([], "") :: state)
        []

let findNumbersRegex (rowIndex, line) =
    let matches = System.Text.RegularExpressions.Regex.Matches(line, "(\d+)")

    matches
    |> Seq.map (fun res ->
        let set =
            [ res.Index .. res.Index + res.Length - 1 ]
            |> List.map (fun v -> (rowIndex, v))
            |> Set.ofList

        set, res.Value)
    |> Seq.toArray

let numbers_old =
    inputIndexed |> Array.map findNumbers |> Array.toList |> List.collect id

let numbers = inputIndexed |> Array.map findNumbersRegex |> Array.collect id

numbers_old
|> List.filter (fun (positions, _) -> Set.intersect symbolsAdjacentPos (Set.ofList positions) |> Set.isEmpty |> not)
|> List.map (fun (_, str) -> (int) str)
|> List.sum
|> tracePrint "Day1.1: %i"

numbers
|> Array.filter (fun (numberPosSet, _) -> not (Set.isEmpty (Set.intersect symbolsAdjacentPos numberPosSet)))
|> Array.map (fun (_, str) -> (int) str)
|> Array.sum
|> tracePrint "Day1.1: %i"


let isStar char = char = '*'

let starPositions =
    inputIndexed |> Array.map (getRowSymbols isStar) |> Array.collect id

let findAdjacentNumbers setNumbers pos =
    let starSet = getAdjacentPos Set.empty pos

    setNumbers
    |> Array.choose (fun (posSet, str) ->
        if Set.isEmpty (Set.intersect posSet starSet) then
            None
        else
            Some str)

starPositions
|> Array.map (findAdjacentNumbers numbers)
|> Array.filter (fun list -> Array.length list = 2)
|> Array.map (fun ([| left; right |]) -> (int) left * (int) right)
|> Array.sum
|> tracePrint "Day2.2: %i"
