#load "helpers.fsx"

open Helpers

let testInput =
    Array.toList [| "1-3 a: abcde"; "1-3 b: cdefg"; "2-9 c: ccccccccc" |]

let input = System.IO.File.ReadAllLines "./day2_input" |> Array.toList

type PasswordRecord = (int * int * char * string)

let rec parseInput input acc : PasswordRecord list =
    match input with
    | [] -> acc
    | Regex "(\d+)-(\d+) ([a-z]): ([a-z]+)" [ min; max; letter; password ] :: tail ->
        let newAcc = (int min, int max, char letter, password) :: acc
        parseInput tail newAcc
    | row -> failwithf "unexpected input, got: %A" row

let isValidPassword ((min, max, letter, password): PasswordRecord) =
    password
    |> Seq.filter (fun c -> c = letter)
    |> Seq.length
    |> (fun count -> count >= min && count <= max)

parseInput input []
|> List.filter isValidPassword
|> List.length
|> tracePrint "problem1 %i"


let isValidPassword2 ((min, max, letter, password): PasswordRecord) =
    (password[min - 1] = letter) <> (password[max - 1] = letter)

parseInput input []
|> List.filter isValidPassword2
|> List.length
|> tracePrint "problem2 %i"
