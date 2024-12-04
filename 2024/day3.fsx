#load "Shared.fsx"

open Shared.Parser

let input = "./day3_input.txt" |> System.IO.File.ReadAllText

let example =
    "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

let parseMul = pstring "mul(" >>. pint .>> pchar ',' .>>. pint .>> pchar ')'

let parseMuls = many (skipManyTill1 parseMul)

input
|> run parseMuls
|> function
    | Success(matches, _) -> matches
    | Failure err -> failwith err
|> List.map (fun (x, y) -> x * y)
|> List.sum
|> printfn "Day 3a : %i"


type Instruction =
    | Mul of (int * int)
    | Dont
    | Do

let parseMulInstr = parseMul |>> Mul
let parseDo = pstring "do()" |>> (fun _ -> Do)
let parseDont = pstring "don't()" |>> (fun _ -> Dont)

let parseInstructions =
    many (skipManyTill1 (parseMulInstr <|> parseDo <|> parseDont))

let filterInstructions (state, acc) inst =
    match inst with
    | Mul m ->
        match state with
        | Dont -> (state, acc)
        | _ -> (state, m :: acc)
    | i -> (i, acc)

input
|> run parseInstructions
|> function
    | Success(instructions, _) -> instructions
    | Failure err -> failwith err
|> List.fold filterInstructions (Do, [])
|> snd
|> List.map (fun (x, y) -> x * y)
|> List.sum
|> printfn "Day 3b : %i"
