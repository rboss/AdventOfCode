#load "Shared.fsx"

open Shared

let exampleInput = [|
    "1abc2"
    "pqr3stu8vwx"
    "a1b2c3d4e5f"
    "treb7uchet"|]

let calibrateLine line =
    let numbers =
        line
        |> Seq.filter (fun c -> c >= '0' && c <= '9')
    
    Seq.head numbers, Seq.last numbers

let calibrate rows =
    rows
    |> Seq.map calibrateLine
    |> Seq.map (fun (f, l) -> f.ToString() + l.ToString())
    |> Seq.map (int)
    |> Seq.reduce (+)

exampleInput
|> calibrate
|> tracePrint "Day1.ex : %i"

let input = System.IO.File.ReadAllLines "day1_input.txt" 

input
|> calibrate
|> tracePrint "Day1.1 : %i"


let exampleInput2 = [|
    "two1nine"
    "eightwothree"
    "abcone2threexyz"
    "xtwone3four"
    "4nineeightseven2"
    "zoneight234"
    "7pqrstsixteen"
|]

open Parser

let words = 
    [| "zero"; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"; |] 
    |> Array.mapi (fun value word -> value, word)

let wordCombinations = 
    Array.allPairs words words
    |> Array.filter (fun ((value1, word1), (value2, word2)) -> word1.EndsWith(word2[0..0]))
    |> Array.map (fun ((value1, word1), (value2, word2)) -> 
        pstring (word1 + word2[1..]) |>> (fun _ -> Some ([value1; value2])))
    |> Array.toList

let someWordNumber = 
    words
    |> Array.map (fun ( value, word ) -> pstring word |>> (fun _ -> Some [value])  )
    |> Array.toList

let someInt = anyOf ['0'..'9'] |>> (fun v -> Some [int (v.ToString())])

let skipAnyChar = pany |>> (fun _ -> None)

let pIntOrSpelled = (choice wordCombinations) <|> (choice someWordNumber) <|> someInt

let pIntegers = 
    (many1 (pIntOrSpelled <|> skipAnyChar)) 
    |>> (List.choose id >> List.collect id)
    
exampleInput2
|> Array.map (Parser.run pIntegers)
|> Array.map ( function
    | Success ( numbers, _) ->
        (List.head numbers, List.last numbers)
    | Failure err -> 
        failwithf "failed parsing line err: %s" err)
|> Seq.map (fun (f, l) -> f.ToString() + l.ToString())
|> Seq.toList
|> Seq.map (int)
|> Seq.reduce (+)
|> tracePrint "Day1.2ex : %i"

    
input
|> Array.map (Parser.run pIntegers)
|> Array.map ( function
    | Success ( numbers, _) ->
        (List.head numbers, List.last numbers)
    | Failure err -> 
        failwithf "failed parsing line err: %s" err)
|> Seq.map (fun (f, l) -> f.ToString() + l.ToString())
|> Seq.toList
|> Seq.map (int)
|> Seq.reduce (+)
|> tracePrint "Day1.2 : %i"