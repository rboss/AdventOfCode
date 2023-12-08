#load "Shared.fsx"

open Shared

let exampleInput = [|
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
|]

let input = System.IO.File.ReadAllLines "day2_input.txt" 

open Parser

let lessThan max value = 
    let innerFn input = 
        if value <= max then 
            Success(value, input)
        else 
            Failure(sprintf "Value bigger than %i" max)

    Parser innerFn
    
let pGameNr = pstring "Game " >>. pint .>> pchar ':'
let pColor color = 
    opt (pchar ',' <|> pchar ';') >>. whitespaceChar >>. pint .>> whitespaceChar .>> pstring color
let pGreen = pColor "green" >>= lessThan 13
let pBlue = pColor "blue" >>= lessThan 14
let pRed = pColor "red"  >>= lessThan 12

let pGrab = choice [pGreen; pBlue; pRed]

let pGame = pGameNr .>> (many1 pGrab)

Parser.run pGame "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
Parser.run pGame "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

input 
|> Array.map (Parser.run pGame) 
|> Array.map ( function 
    | Success (v, "") -> v
    | _ -> 0)
|> Array.sum


