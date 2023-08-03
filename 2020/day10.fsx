#load "helpers.fsx"

open Helpers

let input = 
    System.IO.File.ReadAllLines "./day10_input.txt" 
    |> Array.toList 
    |> List.map (int)

let testInput = [
    16
    10
    15
    5
    1
    11
    7
    19
    6
    12
    4
]

let testInput2 = [
    28; 33; 18; 42; 31; 14; 46; 20; 48; 47; 24; 23; 49; 45; 19; 38; 39; 11; 1; 32; 25; 35; 8; 17; 7; 9; 4; 2; 34; 10; 3
]


let calcJoltDifferenceMultiplier adapters =
    0 :: ( List.max adapters + 3) :: adapters
    |> List.sort
    |> List.pairwise
    |> List.map (fun (l, r) -> r - l)
    |> List.groupBy id
    |> List.map (fun (_, list) -> List.length list)
    |> List.reduce (*)

calcJoltDifferenceMultiplier input

let calculateVariations initAdapters =
    let cache = System.Collections.Generic.Dictionary<_, _>()
    let rec calcVariations adapters current = 
        match cache.TryGetValue current with
        | true, value -> 
            value
        | false, _ -> 
            let result = 
                match adapters with 
                | [] -> 
                    0UL
                | fst :: snd :: thrd :: res when current + 3 >= thrd -> 
                    2UL + calcVariations ( snd :: thrd :: res ) fst 
                    + calcVariations ( thrd :: res ) snd 
                    + calcVariations ( res ) thrd 
                | fst :: snd :: res when current + 3 >= snd -> 
                    1UL + calcVariations ( snd :: res ) fst 
                    + calcVariations ( res ) snd 

                | fst :: res when current + 3 >= fst -> 
                    calcVariations ( res ) fst 
                | x -> 
                    failwithf "not found another match, got %A" x
            cache.Add(current, result)
            result

    calcVariations initAdapters 0


( List.max input + 3) :: input
|> List.sort
|> calculateVariations 
|> ( + ) 1UL
|> tracePrint "problem2: %u" //21156911906816