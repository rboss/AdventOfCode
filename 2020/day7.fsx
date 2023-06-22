#load "helpers.fsx"

open Helpers
let input = System.IO.File.ReadAllLines "./day7_input.txt" |> Array.toList

let testInput =
    [ "light red bags contain 1 bright white bag, 2 muted yellow bags."
      "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
      "bright white bags contain 1 shiny gold bag."
      "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
      "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
      "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
      "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
      "faded blue bags contain no other bags."
      "dotted black bags contain no other bags." ]

let parseContent =
    function
    | "no other bags." -> None
    | Regex "(\d+) (.+) bag[s]?" [ amount; bag ] -> Some(bag, amount)
    | cont -> failwithf "could not match bag content: %s" cont

let (|Bag|_|) str =
    match str with
    | Regex "^(.+) bags contain (.*)$" [ bag; content ] ->
        let innerBags = content.Split(",") |> Array.choose parseContent
        Some(Bag(bag, innerBags))
    | _ -> None

let rowToBag =
    function
    | Bag(bag, innerBags) -> innerBags |> Array.map (fun innerBag -> [ bag, innerBag ]) |> Array.toList
    | row -> failwithf "could not match row: %s" row

let changeBag newParentBag exist =
    match exist with
    | Some v -> Some(newParentBag :: v)
    | None -> Some [ newParentBag ]

let foldInnerOut acc [ bag, (innerBag, amount) ] = Map.change innerBag (changeBag bag) acc

let getInnerOutLookupMap str =
    str |> List.collect rowToBag |> List.fold foldInnerOut Map.empty

let rec lookupBags bagMap startingBag =
    let rec lookupNextBag acc bagsToSearch =
        match bagsToSearch with
        | bag :: rest ->
            let innerBags = bagMap |> Map.tryFind bag |> Option.defaultValue []
            lookupNextBag (bag :: acc) (innerBags @ rest)
        | [] -> acc

    lookupNextBag [] [ startingBag ]

let bagMap = getInnerOutLookupMap input

lookupBags bagMap "shiny gold"
|> Set.ofList
|> Set.count
|> (+) -1
|> tracePrint "problem1 %i" //197


let foldTopDown acc [ bag, (innerBag, amount) ] =
    Map.change bag (changeBag (innerBag, int amount)) acc

let getOuterInnerLookupMap str =
    str |> List.collect rowToBag |> List.fold foldTopDown Map.empty

let lookupBags2 (map: Map<string, list<string * int>>) startingBag =
    let rec lookupNextBag (nextBag, amount) =
        printfn "now: %s - %i" nextBag amount

        let innerScore =
            map
            |> Map.tryFind nextBag
            |> Option.defaultValue []
            |> List.map (fun (innerBag, innerAmount) ->
                innerAmount + innerAmount * lookupNextBag (innerBag, innerAmount))
            |> List.sum

        printfn "bag %s %i" nextBag innerScore
        innerScore

    lookupNextBag (startingBag, 1)

let bagMap2 = getOuterInnerLookupMap input
lookupBags2 bagMap2 "shiny gold"
