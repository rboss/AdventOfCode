#load "helpers.fsx"

open Helpers

let testInput =
    Array.toList
        [| "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
           "byr:1937 iyr:2017 cid:147 hgt:183cm"
           ""
           "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
           "hcl:#cfa07d byr:1929"
           ""
           "hcl:#ae17e1 iyr:2013"
           "eyr:2024"
           "ecl:brn pid:760753108 byr:1931"
           "hgt:179cm"
           ""
           "hcl:#cfa07d eyr:2025 pid:166559648"
           "iyr:2011 ecl:brn hgt:59in" |]

let input = System.IO.File.ReadAllLines "./day4_input.txt" |> Array.toList

type Passport = Set<string>

let parseInput input : Passport list =
    let rec parseRow rows acc =
        match rows with
        | [] -> acc
        | "" :: tail -> parseRow tail (Set.empty<string> :: acc)
        | row :: tail ->
            let newHead =
                row.Split(" ")
                |> Array.map (fun item -> item.Split(":"))
                |> Array.map (fun [| str; _ |] -> str)
                |> Array.fold (fun state str -> Set.add str state) acc.Head

            parseRow tail (newHead :: acc.Tail)

    parseRow input [ Set.empty<string> ]

let requiredSet = Set.ofList [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ]

input
|> parseInput
|> List.filter (fun passport -> Set.isSubset requiredSet passport)
|> List.length
|> tracePrint "problem1 %i" //254
