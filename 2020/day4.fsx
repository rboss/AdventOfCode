#load "helpers.fsx"

open System
open Helpers

type Field =
    | Byr //(Birth Year)
    | Iyr //(Issue Year)
    | Eyr //(Expiration Year)
    | Hgt // (Height)
    | Hcl //(Hair Color)
    | Ecl //(Eye Color)
    | Pid //(Passport ID)
    | Cid //(Country ID)

let strToFieldType =
    function
    | "byr" -> Field.Byr
    | "iyr" -> Field.Iyr
    | "eyr" -> Field.Eyr
    | "hgt" -> Field.Hgt
    | "hcl" -> Field.Hcl
    | "ecl" -> Field.Ecl
    | "pid" -> Field.Pid
    | "cid" -> Field.Cid
    | str -> failwithf "unexpected fieldtype str: %s" str

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


let parseInput input =
    let rec parseRow rows acc =
        match rows with
        | [] -> acc
        | "" :: tail -> parseRow tail (Map.empty<Field, string> :: acc)
        | row :: tail ->
            let newHead =
                row.Split(" ")
                |> Array.map (fun item -> item.Split(":"))
                |> Array.map (fun [| str; value |] -> (strToFieldType str), value)
                |> Array.fold (fun state (field, value) -> Map.add field value state) acc.Head

            parseRow tail (newHead :: acc.Tail)



    parseRow input [ Map.empty<Field, string> ]

let requiredFields =
    [ Field.Byr; Field.Iyr; Field.Eyr; Field.Hgt; Field.Hcl; Field.Ecl; Field.Pid ]

let fieldExistsMap (field: Field) =
    Result.bind (fun (passportMap: Map<Field, string>) ->
        if Map.containsKey field passportMap then
            Ok passportMap
        else
            Error(sprintf "missing field: %A" field))

let requiredFieldsCheck =
    requiredFields |> List.map fieldExistsMap |> List.reduce (>>)
//286
input
|> parseInput
|> List.filter (fun map -> Ok map |> requiredFieldsCheck |> Result.isOk)
|> List.length
|> tracePrint "problem1 %i" //254
