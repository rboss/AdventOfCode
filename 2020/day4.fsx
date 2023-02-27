#load "helpers.fsx"

open System
open Helpers

type FieldType =
    | Byr
    | Iyr
    | Eyr
    | Hgt
    | Hcl
    | Ecl
    | Pid
    | Cid

type Field =
    | Byr of string //(Birth Year)
    | Iyr of string //(Issue Year)
    | Eyr of string //(Expiration Year)
    | Hgt of string // (Height)
    | Hcl of string //(Hair Color)
    | Ecl of string //(Eye Color)
    | Pid of string //(Passport ID)
    | Cid of string //(Country ID)

let strToFieldType =
    function
    | "byr" -> FieldType.Byr
    | "iyr" -> FieldType.Iyr
    | "eyr" -> FieldType.Eyr
    | "hgt" -> FieldType.Hgt
    | "hcl" -> FieldType.Hcl
    | "ecl" -> FieldType.Ecl
    | "pid" -> FieldType.Pid
    | "cid" -> FieldType.Cid
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
        | "" :: tail -> parseRow tail (Set.empty<FieldType> :: acc)
        | row :: tail ->
            let newHead =
                row.Split(" ")
                |> Array.map (fun item ->
                    let [| fieldStr; value |] = item.Split(":")
                    strToFieldType fieldStr)
                |> Array.fold (fun state v -> Set.add v state) acc.Head

            parseRow tail (newHead :: acc.Tail)



    parseRow input [ Set.empty<FieldType> ]

let validSet =
    Set.ofList
        [ FieldType.Byr
          FieldType.Iyr
          FieldType.Eyr
          FieldType.Hgt
          FieldType.Hcl
          FieldType.Ecl
          FieldType.Pid ]

input
|> parseInput
|> List.filter (fun set -> Set.isSubset validSet set)
|> List.length
|> tracePrint "problem1 %i"