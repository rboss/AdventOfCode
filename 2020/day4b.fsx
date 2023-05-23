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

type Passport = Map<string, string>

type Unit =
    | Centimeters of int
    | Inches of int

module Validation =

    let toDigit (str: string) =
        match System.Int32.TryParse str with
        | true, d -> Ok d
        | false, _ -> Error(sprintf "%s not a digit" str)

    let matchHeight (str: string) =
        match str with
        | Regex "^(\d+)cm$" [ cm ] -> Ok(Centimeters(int cm))
        | Regex "^(\d+)in$" [ inch ] -> Ok(Inches(int inch))
        | _ -> Error(sprintf "%s not correct height" str)

    let _eyeColors = Set.ofList [ "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" ]

    let validEyecolor color =
        if Set.contains color _eyeColors then
            Ok ""
        else
            Error "eye color not valid"

    let between min max digit =
        if digit >= min && digit <= max then
            Ok digit
        else
            Error(sprintf "%d outside range (%d-%d)" digit min max)


module ResultX =
    let map f xResult =
        match xResult with
        | Ok x -> Ok(f x)
        | Error errs -> Error errs

    let bind f xResult =
        match xResult with
        | Ok x -> f x
        | Error e -> Error e

    type ResultBuilder() =
        member this.Return x = Ok x
        member this.Bind(x, f) = bind f x

    let result = new ResultBuilder()

let parseInput input : Passport list =
    let rec parseRow rows acc =
        match rows with
        | [] -> acc
        | "" :: tail -> parseRow tail (Map.empty<string, string> :: acc)
        | row :: tail ->
            let newHead =
                row.Split(" ")
                |> Array.map (fun item -> item.Split(":"))
                |> Array.fold (fun state [| field; value |] -> Map.add field value state) acc.Head

            parseRow tail (newHead :: acc.Tail)

    parseRow input [ Map.empty<string, string> ]

let findField field (passport: Passport) =
    match Map.tryFind field passport with
    | Some value -> Ok value
    | None -> Error(sprintf "missing field %A" field)

let byrValidator passport =
    ResultX.result {
        let! field = findField "byr" passport
        let! digit = Validation.toDigit field
        let! _ = Validation.between 1920 2002 digit
        return passport
    }

let iyrValidator passport =
    ResultX.result {
        let! field = findField "iyr" passport
        let! digit = Validation.toDigit field
        let! _ = Validation.between 2010 2020 digit
        return passport
    }

let eyrValidator passport =
    ResultX.result {
        let! field = findField "eyr" passport
        let! digit = Validation.toDigit field
        let! _ = Validation.between 2020 2030 digit
        return passport
    }

let hgtValidator passport =
    ResultX.result {
        let! field = findField "hgt" passport
        let! height = Validation.matchHeight field

        let! _ =
            match height with
            | Centimeters cm -> Validation.between 150 193 cm
            | Inches inch -> Validation.between 59 76 inch

        return passport
    }

let hclValidator passport =
    ResultX.result {
        let! field = findField "hcl" passport

        let! _ =
            match field with
            | Regex "^#[a-f0-9]{6}$" [] -> Ok ""
            | _ -> Error(sprintf "Invalid haircolor %s" field)

        return passport
    }

let eclValidator passport =
    let eyeColors = Set.ofList [ "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" ]

    ResultX.result {
        let! field = findField "ecl" passport
        let! _ = Validation.validEyecolor field
        return passport
    }

let pidValidator passport =
    ResultX.result {
        let! field = findField "pid" passport

        let! _ =
            match field with
            | Regex "^[0-9]{9}$" [] -> Ok ""
            | _ -> Error(sprintf "Invalid passport id %s" field)

        return passport
    }

let validInput =
    Array.toList
        [| "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980"
           "hcl:#623a2f"
           ""
           "eyr:2029 ecl:blu cid:129 byr:1989"
           "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
           ""
           "hcl:#888785"
           "hgt:164cm byr:2001 iyr:2015 cid:88"
           "pid:545766238 ecl:hzl"
           "eyr:2022"
           ""
           "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719" |]

let invalidInput =
    Array.toList
        [| "eyr:1972 cid:100"
           "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"
           ""
           "iyr:2019"
           "hcl:#602927 eyr:1967 hgt:170cm"
           "ecl:grn pid:012533040 byr:1946"
           ""
           "hcl:dab227 iyr:2012"
           "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"
           ""
           "hgt:59cm ecl:zzz"
           "eyr:2038 hcl:74454a iyr:2023"
           "pid:3556412378 byr:2007" |]

let validatePassport passport =
    let (>>=) x f = Result.bind f x

    let valid =
        (Ok passport)
        >>= byrValidator
        >>= iyrValidator
        >>= eyrValidator
        >>= hgtValidator
        >>= hclValidator
        >>= eclValidator
        >>= pidValidator

    valid

let validatePassports passports = passports |> List.map validatePassport

byrValidator (Map.ofList [ "byr", "2002" ])
byrValidator (Map.ofList [ "byr", "2003" ])
hgtValidator (Map.ofList [ "hgt", "60in" ])
hgtValidator (Map.ofList [ "hgt", "190cm" ])
hgtValidator (Map.ofList [ "hgt", "190in" ])
hgtValidator (Map.ofList [ "hgt", "190" ])
hclValidator (Map.ofList [ "hcl", "#123abc" ])
hclValidator (Map.ofList [ "hcl", "#123abz" ])
hclValidator (Map.ofList [ "hcl", "123abc" ])
eclValidator (Map.ofList [ "ecl", "brn" ])
eclValidator (Map.ofList [ "ecl", "wat" ])
pidValidator (Map.ofList [ "pid", "000000001" ])
pidValidator (Map.ofList [ "pid", "0123456789" ])


input
|> parseInput
|> validatePassports
|> List.filter Result.isOk
|> List.length
|> tracePrint "problem2 %A" // 205
