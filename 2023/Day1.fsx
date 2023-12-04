let tracePrint format a =
    printfn format a
    a

let (|Regex|_|) pattern str =
    let m = System.Text.RegularExpressions.Regex.Match(str, pattern)

    if m.Success then
        Some(List.tail [ for x in m.Groups -> x.Value ])
    else
        None

let exampleInput = [ 
    "2abc2"
    "pqr3stu8vwx"
    "a1b2c3d4e5f"
    "treb7uchet"]

let lineCalibration = function
    | Regex "^.*(\d).*(\d)$" [first; last] -> first
    | line -> failwith "Could not find calibration on line: %s" line

exampleInput
|> List.map lineCalibration