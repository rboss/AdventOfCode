module Helpers

fsi.ShowDeclarationValues <- false

open System.Text.RegularExpressions

let tracePrint format a =
    printfn format a
    a

let (|Integer|_|) (i: string) =
    match System.Int32.TryParse i with
    | true, v -> Some v
    | false, _ -> None

let (|Regex|_|) pattern str =
    let m = Regex.Match(str, pattern)

    if m.Success then
        Some(List.tail [ for x in m.Groups -> x.Value ])
    else
        None
