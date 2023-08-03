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

module Array2D = 
    let tryGet (arr2d: 'a[,]) (y, x) =
        if (y >= 0 && x >= 0 && y < Array2D.length1 arr2d && x < Array2D.length2 arr2d) then
            Some arr2d[y,x]
        else
            None

    let flatten (arr2d: 'a[,]) = 
        arr2d |> Seq.cast<'a> |> Seq.toArray

