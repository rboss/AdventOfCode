open System


let input = System.IO.File.ReadAllLines "./day1_input"

let tracePrint format a =
    printfn format a
    a

input
|> Array.windowed 2
|> Array.filter (fun [| l; r |] -> r > l)
|> Array.length
|> tracePrint "problem1: %A"
