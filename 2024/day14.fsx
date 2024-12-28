let example = "./day14_example.txt" |> System.IO.File.ReadAllLines
let input = "./day14_input.txt" |> System.IO.File.ReadAllLines

#load "Shared.fsx"

open Shared.Parser

let coordParser = pint .>> pchar ',' .>>. pint
let robotRowParser = pstring "p=" >>. coordParser .>> pstring " v=" .>>. coordParser

let parseRobotRow row =
    match run robotRowParser row with
    | Success(robot, _) -> robot
    | err -> failwith "could not parse row %A, %A" row err

let simulate width height seconds ((x, y), (vx, vy)) =
    let tileCoord length v =
        let coord = v % length
        if coord < 0 then length + coord else coord

    tileCoord width (x + vx * seconds), tileCoord height (y + vy * seconds)

let splitQuadrants width height (arr: (int * int) array) =
    let midW = width / 2
    let midH = height / 2
    let tupleToArray (left, right) = [| left; right |]

    arr
    |> Array.filter (fun (x, y) -> x <> midW && y <> midH)
    |> Array.partition (fun (x, _) -> x < midW)
    |> tupleToArray
    |> Array.collect (Array.partition (fun (_, y) -> y < midH) >> tupleToArray)

let tileWidth = 101
let tileHeight = 103

input
|> Array.map parseRobotRow
|> Array.map (simulate tileWidth tileHeight 100)
|> splitQuadrants tileWidth tileHeight
|> Array.map (Array.length)
|> Array.reduce (*)
