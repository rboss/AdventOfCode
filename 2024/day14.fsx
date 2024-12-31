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

    (tileCoord width (x + vx * seconds), tileCoord height (y + vy * seconds)), (vx, vy)

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
|> Array.map fst
|> splitQuadrants tileWidth tileHeight
|> Array.map (Array.length)
|> Array.reduce (*)
|> printfn "Day 14a : %i"

let repeatApply f data =
    seq {
        let mutable result = data

        for i in 1 .. System.Int32.MaxValue do
            result <- (Array.map f result)
            yield i, result
    }

let countSequentialNum (numbers: int seq) : int =
    numbers
    |> Seq.sort
    |> Seq.fold (fun (prev, acc) curr -> (curr, (if curr = prev + 1 then acc else acc + 1))) (System.Int32.MinValue, 0)
    |> snd

let sequentialScore (cords: (int * int) array) =
    let xSeqential =
        cords
        |> Array.groupBy fst
        |> Array.map (fun (_, cords) -> cords |> Array.map snd |> countSequentialNum)

    let ySequential =
        cords
        |> Array.groupBy snd
        |> Array.map (fun (_, cords) -> cords |> Array.map fst |> countSequentialNum)

    Array.concat [ xSeqential; ySequential ] |> Array.map float |> Array.average
//

let magic_number = 4.0

input
|> Array.map parseRobotRow
|> repeatApply (simulate tileWidth tileHeight 1)
|> Seq.skipWhile (fun (i, cords) -> cords |> Array.map fst |> sequentialScore >= magic_number)
|> Seq.take 1
|> Seq.map fst
|> Seq.head
|> printfn "Day 14b : %i"
