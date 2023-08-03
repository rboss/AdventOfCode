#load "helpers.fsx"

open System
open Helpers

let input = System.IO.File.ReadAllLines "./day12_input.txt" |> Array.toList
let testInput = [ "F10"; "N3"; "F7"; "R90"; "F11" ]

type Position =
    { x: int
      y: int }

    static member (+)(pos1: Position, pos2: Position) =
        { x = pos1.x + pos2.x
          y = pos1.y + pos2.y }

    static member (*)(pos1: Position, times: int) =
        { x = pos1.x * times
          y = pos1.y * times }

type Degree =
    { deg: int }

    static member (+)(degree: Degree, rotateDeg: int) =
        { deg = (degree.deg + rotateDeg % 360) |> Math.Abs }

    static member (-)(degree: Degree, rotateDeg: int) =
        { deg = (degree.deg - rotateDeg % 360) |> Math.Abs }

let rotateRight { y = y; x = x } = { y = x; x = y * -1 }
let rotateLeft { y = y; x = x } = { y = x * -1; x = y }

type Vector = Position

type Action =
    | Move of Vector
    | Left
    | Right
    | Forward

type Instruction = (Action * int)

let directionToPos ({ deg = deg }) =
    match deg % 360 with
    | 90 -> { y = 0; x = 1 }
    | 180 -> { y = 1; x = 0 }
    | 270 -> { y = 0; x = -1 }
    | 0 -> { y = -1; x = 0 }
    | x -> failwithf "incorrect direction degree %i" x

let parseRow (row: string) : Instruction =
    match row.[0], row.[1..] with
    | 'L', Integer v -> Left, v
    | 'R', Integer v -> Right, v
    | 'F', Integer v -> Forward, v
    | 'E', Integer v -> Move { y = 0; x = 1 }, v
    | 'S', Integer v -> Move { y = 1; x = 0 }, v
    | 'W', Integer v -> Move { y = 0; x = -1 }, v
    | 'N', Integer v -> Move { y = -1; x = 0 }, v
    | c, s -> failwithf "unknown instruction: %c%s" c s

let executeInstruction (position, degree) instruction =
    match instruction with
    | Forward, value ->
        let movePos = (degree |> directionToPos) * value
        (position + movePos, degree)
    | Move vector, times -> (position + vector * times, degree)
    | Left, rotateDeg -> position, degree - rotateDeg
    | Right, rotateDeg -> position, degree + rotateDeg

let initalPosDirection = ({ y = 0; x = 0 }, { deg = 90 })

input
|> List.map parseRow
|> List.fold executeInstruction initalPosDirection
|> fst
|> (fun ({ x = x; y = y }) -> Math.Abs x + Math.Abs y)
|> tracePrint "problem1: %i" //1533


let rotateDegrees rotateF position degrees =
    let times = degrees / 90
    { 1..times } |> Seq.fold (fun acc _ -> rotateF acc) position

let executeInstructionWaypoint ((position: Position), (waypoint: Position)) instruction =
    match instruction with
    | Forward, times -> (position + (waypoint * times), waypoint)
    | Move vector, times -> (position, waypoint + (vector * times))
    | Left, degrees -> position, (rotateDegrees rotateLeft waypoint degrees)
    | Right, degrees -> position, (rotateDegrees rotateRight waypoint degrees)

input
|> List.map parseRow
|> List.fold executeInstructionWaypoint ({ y = 0; x = 0 }, { y = -1; x = 10 })
|> fst
|> (fun ({ x = x; y = y }) -> Math.Abs x + Math.Abs y)
|> tracePrint "problem2: %i" //25235
