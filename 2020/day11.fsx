#load "helpers.fsx"

open Helpers

let input = System.IO.File.ReadAllLines "./day11_input.txt" |> Array.toList

let testInput =
    [ "L.LL.LL.LL"
      "LLLLLLL.LL"
      "L.L.L..L.."
      "LLLL.LL.LL"
      "L.LL.LL.LL"
      "L.LLLLL.LL"
      "..L.L....."
      "LLLLLLLLLL"
      "L.LLLLLL.L"
      "L.LLLLL.LL" ]

type SeatStatus =
    | Floor
    | Empty
    | Occupied

let seatOccupied =
    function
    | Occupied -> true
    | Floor
    | Empty -> false

let charToSeatStatus =
    function
    | '.' -> Floor
    | 'L' -> Empty
    | '#' -> Occupied
    | x -> failwithf "not a valid seat char %c" x

let toSeatStatusArray input =
    input
    |> List.map (Seq.toList >> List.map charToSeatStatus >> List.toArray)
    |> List.toArray
    |> array2D

let inputSeats = toSeatStatusArray input
let testSeats = toSeatStatusArray testInput

let deltas =
    [ (-1, -1); (0, -1); (1, -1); (-1, 0); (1, 0); (-1, 1); (0, 1); (1, 1) ]

let repeatDelta (y, x) (dy, dx) =
    seq { for i in 1..100 -> (y + (i * dy), x + (i * dx)) }

let findValidAdjacentSeat seatIsValid seats (y, x) (dy, dx) =
    let getSeat = Array2D.tryGet seats

    repeatDelta (y, x) (dy, dx) |> Seq.map getSeat |> Seq.find seatIsValid

let getAllValidAdjacentsSeats isSeatValid seats y x =
    let nextValidAdjacentSeat = findValidAdjacentSeat isSeatValid seats (y, x)
    deltas |> List.map nextValidAdjacentSeat |> List.choose id

let seatsAllEmpty seats =
    seats |> List.forall (seatOccupied >> not)

let seatsToMany seats limit =
    let occupied = seats |> List.filter seatOccupied |> List.length
    occupied >= limit

let calcNextSeatStatus isValidSeat maxOccupiedLimit =
    fun seats y x current ->
        let adjSeats = getAllValidAdjacentsSeats isValidSeat seats y x

        match current with
        | Empty when seatsAllEmpty adjSeats -> Occupied
        | Occupied when seatsToMany adjSeats maxOccupiedLimit -> Empty
        | _ -> current

let tickUntilStable isValidSeat maxOccupiedLimit initialSeats =
    let nextSeatStatus = calcNextSeatStatus isValidSeat maxOccupiedLimit

    let rec tick seats =
        let nextSeatStatus = nextSeatStatus seats
        let nextSeats = seats |> Array2D.mapi nextSeatStatus
        if nextSeats = seats then nextSeats else tick nextSeats

    tick initialSeats

let anySeatValid _ = true

tickUntilStable anySeatValid 4 inputSeats
|> Array2D.flatten
|> Array.filter seatOccupied
|> Array.length
|> tracePrint "problem1: %i" //2468

let isSeatOrOutofBounds =
    function
    | None
    | Some Empty
    | Some Occupied -> true
    | Some Floor -> false

tickUntilStable isSeatOrOutofBounds 5 inputSeats
|> Array2D.flatten
|> Array.filter seatOccupied
|> Array.length
|> tracePrint "problem2: %i" //2214
