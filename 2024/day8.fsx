let input =
    "./day8_input.txt" |> System.IO.File.ReadAllLines |> Array.map Seq.toArray

let antennas =
    input
    |> Array.mapi (fun y row -> row |> Array.mapi (fun x v -> (y, x), v))
    |> Array.collect id
    |> Array.filter (fun (_, c) -> c <> '.')

let groupByFreq list =
    list
    |> Seq.groupBy snd
    |> Seq.map snd
    |> Seq.map (fun freqAntennas -> freqAntennas |> Seq.map fst |> Seq.toList)

let getFrequencyAntinodes calcF antennas =
    antennas
    |> Seq.allPairs antennas
    |> Seq.filter (fun (pos1, pos2) -> pos1 <> pos2)
    |> Seq.collect calcF

let max = input.Length
let filterPosInMap (y, x) = x >= 0 && x < max && y >= 0 && y < max

let calcSingleAntinodes ((y1, x1), (y2, x2)) =
    [ (y1 + (y1 - y2), x1 + (x1 - x2)); (y2 + (y2 - y1), x2 + (x2 - x1)) ]

antennas
|> groupByFreq
|> Seq.collect (getFrequencyAntinodes calcSingleAntinodes)
|> Seq.filter filterPosInMap
|> Set.ofSeq
|> Set.count
|> printfn "Day 8a : %i"

let calcRepeatingAntinodes (pos1, pos2) =
    let inifiniteSeq ((y1, x1), (y2, x2)) =
        let yDelta = (y1 - y2)
        let xDelta = (x1 - x2)

        Seq.initInfinite (fun i -> (y1 + (i * yDelta), x1 + (i * xDelta)))
        |> Seq.skip 1
        |> Seq.takeWhile filterPosInMap

    seq {
        yield! inifiniteSeq (pos1, pos2)
        yield! inifiniteSeq (pos2, pos1)
    }

antennas
|> groupByFreq
|> Seq.collect (getFrequencyAntinodes calcRepeatingAntinodes)
|> Set.ofSeq
|> Set.union ((antennas |> Seq.map fst |> Set.ofSeq))
|> Set.count
|> printfn "Day 8b : %i"
