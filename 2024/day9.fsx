let input = "./day9_input.txt" |> System.IO.File.ReadAllText |> Seq.map id

let charToInt = System.Globalization.CharUnicodeInfo.GetDigitValue

let calcChecksum disc =
    disc
    |> Array.mapi (fun i c -> if c <> "." then (int64 c) * (int64) i else 0)
    |> Array.sum

let expandPart i length =
    let sym = if i % 2 = 0 then string (i / 2) else "."
    [ for _ in [ 1 .. (charToInt) length ] -> sym ]

let unfoldDisc disc =
    disc |> Seq.mapi expandPart |> Seq.collect id |> Seq.toArray

let compressDisk disc =
    let mutable head = 0
    let mutable tail = (Array.length disc) - 1

    while head < tail do
        match disc[head], disc[tail] with
        | ".", "." -> tail <- tail - 1
        | _, "." ->
            head <- head + 1
            tail <- tail - 1
        | ".", _ ->
            disc[head] <- disc[tail]
            disc[tail] <- "."
            head <- head + 1
            tail <- tail - 1
        | _, _ -> head <- head + 1

    disc

input |> unfoldDisc |> compressDisk |> calcChecksum |> printfn "Day 9a : %i"

let toPart index (i, size) =
    (index, (if i % 2 = 0 then i / 2 else -1), size), index + size

let defragDisc discStr =
    let disc = unfoldDisc discStr

    let freeParts, fileList =
        let freeList, fileList =
            discStr
            |> Seq.map charToInt
            |> Seq.indexed
            |> Seq.mapFold toPart 0
            |> fst
            |> Seq.toList
            |> List.partition (fun (_, id, _) -> id = -1)

        List.toArray freeList, List.rev fileList

    let rec moveFile files =
        match files with
        | [] -> disc
        | (fileIndex, fileId, fileSize) :: tail ->
            match Array.tryFindIndex (fun (index, _, size) -> size >= fileSize && index < fileIndex) freeParts with
            | Some i ->
                let (freeIndex, _, freeSize) = freeParts[i]
                Array.fill disc freeIndex fileSize (string fileId)
                Array.fill disc fileIndex fileSize "."
                freeParts[i] <- (freeIndex + fileSize, -1, freeSize - fileSize)
                moveFile tail
            | None -> moveFile tail

    moveFile fileList

input |> defragDisc |> calcChecksum |> printfn "Day 9b : %i"
