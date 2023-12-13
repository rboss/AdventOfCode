#load "Shared.fsx"

open Shared


let exampleInput = System.IO.File.ReadAllText "day5_input_example.txt"
let input = System.IO.File.ReadAllText "day5_input.txt"

open Parser

type Alamanac =
    { seeds: Seeds
      seedToSoil: ConvertionMap
      soilToFertilizer: ConvertionMap
      fertilizerToWater: ConvertionMap
      waterToLight: ConvertionMap
      lightToTemperature: ConvertionMap
      temperatureToHumidity: ConvertionMap
      humidityToLocation: ConvertionMap }

and Seeds = int64 list
and ConvertionMap = int64 list list

let pnewline = pstring System.Environment.NewLine
let pnumbers = sepBy1 pint64 whitespaceChar
let pSeeds = pstring "seeds: " >>. pnumbers .>> pnewline .>> pnewline

let mappingParser title =
    pstring title >>. pnewline >>. sepBy1 pnumbers pnewline
    .>> pnewline
    .>> pnewline

let pSeedsToSoil = mappingParser "seed-to-soil map:"
let pSoilToFertilizer = mappingParser "soil-to-fertilizer map:"
let pFertilizerToWater = mappingParser "fertilizer-to-water map:"
let pWaterToLight = mappingParser "water-to-light map:"
let pLightToTemperature = mappingParser "light-to-temperature map:"
let pTemperatureToHumidity = mappingParser "temperature-to-humidity map:"

let pHumidityToLocation =
    pstring "humidity-to-location map:" >>. pnewline >>. sepBy1 pnumbers pnewline

let pAlmanac =
    pSeeds
    .>>. pSeedsToSoil
    .>>. pSoilToFertilizer
    .>>. pFertilizerToWater
    .>>. pWaterToLight
    .>>. pLightToTemperature
    .>>. pTemperatureToHumidity
    .>>. pHumidityToLocation
    |>> (fun (((((((seeds, toSoil), toFertilizer), toWater), toLight), toTemperature), toHumidity), toLocation) ->
        { seeds = seeds
          seedToSoil = toSoil
          soilToFertilizer = toFertilizer
          fertilizerToWater = toWater
          waterToLight = toLight
          lightToTemperature = toTemperature
          temperatureToHumidity = toHumidity
          humidityToLocation = toLocation })

// End parser

let defaultValue def opt = defaultArg opt def

let applyConvertion (convertionMap: ConvertionMap) number =
    convertionMap
    |> List.tryFind (fun [ _; source; steps ] -> number >= source && number < source + steps)
    |> Option.map (fun [ destination; source; _ ] -> number + (destination - source))
    |> defaultValue number

let calculateLocation almanac seed =
    seed
    |> applyConvertion almanac.seedToSoil
    |> applyConvertion almanac.soilToFertilizer
    |> applyConvertion almanac.fertilizerToWater
    |> applyConvertion almanac.waterToLight
    |> applyConvertion almanac.lightToTemperature
    |> applyConvertion almanac.temperatureToHumidity
    |> applyConvertion almanac.humidityToLocation

let problem1 data =
    let almanac =
        match Parser.run pAlmanac data with
        | Success(result, rest) -> result
        | Failure f -> failwith "failed parsing almanac %s" f

    almanac.seeds |> List.map (calculateLocation almanac) |> List.min

problem1 exampleInput |> tracePrint "Day5 1.ex: %i"
problem1 input |> tracePrint "Day5 1.1: %i"

let applyReverseConvertion (convertionMap: ConvertionMap) =

    fun number ->
        convertionMap
        |> List.tryFind (fun [ source; _; steps ] -> number >= source && number < source + steps)
        |> Option.map (fun [ source; destination; _ ] -> number + (destination - source))
        |> defaultValue number

let calculateSeed almanac location =
    location
    |> applyReverseConvertion almanac.humidityToLocation
    |> applyReverseConvertion almanac.temperatureToHumidity
    |> applyReverseConvertion almanac.lightToTemperature
    |> applyReverseConvertion almanac.waterToLight
    |> applyReverseConvertion almanac.fertilizerToWater
    |> applyReverseConvertion almanac.soilToFertilizer
    |> applyReverseConvertion almanac.seedToSoil


let isValidSeed seeds =
    let seedRanges = List.chunkBySize 2 seeds |> List.sortBy (fun [ start; _ ] -> start)

    (fun seed ->
        seedRanges
        |> List.exists (fun ([ start; steps ]) -> seed >= start && seed < (start + steps)))

let problem2 data =
    let almanac =
        match Parser.run pAlmanac data with
        | Success(result, _) -> result
        | Failure f -> failwith "failed parsing almanac %s" f

    let seed =
        seq [ 1L .. 100_000_000L ]
        |> Seq.map (calculateSeed almanac)
        |> Seq.find (isValidSeed almanac.seeds)

    calculateLocation almanac seed

#time
problem2 input
#time

// let almanac =
//     match Parser.run pAlmanac input with
//     | Success(result, rest) -> result
//     | Failure f -> failwith "failed parsing almanac %s" f
// List.chunkBySize 2 almanac.seeds |> List.sortBy (fun [start; _] -> start)
// calculateLocation almanac 504595750L
// calculateSeed almanac 26829166