module Shared

fsi.ShowDeclarationValues <- false

let inline charToInt c = int c - int '0'

let parseInt (str: string) =
    match System.Int32.TryParse str with
    | true, v -> Ok v
    | false, _ -> Error [ str + " is not an integer" ]

let (|Regex|_|) pattern str =
    let m = System.Text.RegularExpressions.Regex.Match(str, pattern)

    if m.Success then
        Some(List.tail [ for x in m.Groups -> x.Value ])
    else
        None

let (|RegexInt|_|) pattern str =
    let m = System.Text.RegularExpressions.Regex.Match(str, pattern)

    // TODO: try with traversable
    let maybeInts = [ for x in Seq.tail m.Groups -> System.Int32.TryParse x.Value ]

    if m.Success then
        if (List.forall (fun (parsed, _) -> parsed) maybeInts) then
            Some(List.map (fun (_, intVal) -> intVal) maybeInts)
        else
            None
    else
        None

let tracePrint format a =
    printfn format a
    a

module Parser =

    open System

    type ParseResult<'a> =
        | Success of 'a
        | Failure of string

    type Parser<'T> = Parser of (string -> ParseResult<'T * string>)

    let satisfy predicate =
        let innerFn str =
            if String.IsNullOrEmpty(str) then
                Failure "No more input"
            else
                let first = str.[0]

                if predicate first then
                    let remaining = str.[1..]
                    Success(first, remaining)
                else
                    let msg = sprintf "Unexpected '%c'" first
                    Failure msg

        Parser innerFn

    let pchar charToMatch =
        let predicate ch = charToMatch = ch
        satisfy predicate

    let run parser input =
        // unwrap parser to get inner function
        let (Parser innerFn) = parser
        // call inner function with input
        innerFn input

    let andThen parser1 parser2 =
        let innerFn input =
            // run parser1 with the input
            let result1 = run parser1 input

            // test the result for Failure/Success
            match result1 with
            | Failure err -> Failure err
            | Success(value1, remaining1) ->
                // run parser2 with the remaining input
                let result2 = run parser2 remaining1

                // test the result for Failure/Success
                match result2 with
                | Failure err ->
                    // return error from parser2
                    Failure err

                | Success(value2, remaining2) ->
                    // combine both values as a pair
                    let newValue = (value1, value2)
                    // return remaining input after parser2
                    Success(newValue, remaining2)

        // return the inner function
        Parser innerFn

    let orElse parser1 parser2 =
        let innerFn input =
            let result1 = run parser1 input

            match result1 with
            | Success _ -> result1
            | Failure err ->
                let result2 = run parser2 input
                result2

        Parser innerFn

    let mapP f parser =
        let innerFn input =
            let result = run parser input

            match result with
            | Success(value, remaining) ->
                let newValue = f value
                Success(newValue, remaining)
            | Failure err -> Failure err

        Parser innerFn

    let (<|>) = orElse
    let (<!>) = mapP
    let (.>>.) = andThen
    let (|>>) x f = mapP f x

    let (.>>) p1 p2 =
        // create a pair
        p1 .>>. p2
        // then only keep the first value
        |> mapP (fun (a, b) -> a)

    let (>>.) p1 p2 =
        // create a pair
        p1 .>>. p2
        // then only keep the second value
        |> mapP (fun (a, b) -> b)

    let bindP f p =
        let innerFn input =
            let result1 = run p input

            match result1 with
            | Failure err ->
                // return error from parser1
                Failure err
            | Success(value1, remainingInput) ->
                // apply f to get a new parser
                let p2 = f value1
                // run parser with remaining input
                run p2 remainingInput

        Parser innerFn

    let (>>=) p f = bindP f p

    let choice listOfParsers = List.reduce (<|>) listOfParsers

    let anyOf listOfChars = listOfChars |> List.map pchar |> choice

    let applyP fP xP =
        // create a Parser containing a pair (f,x)
        (fP .>>. xP)
        // map the pair by applying f to x
        |> mapP (fun (f, x) -> f x)

    let returnP x =
        let innerFn input =
            // ignore the input and return x
            Success(x, input)
        // return the inner function
        Parser innerFn

    let (<*>) = applyP

    let lift2 f xP yP = returnP f <*> xP <*> yP

    let rec sequence parserList =
        // define the "cons" function, which is a two parameter function
        let cons head tail = head :: tail

        // lift it to Parser World
        let consP = lift2 cons

        // process the list of parsers recursively
        match parserList with
        | [] -> returnP []
        | head :: tail -> consP head (sequence tail)

    let rec parseZeroOrMore parser input =
        let firstResult = run parser input

        match firstResult with
        | Success(firstValue, inputAfterFirstParse) ->
            // if parse succeeds, call recursively
            // to get the subsequent values
            let (subsequentValues, remainingInput) = parseZeroOrMore parser inputAfterFirstParse
            let values = firstValue :: subsequentValues
            (values, remainingInput)
        | Failure err -> ([], input)

    let many parser =
        let innerFn input =
            // parse the input -- wrap in Success as it always succeeds
            Success(parseZeroOrMore parser input)

        Parser innerFn

    let many1 parser =
        let innerFn input =
            let firstResult = run parser input

            match firstResult with
            | Failure(err) -> Failure err
            | Success(firstResult, inputAfterFirstParser) ->
                let (subsequentValues, remainingInput) =
                    parseZeroOrMore parser inputAfterFirstParser

                Success(firstResult :: subsequentValues, remainingInput)

        Parser innerFn

    let opt p =
        let some = p |>> Some
        let none = returnP None
        some <|> none

    let pint =
        // helper
        let resultToInt (sign, charList) =
            let i = charList |> List.toArray |> System.String |> int

            match sign with
            | Some ch -> -i // negate the int
            | None -> i

        // define parser for one digit
        let digit = anyOf [ '0' .. '9' ]

        // define parser for one or more digits
        let digits = many1 digit

        // parse and convert
        opt (pchar '-') .>>. digits |>> resultToInt

    let pint64 =
        // helper
        let resultToInt (sign, charList) =
            let i = charList |> List.toArray |> System.String |> int64

            match sign with
            | Some ch -> -i // negate the int
            | None -> i

        // define parser for one digit
        let digit = anyOf [ '0' .. '9' ]

        // define parser for one or more digits
        let digits = many1 digit

        // parse and convert
        opt (pchar '-') .>>. digits |>> resultToInt

    let charListToStr charList =
        charList |> List.toArray |> System.String

    let pstring str =
        str |> List.ofSeq |> List.map pchar |> sequence |> mapP charListToStr

    let pany = satisfy (fun _ -> true)

    let whitespaceChar = satisfy Char.IsWhiteSpace

    let sepBy1 p sep = 
        let sepThenP = sep >>. p
        p .>>. many sepThenP
        |>> (fun (p, pList) -> p :: pList)

