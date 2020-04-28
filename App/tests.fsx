#r @"./bin/Debug/netcoreapp3.1/FParsecCS.dll"
#r @"./bin/Debug/netcoreapp3.1/FParsec.dll"
#load "Parsing.fs"
#load "Validation.fs"
#load "DataGeneration.fs"

open App
open Parser
open DataGenerator
open FParsec

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let p1 : Parser<int32,unit> = pint32 .>> followedBy (anyOf " \n")
let p2 : Parser<int32 list,unit> = sepBy p1 (anyOf " ,")
