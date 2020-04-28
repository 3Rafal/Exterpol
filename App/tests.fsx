#r @"./bin/Debug/netcoreapp3.1/FParsecCS.dll"
#r @"./bin/Debug/netcoreapp3.1/FParsec.dll"
#load "Domain.fs"
#load "Parsing.fs"
#load "Validation.fs"
#load "DataGeneration.fs"

open App
open RowParser
open FParsec

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
