open System.IO

open App
open FileParser
open Validator
open CSVGenerator

[<EntryPoint>]
let main argv =
    let csv = File.ReadAllText @"C:\Users\Rafal.Gwozdzinski\RiderProjects\Exterpol\App\data.txt"
              |> parseData
              |> validate
              |> createCSV
    File.WriteAllText (@"C:\Users\Rafal.Gwozdzinski\RiderProjects\Exterpol\App\data.csv", csv)
    0
