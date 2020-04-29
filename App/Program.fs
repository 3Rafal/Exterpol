open System.IO

open App
open FileParser
open Validator
open CSVGenerator

let generate csv = csv |> parseData |> validate |> createCSV
    
[<EntryPoint>]
let main argv =
    let csv = File.ReadAllText @"C:\Users\Rafal.Gwozdzinski\RiderProjects\Exterpol\App\data.txt"
              |> generate
    File.WriteAllText (@"C:\Users\Rafal.Gwozdzinski\RiderProjects\Exterpol\App\data.csv", csv)
    0
    

