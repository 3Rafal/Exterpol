module App.DataGenerator

open System
open Parser

let annualRange range suffix =
    [for x in range.start .. range.finish -> sprintf "Y%d_%s" x suffix]

let quarterlyRange range suffix =
    [for y in range.start .. range.finish do
     for q in 1 .. 4
     -> sprintf "Q%d_%d_%s" q y suffix]

let monthlyRange range suffix =
    [for y in range.start .. range.finish do
     for m in 1 .. 12 
     -> sprintf "M%d_%d_%s" m y suffix]

let rangeNames frequency range suffix =
    match frequency with
    | Annual -> annualRange range suffix
    | Quarterly -> quarterlyRange range suffix
    | Monthly -> monthlyRange range suffix
    
let columnNames column =
    match column with
    | SplitId -> ["SplitId"]
    | ParentId -> ["ParentId"]
    | Attribute a -> [a.name]
    | Volume v -> rangeNames v.frequency v.range "volume"
    | Rate r -> rangeNames r.frequency r.range "rate"
    
let header data =
    data.columns
    |> List.map columnNames
    |> List.concat
    |> String.concat ","

let volumesInYear freq =
    match freq with
    | Annual -> 1
    | Quarterly -> 4
    | Monthly -> 12
    
let randomString n : string = 
    let r = System.Random()
    let chars = Array.concat([[|'a' .. 'z'|];[|'A' .. 'Z'|];[|'0' .. '9'|]])
    let sz = Array.length chars in
    new String(Array.init n (fun _ -> chars.[r.Next sz]))

let dateTimeString (dateTime : System.DateTime) = dateTime.ToString("MM/dd/yyyy")
let randomAttribute attributeType =
    match attributeType with
    | AttributeType.String len -> randomString len
    | AttributeType.Integer -> System.Random().Next() |> string
    | AttributeType.Numeric -> System.Random().NextDouble() |> (*) 1000000.0 |> fun x -> Math.Round(x, 6) |> string
    | AttributeType.DateTime -> System.DateTime.Now |> dateTimeString
        
let rangeCount range frequency =
    let years = range.finish - range.start + 1
    years * (volumesInYear frequency)
    
let multiplyRange range frequency value =
    let count = rangeCount range frequency
    seq { for _ in 1..count -> value}
    |> String.concat ","
let randomColumnValue column =
    match column with
    | Column.SplitId -> 0 |> string
    | ParentId -> 0 |> string
    | Column.Attribute a -> randomAttribute a.dataType
    | Column.Volume v -> System.Random().Next() |> string |> multiplyRange v.range v.frequency 
    | Column.Rate r -> 0.0 |> string |> multiplyRange r.range r.frequency

let valueToString column value =
    match value with
    | Integer i -> match column with
                   | Volume v -> i |> string |> multiplyRange v.range v.frequency
                   | Rate r -> i |> string |> multiplyRange r.range r.frequency
                   | _ -> i |> string
    | Numeric n -> match column with
                   | Rate r -> n |> string |> multiplyRange r.range r.frequency
                   | _ -> n |> string
    | String s -> s
    | DateTime d -> d |> dateTimeString
    | CellValue.Random -> randomColumnValue column

let allRandom columns =
    columns |> List.map (fun x -> valueToString x CellValue.Random)
    
let row columns row =
    (match row with
    | Random -> allRandom columns
    | CellValues c -> List.zip columns c
                      |> List.map (fun (x,y)-> valueToString x y)) |> String.concat ","
    
let newRowSplitId = 0 |> string
let newRowAllRandom columns =
    columns
    |> List.map (fun x -> match x with
                          | SplitId -> newRowSplitId
                          | _ -> valueToString x CellValue.Random )
let newRow columns row =
    (match row with
    | Random -> newRowAllRandom columns
    | CellValues c -> List.zip columns c
                      |> List.map (fun (x,y)-> match x with
                                               | SplitId -> newRowSplitId
                                               | _ -> valueToString x y)) |> String.concat ","
    
let rowString data =
    data.rows
    |> List.map (fun x -> match x with
                          | NewRow n -> [for _ in 1..n.count -> (newRow data.columns n.cells)]
                          | ExistingRow e -> [row data.columns e])
    |> List.concat

let createCSV data = (header data) :: (rowString data) |> String.concat "\n"
