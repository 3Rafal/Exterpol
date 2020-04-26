module App.DataGenerator

open System
open Parser

let annualVolumes range =
    [for x in range.start .. range.finish -> sprintf "Y%d_volume" x]

let quarterlyVolumes range =
    [for y in range.start .. range.finish do
     for q in 1 .. 4
     -> sprintf "Q%d_%d_volume" q y]

let monthlyVolumes range =
    [for y in range.start .. range.finish do
     for m in 1 .. 12 
     -> sprintf "M%d_%d_volume" m y]

let volumeNames frequency range =
    match frequency with
    | Annual -> annualVolumes range
    | Quarterly -> quarterlyVolumes range
    | Monthly -> monthlyVolumes range
    
let columnNames column =
    match column with
    | Column.SplitId -> ["SplitId"]
    | Column.Attribute a -> [a.name]
    | Column.Volume (f,r) -> volumeNames f r
    
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
    
let volumeCount freq range =
    let years = range.finish - range.start
    years * (volumesInYear freq)

let findVolumeCount data =
    data.columns
    |> List.map (fun x -> match x with
                              | Volume (f,r) -> Some (volumeCount f r)
                              | _ -> None)
    |> List.tryFind Option.isSome
    |> function
       | Some v -> Option.get v
       | None -> 0
    
let randomString n : string = 
    let r = System.Random()
    let chars = Array.concat([[|'a' .. 'z'|];[|'A' .. 'Z'|];[|'0' .. '9'|]])
    let sz = Array.length chars in
    new String(Array.init n (fun _ -> chars.[r.Next sz]))
    
let randomAttribute attributeType =
    match attributeType with
    | AttributeType.String len -> randomString len
    | AttributeType.Integer -> System.Random().Next() |> string
    | AttributeType.Numeric -> System.Random().NextDouble() |> string
        
// TODO: n*k complexity? or is it auto-optimised?
let multiplyVolume data value =
    let count = findVolumeCount data
    seq { for _ in 1..count -> value}
    |> String.concat ","
let missingColumnValue data column =
    match column with
    | Column.SplitId -> 0 |> string
    | Column.Attribute a -> randomAttribute a.dataType
    | Column.Volume (_,_) -> System.Random().Next() |> string |> multiplyVolume data
    
type RowCellKey =
    | SplitId
    | Attribute of name : string
    | Volume

let attributeValueToString value =
    match value with
    | Integer i -> i |> string
    | Numeric n -> n |> string
    | String s -> s

let volumeValueToString data vol =
    match vol with
    | Exact v -> v
    | Random (s, f) -> System.Random().Next(s,f)
    |> string
    |> multiplyVolume data
    
let newCellsToMap cells data =
    cells
    |> List.map (fun x -> match x with
                          | NewRowCell.NewRowAttribute a -> (Attribute a.name, a.value |> attributeValueToString)
                          | NewRowCell.NewRowVolume v -> (Volume, v |> volumeValueToString data))
    |> Map.ofList
let cellsToMap cells data =
    cells
    |> List.map (fun x -> match x with
                          | RowCell.RowAttribute a -> (Attribute a.name, a.value |> attributeValueToString)
                          | RowCell.RowVolume v -> (Volume, v |> volumeValueToString data)
                          | RowCell.RowSplitId s -> (SplitId, s |> string))
    |> Map.ofList

let columnToKey column =
    match column with
    | Column.SplitId -> SplitId
    | Column.Attribute a -> Attribute a.name
    | Column.Volume (_,_) -> Volume

let newRow data row =
    let map = newCellsToMap row data
    data.columns
    |> List.map (fun c -> match map.TryFind (columnToKey c) with
                          | Some s -> s
                          | None -> missingColumnValue data c)
    |> String.concat "," 
    
let row data row =
    let map = cellsToMap row data
    data.columns
    |> List.map (fun c -> match map.TryFind (columnToKey c) with
                          | Some s -> s
                          | None -> missingColumnValue data c)
    |> String.concat ","
    
let newRows data =
    match data.newRows with
    | Some s -> s
                |> List.map (fun x -> [ for _ in 1..x.count -> (newRow data x.cells)])
                |> List.concat
    | None -> []
    
let rows data =
    match data.rows with
    | Some s -> s |> List.map (fun x -> row data x)
    | None -> []

let createCSV data =
    [header data]
    @ match data.newRows with
      | Some _ -> newRows data
      | None -> []
    @ match data.rows with
      | Some _ -> rows data
      | None -> []
    |> String.concat "\n"
