module App.Validation

open Parser
let attrMap columns =
    columns
    |> List.fold (fun acc x -> match x with
                               | Attribute a -> (a.name, a) :: acc
                               | _ -> acc) []
    |> Map.ofList

let validateAttribute attr cell =
    match (attr, cell) with
    | (AttributeType.Integer , Integer _) -> ()
    | (AttributeType.Numeric, Numeric _) -> ()
    | (AttributeType.String len, String s) -> if s.Length > len then failwith (sprintf "Too long string %s" s)
    | (_,_) -> failwith (sprintf "Invalid attribute value: %A" cell)

let validateCell column cell =
    match (column, cell) with
    | (_,CellValue.Random) -> ()
    | (SplitId, Integer _) -> ()
    | (ParentId, Integer _) -> ()
    | (Attribute a, _) -> validateAttribute a.dataType cell
    | (Volume _, Integer _) -> ()
    | (_,_) -> failwith (sprintf "Invalid column value: %A" cell)

let validateCells columns cells =
    let validateForColumn cells col = cells |> List.iter (validateCell col)
    match cells with
    | Random -> ()
    | CellValues c -> columns |> List.iter (validateForColumn c)
    
let cells row =
    match row with
    | NewRow n -> n.cells 
    | ExistingRow e -> e
    
let validateRowAttributes columns rows =
    rows |> List.iter (fun x -> x |> cells |> (validateCells columns))

let validate data =
//    data.rows |> validateRowAttributes data.columns
    data
