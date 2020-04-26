module App.Validation

open Parser
let attrMap columns =
    columns
    |> List.fold (fun acc x -> match x with
                               | Attribute a -> (a.name, a) :: acc
                               | _ -> acc) []
    |> Map.ofList

let validateAttribute (attrMap : Map<string,Attribute>, cell) =
    match (attrMap.[cell.name].dataType, cell.value) with
    | (AttributeType.Integer, AttributeCellValue.Integer _) -> ()
    | (AttributeType.Numeric, AttributeCellValue.Numeric _) -> ()
    | (AttributeType.String _, AttributeCellValue.String _) -> ()
    | (_,_) -> failwith (sprintf "Invalid attribute value: %A" cell.value)
    
let newAttributeCells cells =
    cells
    |> List.fold (fun acc x -> match x with
                               | NewRowAttribute a -> a :: acc
                               | _ -> acc) []
        
let attributeCells cells =
    cells
    |> List.fold (fun acc x -> match x with
                               | RowAttribute a -> a :: acc
                               | _ -> acc) []
let validateNewRowAttributes newRows columns =
    let aMap = attrMap columns
    newRows |> List.iter (fun x -> x.cells |> newAttributeCells |> List.iter (fun x -> validateAttribute (aMap, x)))

let validateRowAttributes (rows : Row list, columns) =
    let aMap = attrMap columns
    rows |> List.iter (fun x -> x |> attributeCells |> List.iter (fun x -> validateAttribute (aMap, x)))

let validate data =
    data.newRows |> Option.iter (fun x -> validateNewRowAttributes x data.columns)
    data.rows |> Option.iter (fun x -> validateRowAttributes (x, data.columns))
    data
