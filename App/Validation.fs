namespace App

module Validator =
    let validateAttribute attr cell =
        match (attr, cell) with
        | (AttributeType.Integer , Integer _) -> ()
        | (AttributeType.Numeric, Numeric _) -> ()
        | (AttributeType.DateTime, DateTime _) -> ()
        | (AttributeType.String len, String s) -> if s.Length > len
                                                  then failwith (sprintf "Too long string %s" s)
        | (_,_) -> failwith (sprintf "Invalid attribute value: %A" cell)

    let validateCell (column, cell) =
        match (column, cell) with
        | (_,CellValue.Random) -> ()
        | (SplitId, Integer _) -> ()
        | (ParentId, Integer _) -> ()
        | (Attribute a, _) -> validateAttribute a.dataType cell
        | (Volume _, Integer _) -> ()
        | (Rate _, Integer _) -> ()
        | (Rate _, Numeric _) -> ()
        | (_,_) -> failwith (sprintf "Invalid column value: %A" cell)

    let validateCells columns cells =
        match cells with
        | CellValues c -> List.zip columns c |> List.iter validateCell
        | RandomRoot -> ()
        | RandomNonRoot _ -> ()
        
    let cells row =
        match row with
        | NewRow n -> n.cells 
        | ExistingRow e -> e
        
    let validateRowAttributes data =
        data.rows |> List.iter (fun x -> x |> cells |> (validateCells data.columns))

    let validate data =
        validateRowAttributes data 
        data
