module App.Parser

#if INTERACTIVE
#r @"./bin/Debug/netcoreapp3.1/FParsecCS.dll"
#r @"./bin/Debug/netcoreapp3.1/FParsec.dll"
#endif

open System
open FParsec
open System.IO

///////
// Meta
//////
type Frequency =
    | Annual
    | Quarterly
    | Monthly

type Year = uint32
type Range = { start : Year; finish : Year}

type AttributeType =
    | String of length : int
    | Integer
    | Numeric
    
type Column =
    | SplitId 
    | Attribute of name : string * dataType : AttributeType
    | Volume of frequency : Frequency * range : Range
    
type Match =
    | SplitId
    | Attribute
    
type Meta = {
    matchType : Match
    columns : List<Column>    
}

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg


let parseMatchType : Parser<Match,unit> =
    skipString "matchType:"
    >>. many (skipString " ")
    >>. (stringCIReturn "splitId"  SplitId) <|> (stringCIReturn "attribute" Attribute)
    .>> spaces
    
let skipParens x = skipString"(" >>. x .>> skipString ")"
let parseAttributeType : Parser<AttributeType,unit> =
    (stringCIReturn "int" Integer)
    <|> (stringCIReturn "numeric" Numeric)
    <|> (skipString "string(" >>. pint32 >>= (fun x -> preturn (String x)) .>> skipString")")
    |> skipParens
    
let parseAttributeName : Parser<string,unit> = (skipStringCI "a" >>. pint32 >>= (fun x -> preturn (string ("A" + string x))))
    
let parseAttribute : Parser<Column,unit> =
    pipe2 parseAttributeName parseAttributeType (fun n t -> Column.Attribute (n, t))

let parseVolumeFrequency : Parser<Frequency,unit> =
    (stringCIReturn "Y" Annual)
    <|> (stringCIReturn "Q" Quarterly)
    <|> (stringCIReturn "M" Monthly)
let parseVolumeRange : Parser<Range,unit> =
    pipe2 (puint32 .>> skipString "-") puint32 (fun s f -> {start = s; finish = f})
    
let parseVolume : Parser<Column,unit> =
    skipStringCI "volume("
    >>. pipe2 parseVolumeFrequency parseVolumeRange (fun f r -> Volume (f, r))
    .>> skipStringCI")"
    
let parseColumn : Parser<Column,unit> =
    (stringCIReturn "splitId" Column.SplitId)
    <|> parseAttribute 
    <|> (parseVolume)

let parseColumns : Parser<Column list,unit> =
    skipStringCI "columns:"
    >>. many (skipString " ")
    >>. sepBy parseColumn (pchar ' ')

let parseHeader header : Parser<unit,unit> =
    spaces
    >>. skipStringCI header 
    .>> spaces
    
let parseMeta : Parser<Meta,unit> =
    pipe3 (parseHeader "#meta") parseMatchType parseColumns (fun _ m c -> {matchType = m; columns = c})
    
//////////
// Rows //
//////////

type VolumeCell = int
//| Exact of int
//| Random of min : int * max : int
    
type AttributeCellValue =
    | Integer of int
    | String of string
    | Numeric of float

type AttributeCell = {
    name : string
    value : AttributeCellValue
}

type SplitId = int 
    
type NewRowCell =
    | NewRowAttribute of AttributeCell
    | NewRowVolume of VolumeCell

type RowCell =
    | RowSplitId of int
    | RowAttribute of AttributeCell
    | RowVolume of VolumeCell
    
type Row = RowCell list
type NewRowCount = int
type NewRow = {
    count : NewRowCount
    cells : NewRowCell list
}

//let parseRandom : Parser<VolumeCell, unit> =
//    pipe2
//        (skipStringCI "(random(" >>. pint32 .>> skipStringCI "-")
//        (pint32 .>> skipStringCI "))")
//        (fun x y -> Random (x, y))
        
let parseExact : Parser<VolumeCell,unit> =
    skipParens pint32 //|>> Exact

// TODO: must be a better way        
let parseRowVolume : Parser<VolumeCell,unit> =
    (skipStringCI "volume" >>. parseExact)// <|>
    //(skipStringCI "volume" >>. parseRandom)

let skipQuotationMarks x = skipString"\"" >>. x .>> skipString "\""
let parseAttributeCellValue : Parser<AttributeCellValue,unit> =
    (pfloat |>> Numeric)
    <|> (pint32 |>> Integer)
    <|> ((manyChars (noneOf"\"")) |> skipQuotationMarks  |>> String)
let parseAttributeCell : Parser<AttributeCell,unit> =
    pipe2
        ((satisfy (fun x -> x = 'A')) >>. (manyChars (noneOf "(")))
        (parseAttributeCellValue |> skipParens)
        (fun x y -> {name = x; value = y} )

let parseNewRowCell : Parser<NewRowCell,unit> =
    (parseAttributeCell |>> NewRowAttribute)
    <|> (parseRowVolume |>> NewRowVolume)
    
let parseNewRowCells : Parser<NewRowCell list,unit> =
    many (skipString " ")
    >>. sepBy parseNewRowCell (pchar ' ')

let parseNewRowCount : Parser<NewRowCount,unit> =
    pint32
    .>> skipString ":"
    .>> spaces

let parseNewRow : Parser<NewRow,unit> =
    pipe2 parseNewRowCount parseNewRowCells (fun c v -> {count = c; cells = v})

let parseNewRows : Parser<NewRow list,unit> =
    parseHeader "#new_rows"
    >>. (many (parseNewRow .>> spaces))
    
let parseSplitId : Parser<SplitId,unit> =
    skipStringCI "splitId"
    >>. (pint32 |> skipParens)

let parseRowCell : Parser<RowCell,unit> =
    (parseAttributeCell |>> RowAttribute)
    <|> (parseRowVolume |>> RowVolume)
    <|> (parseSplitId |>> RowSplitId)
    
let parseRow : Parser<Row,unit> =
    pchar ':'
    >>. (sepBy parseRowCell (pchar ' '))

let parseRows : Parser<Row list,unit> =
    parseHeader "#rows"
    >>. many (parseRow)
    
type Data = {
    meta : Meta
    newRows : NewRow list
    rows : Row list
}

let pData : Parser<Data,unit> =
    pipe3 parseMeta parseNewRows parseRows (fun m n r -> {meta=m; newRows=n; rows=r})
    
let parseData str =
    match run pData str with
    | Success (v, _, _) -> v
    | Failure (msg,_,_) -> failwith msg 

