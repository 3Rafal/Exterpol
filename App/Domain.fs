namespace App

open System

[<AutoOpen>]
module Columns =
    type AttributeType =
        | String of length : int
        | Integer
        | Numeric
        | DateTime
        
    type Attribute = {
        name : string
        dataType : AttributeType
    }

    type Frequency =
        | Annual
        | Quarterly
        | Monthly
        
    type Range = { start : int; finish : int}
    
    type DateRange = {
        frequency : Frequency
        range : Range
    }
    type Volume = DateRange
    type Rate = DateRange
    
    type Column =
        | SplitId
        | ParentId
        | Attribute of Attribute
        | Volume of Volume
        | Rate of Rate

[<AutoOpen>]
module Rows =
    type CellValue =
        | String of string
        | Integer of int
        | DateTime of DateTime
        | Numeric of float
        | Random
     
     type RowCells =
         | CellValues of CellValue list
         | RandomRoot
         | RandomNonRoot of int
         
    type NewRow = {
        count : int
        cells : RowCells
    }
    type ExistingRow = RowCells

    type Row =
        | NewRow of NewRow
        | ExistingRow of ExistingRow

[<AutoOpen>]
module Data =
    type Data = {
        columns : Column list
        rows : Row list
    }
    
