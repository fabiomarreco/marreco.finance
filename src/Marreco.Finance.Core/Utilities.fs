[<AutoOpen>]
module Utilities 

open System.Text.RegularExpressions;
open System.Text;
open System;

///Converts from int to float keeping measure 

let inline public  intToFloat (x:int<'u>) : float<'u> =
    x |> float |>  LanguagePrimitives.FloatWithMeasure

let inline public  intToDec (x:int<'u>) : decimal<'u> =
    x |> decimal |>  LanguagePrimitives.DecimalWithMeasure

let inline public apply (fn:float->float) (x:float<'u>)  : float<'u> =
    x |> float |> fn |> LanguagePrimitives.FloatWithMeasure

let inline public orderParams fn p1 p2  = 
    if (p1 <= p2) then fn p1 p2
    else fn p2 p1

let public (|Regex|_|) pattern input = 
    if input = null then None
    else    
        let m = Regex.Match(input, pattern)
        if m.Success then Some ([for x in m.Groups -> x.Value])
        else None

let public (|DateTimeString|_|) strInput =
    match DateTime.TryParse strInput with
    | true, dt -> Some(dt)
    | _ -> None


