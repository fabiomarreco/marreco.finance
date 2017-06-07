namespace FSharp.ProjectTemplate
open System
module public Utilities =

    ///Converts from int to float keeping measure 
    let inline public  intToFloat (x:int<'u>) : float<'u> =
        x |> float |>  LanguagePrimitives.FloatWithMeasure

