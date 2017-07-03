namespace FSharp.ProjectTemplate
open System
open Utilities
open Calendar
module public Interest =

    type CompoundFrequency = 
    | Compounded of Frequency

    let AnnualyCompounded = Compounded (1</years> |> TimesPerYear)


    type RateBase = | Base of Period



    type InterestRate = 
    | Rate of float * RateBase * CompoundFrequency * DayCountConvention


