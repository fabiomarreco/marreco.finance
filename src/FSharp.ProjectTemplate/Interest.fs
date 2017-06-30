namespace FSharp.ProjectTemplate
open System
open Utilities
open Calendar
module public Interest =

    type CompoundFrequency = 
    | ContinousCompound
    | PeriodicCompound of Frequency


    let (|AnnualCompound|MonthlyCompound|DailyCompound|_|) (compound:CompoundFrequency) = 
    match compound with
    | 

    type InterestRate  = 
    | InterestRate of Period * CompoundFrequency

    type DiscountConvention = 
    | Rate

