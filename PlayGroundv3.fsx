open System;

[<Measure>] type money;
[<Measure>] type years;
[<Measure>] type months;
[<Measure>] type days;

type Hollidays = DateTime list;;

type DayCountConvention = 
    | DCWD252 of Hollidays
    | DC30E360
    | DC30U360
    | DCACT360
    | DCACT365
    | DCACTACT


let actualDaysBetween (date1:DateTime) (date2:DateTime) = (int (date2.Subtract(date1).TotalDays)) * 1<days>

let daysBetween dayCountConvention (date1:DateTime) (date2:DateTime) = 
    match dayCountConvention with
    | DCWD252 hollidays  -> 0<days>
    | DC30E360  -> 1<days>
    | DC30U360  -> 1<days>
    | DCACT360  -> actualDaysBetween date1 date2
    | DCACT365  -> actualDaysBetween date1 date2
    | DCACTACT  -> actualDaysBetween date1 date2

let date1 = DateTime.Today 
let date2 = DateTime.Today 


(date2 - date1).TotalDays

type Period = 
    | Years of float<years>
    | Month of float<months>
    | Days of int<days>

let p2 =2.0<years> |> Years
let p1 =2.0<years> |> Years

p2 - p1.

let Annual = Period(1.0<years>)
let SemiAnnual = YearPeriod(0.5<years>)

type CompoundFrequency = 
| ContinousCompound 
| PeriodicCompound of Period //numero de * ao ano

let AnnualCompound = PeriodicCompound(Annual)


type InterestRate = | InterestRate of decimal<rate> * Period * CompoundFrequency

type Discount = 
| CompoundFactor of decimal
| DiscountFactor of decimal
| DiscountRate of InterestRate * Period


let taxa = InterestRate (12M<rate> , Annual , AnnualCompound)
(taxa, 2.0M<years>) |> DiscountRate


let toDiscountFactor v = 
    match v with
    | DiscountFactor d -> d |> DiscountFactor
    | CompoundFactor c -> 1.0M/c |> DiscountFactor
    | DiscountRate (rate , period) -> 