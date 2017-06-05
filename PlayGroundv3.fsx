open System;

// Unidades de medida
// [<Measure>] type money;
[<Measure>] type years;
[<Measure>] type months;
[<Measure>] type days;

let inline intToFloat (x:int<'u>) : float<'u> =
    x |> float |>  LanguagePrimitives.FloatWithMeasure


// //Redefinicao de operadores
let (?+) (dt:DateTime) (days:int<days>) = dt.AddDays(float days);;
let (?+) (dt:DateTime) (months:int<months>) = dt.AddMonths(int months);;
let (?+) (dt:DateTime) (years:int<years>) = dt.AddYears(int years);;


type Hollidays = DateTime list;;

type DayCountConvention = 
    | DCWD252 of Hollidays
    | DC30E360
    | DC30360US
    | DCACT360
    | DCACT365
    | DCACTACTISDA

//Subtract 2 days
let actualDaysBetween (date1:DateTime) (date2:DateTime) = (int (date2.Subtract(date1).TotalDays)) * 1<days>

//Active patterns for datetime
let (|Date|) (date:DateTime) = (date.Year, date.Month, date.Day)
let (|EndOfMonth|StartOfMonth|MiddleOfMonth|) (date:DateTime) = 
    match date with
    | Date(_, m , 1) -> StartOfMonth(m) 
    | d when (DateTime(d.AddMonths(1).Year, d.AddMonths(1).Month, 1).AddDays(-1.0)) = d -> EndOfMonth(d.Month) 
    | Date(_, m , _) -> MiddleOfMonth(m)


//Daycount using convention
let rec daysBetween dayCountConvention (date1:DateTime) (date2:DateTime) = 
    match dayCountConvention with
    | DCWD252 hollidays  -> 0<days>
    | DC30E360  ->  match (date1, date2) with
                    | (Date (y1, m1 , 31), _) -> daysBetween dayCountConvention (DateTime(y1, m1, 30)) date2
                    | (_, Date (y2, m2 , 31)) -> daysBetween dayCountConvention date1 (DateTime(y2, m2, 30))
                    | _ -> actualDaysBetween date1 date2
    | DC30360US  -> match (date1, date2) with
                    | (EndOfMonth 2, EndOfMonth 2) -> daysBetween dayCountConvention date1 (date2.AddDays(float (30-date2.Day)))
                    | (EndOfMonth 2, _) -> daysBetween dayCountConvention (date1.AddDays(float (30-date2.Day))) date2
                    | (Date(y1, m1, 30), Date (y2, m2, 31)) 
                    | (Date(y1, m1, 31), Date (y2, m2, 31)) -> daysBetween dayCountConvention date1 (DateTime(y2, m2, 30))
                    | (Date(y1, m1, 31), _) -> daysBetween dayCountConvention (DateTime(y1, m1, 30)) date2
                    | _ -> actualDaysBetween date1 date2
    | DCACT360 
    | DCACT365 
    | DCACTACTISDA  -> actualDaysBetween date1 date2


let yearsBetween dayCountConvention (date1:DateTime) (date2:DateTime) = 
    let days = daysBetween dayCountConvention date1 date2 |> intToFloat
    match dayCountConvention with
    | DCWD252 _  -> days * (1.0<years> / 252.0<days>)
    | DC30E360  
    | DC30360US 
    | DCACT360  -> days * (1.0<years> / 360.0<days>)
    | DCACT365  -> days * (1.0<years> / 365.0<days>)
    | DCACTACTISDA  ->  Seq.unfold (fun dt -> match dt with
                                              | _ when dt >= date2                 ->  None
                                              | Date(y, m, d)  when y = date2.Year ->  Some((dt,date2), date2)
                                              | Date(y, m, d)                      ->  let firstNextYear = DateTime(y+1, 1, 1); 
                                                                                       Some((dt, firstNextYear), firstNextYear)) date1 
                        |> Seq.sumBy (fun (d1, d2)-> 
                                let daysInYear (dt:DateTime) = (if (DateTime.IsLeapYear(dt.Year)) then 366.0<days> else 365.0<days>)/1.0<years>
                                (intToFloat (actualDaysBetween d1 d2)) / (daysInYear d1))



let date1 = DateTime.Today 
let date2 = date1.AddDays(20.0)

// let rec calc startDate endDate (sum:float<years>) = 
//     let daysInYear (dt:DateTime) = (if (DateTime.IsLeapYear(dt.Year)) then 366.0<days> else 365.0<days>)/1.0<years>
//     match (startDate, endDate) with
//     | (Date(y1, _, _), Date(y2, _, _)) when y1 = y2 -> sum + (float (actualDaysBetween startDate endDate)) / (daysInYear startDate)
//     | (d1, Date(y2, m2, d2)) -> sum + (calc d1 (DateTime(y2+1, 1, 1)) 0)


Seq.unfold (fun dt -> match dt with
                      | _ when dt >= date2  ->  None
                      | Date(y, m, d) 
                        when y = date2.Year ->  Some((dt,date2), date2)
                      | Date(y, m, d)       ->  let firstNextYear = DateTime(y+1, 1, 1); 
                                                Some((dt, firstNextYear), firstNextYear)) date1 
    |> Seq.map (fun (d1, d2)-> 
            let daysInYear (dt:DateTime) = (if (DateTime.IsLeapYear(dt.Year)) then 366.0<days> else 365.0<days>)/1.0<years>
            (intToFloat (actualDaysBetween d1 d2)) / (daysInYear d1))
    |> Seq.sum

    // (a.ToShortDateString(), b.ToShortDateString()))
    // |> Seq.take 10;




let d1 = date1
let d2 = date2
Seq.unfold (fun x -> 
        printfn "%A" x
        if (x > 10) then None
        else Some ((x,3), x+2)) 0
        |> Seq.toList



DateTime(2010,01,01)

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