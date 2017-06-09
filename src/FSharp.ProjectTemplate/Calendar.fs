namespace FSharp.ProjectTemplate
open System
open Utilities
module public Calendar =

    // Units of measure for time
    [<Measure>] type years;
    [<Measure>] type months;
    [<Measure>] type days;

    //Active patterns for datetime
    let (|Date|) (date:DateTime) = (date.Year, date.Month, date.Day)

    let (|EndOfMonth|StartOfMonth|MiddleOfMonth|) (date:DateTime) = 
        match date with
        | Date(_, m , 1) -> StartOfMonth(m) 
        | d when (DateTime(d.AddMonths(1).Year, d.AddMonths(1).Month, 1).AddDays(-1.0)) = d -> EndOfMonth(d.Month) 
        | Date(_, m , _) -> MiddleOfMonth(m)

    let (|Weekday|Weekend|) (date:DateTime) = 
        match date.DayOfWeek with
        | DayOfWeek.Sunday
        | DayOfWeek.Saturday -> Weekend
        | _ -> Weekday
        
    type Calendar (holidays:List<DateTime>) = 
        let rec expandWorkdayCount holidays acc (date:DateTime) = 
            match (date,holidays) with
            | (d, h::t) when d = h -> acc::(expandWorkdayCount t acc (date.AddDays(1.0)))
            | (Weekend, hs) -> acc::(expandWorkdayCount hs acc (date.AddDays(1.0)))
            | (_, []) -> []
            | (d, h) -> (acc+1)::(expandWorkdayCount h (acc+1) (date.AddDays(1.0)))
            | _ -> []
        let firstDay = holidays.[0]
        let lastDay = holidays |> List.last;
        let workdayCount = expandWorkdayCount holidays 0 firstDay
        member x.NetworkDays (startDate:DateTime) (endDate:DateTime)  = 
            // calculo sem calendario
            let workdaysBetween (_startDate:DateTime) (_endDate:DateTime) = 
                let startDate = Seq.initInfinite (float >> _startDate.AddDays) |> Seq.find (fun x-> match x with |Weekend -> false | _ -> true)
                let endDate = Seq.initInfinite (((*)-1) >> float >> _endDate.AddDays) |> Seq.find (fun x-> match x with |Weekend -> false | _ -> true)
                let actualDays = endDate.Subtract(startDate).TotalDays 
                let weekCount = (actualDays |> int) / 7
                match (int startDate.DayOfWeek, int endDate.DayOfWeek) with
                | (w1, w2) when w1 > w2 -> w2 - w1 + 5 + weekCount * 5
                | (w1, w2) -> w2 - w1 + weekCount * 5
            let workdaysBefore = if (startDate < firstDay) then workdaysBetween startDate firstDay else 0
            let workdaysAfter = if (endDate > lastDay) then workdaysBetween lastDay endDate else 0
            let workdaysBetween = workdayCount.[int ((max startDate firstDay).Subtract(firstDay).TotalDays)] - workdayCount.[int ((min endDate lastDay).Subtract(firstDay).TotalDays)]
            (workdaysBefore + workdaysAfter + workdaysBetween) * 1<days>
        member x.Holidays = holidays


    type DayCountConvention = 
        | DCWD252 of (DateTime -> DateTime -> int<days>)
        | DC30E360
        | DC30360US
        | DCACT360
        | DCACT365
        | DCACTACTISDA

    //Subtract 2 days
    let actualDaysBetween (date1:DateTime) (date2:DateTime) = (int (date2.Subtract(date1).TotalDays)) * 1<days>

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
                        | (Date(_, _, 30), Date (y2, m2, 31)) 
                        | (Date(_, _, 31), Date (y2, m2, 31)) -> daysBetween dayCountConvention date1 (DateTime(y2, m2, 30))
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
                                                  | Date(y, _, _)  when y = date2.Year ->  Some((dt,date2), date2)
                                                  | Date(y, _, _)                      ->  let firstNextYear = DateTime(y+1, 1, 1); 
                                                                                           Some((dt, firstNextYear), firstNextYear)) date1 
                            |> Seq.sumBy (fun (d1, d2)-> 
                                    let daysInYear (dt:DateTime) = (if (DateTime.IsLeapYear(dt.Year)) then 366.0<days> else 365.0<days>)/1.0<years>
                                    (intToFloat (actualDaysBetween d1 d2)) / (daysInYear d1))

