[<AutoOpen>]
module Date
    open System

    [<Measure>] type years;
    [<Measure>] type months;
    [<Measure>] type days;
    [<Measure>] type weeks;

    type Date = private Date of DateTime
    with 
        static member FromDateTime (dt:DateTime) = dt.Date |> Date
        static member Create (year, month, day) = DateTime(year, month, day) |> Date
        static member ToDateTime (Date d) = d
        static member DaysBetween (Date d1) (Date d2) = d2.Subtract(d1).TotalDays |> decimal |> (*) 1M<days>

    type YMD = { Y: int; M: int; D: int }
    let (|YMD|) (Date d) = {Y = d.Year; M = d.Month;  D = d.Day } 
    let (|DateTime|) (Date d) = d
    let toYMD (YMD ymd) = ymd

