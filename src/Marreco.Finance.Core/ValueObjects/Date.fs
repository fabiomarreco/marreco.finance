[<AutoOpen>]
module Date

    open System


    type Date = private Date of DateTime
    with 
        static member FromDateTime (dt:DateTime) = dt.Date |> Date
        static member Create (year, month, day) = DateTime(year, month, day) |> Date
        static member ToDateTime (Date d) = d

    type YMD = { Y: int; M: int; D: int }
    let (|YMD|) (Date d) = {Y = d.Year; M = d.Month;  D = d.Day } 
    let toYMD (YMD ymd) = ymd

