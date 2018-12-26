[<AutoOpen>]
module Date

    open System
    type Date = private Date of DateTime
    with 
        static member FromDateTime (dt:DateTime) = dt.Date |> Date
        static member Create (year, month, day) = DateTime(year, month, day) |> Date
        static member ToDateTime (Date d) = d


    let (|YMD|) (Date d) = (d.Year, d.Month, d.Day)
