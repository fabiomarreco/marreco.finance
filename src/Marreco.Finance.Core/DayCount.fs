module DayCount


module Conventions = 
    module ``30360`` = 
        open System

        let private (|EndOfMonth|_|) {Y=y; M=m; D=d } = 
            if (d = DateTime.DaysInMonth(y, m)) then Some m else None

        let countDaysBetween adjustDates (YMD start) (YMD end') =
            let ({Y=y1; M=m1; D=d1},{Y=y2; M=m2; D=d2}) = adjustDates start end'
            (y2 - y1) * 360<days> +  (m2 - m1) * 30<days> + (d2 - d1) * 1<days>

        module BondBasis = 
            let private adjustDates start end' = 
                let d1 = min (start.D) 30
                let d2 = if (d1 = 30 || d1 = 31) then min (end'.D) 30 else end'.D
                let newStart = {start with D = d1 }
                let newEnd = {end' with D = d2 }
                (newStart, newEnd)
            let countDays = countDaysBetween adjustDates

        module US = 
            let rec private adjustDates start end' = 
                match (start, end') with
                | (EndOfMonth 2, EndOfMonth 2)          -> adjustDates start  {end' with D=30}
                | (EndOfMonth 2, _ )                    -> adjustDates {start with D = 30} end'
                | ({Y=_; M=_; D=30}, {Y=_; M=_; D=31 })
                | ({Y=_; M=_; D=31}, {Y=_; M=_; D=31 }) -> adjustDates start {end' with D=30}
                | ({Y=_; M=_; D=31}, _)                 -> adjustDates {start with D = 30} end'
                | _                                     -> start, end'
            let countDays = countDaysBetween adjustDates

        module ICMA = 
            let rec private adjustDates start end'  = 
                match (start, end') with
                | ({Y=_; M=_; D=31}, _) -> adjustDates {start with D = 30} end'
                | (_, {Y=_; M=_; D=31}) -> adjustDates start  { end' with D = 30}
                | _                     -> start, end'
            let countDays = countDaysBetween adjustDates            

        module ISDA = 
            let rec private adjustDates start end'  = 
                match (start, end') with
                | (EndOfMonth _, _)             -> adjustDates {start with D = 30} end'
                | (_, EndOfMonth m) when m <> 2 -> adjustDates start  { end' with D = 30}
                | _                             -> start, end'
            let countDays = countDaysBetween adjustDates



    module Actual = 
        open System
        module ISDA = 
            let rec dayCountFactor dt1 dt2 : decimal<years> = 
                let daysInYear y = if (DateTime.IsLeapYear y) then 366M<days/years> else 365M<days/years>
                let daysBetweenInYear dt1 dt2 = 
                    let x = Date.DaysBetween dt1 dt2 
                    let y = match dt1 with | YMD {Y=y; M=_;D=_} -> daysInYear y
                    x / y

                match (dt1, dt2) with
                | _ when dt1 = dt2  -> 0M<years>
                | _ when dt1 > dt2  -> dayCountFactor dt2 dt1 |> (*) (-1M)
                | (YMD {Y=y1; M=_; D=_}, YMD {Y=y2; M=_; D=_}) when y1 = y2-> (Date.DaysBetween dt1 dt2) / (daysInYear y1)
                | (YMD {Y=y1; M=_; D=_}, _) -> 
                        (Date.DaysBetween dt1 (Date.Create (y1+1, 1, 1))) / (daysInYear y1) 
                        + dayCountFactor (Date.Create (y1+1, 1, 1)) dt2
                

                (*
            //let dayCountFactor date1 date2 = Date.DaysBetween date1 date2 |> (/) ()
            dayCountFactor (Date.Create (2018,01,12)) (Date.Create (2018,12,28));;
            let x = daysInYear 218
            let y = Date.DaysBetween (Date.Create (2018,01,12)) (Date.Create (2018,12,28))

            y / x
            *)