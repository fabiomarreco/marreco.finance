module DayCount

[<Measure>] type years;
[<Measure>] type months;
[<Measure>] type days;
[<Measure>] type weeks;

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



