module DayCount

[<Measure>] type years;
[<Measure>] type months;
[<Measure>] type days;
[<Measure>] type weeks;


module Conventions = 

    type private DaysBetween = Date -> Date -> int<days>
    
    module ``30360`` = 
        open System

        type TokenizedDate = { Y: int; M: int; D: int }
        let (|EndOfMonth|_|) {Y=y; M=m; D=d } = 
            if (d = DateTime.DaysInMonth(y, m)) then Some m else None

        let tokenizeDate (YMD (y, m, d)) = {Y=y; M=m; D = d }

        let countDaysBetween  = function 
            | ( {Y=y1; M=m1; D=d1}, 
                {Y=y2; M=m2; D=d2} ) -> 
                (y2 - y1) * 360<days> +  (m2 - m1) * 30<days> + (d2 - d1) * 1<days>

        let rec bondBasis (startDate, endDate) = 
            let d1 = min (startDate.D) 30
            let d2 = if (d1 = 30 || d1 = 31) then min (endDate.D) 30 else endDate.D
            let startDate' = {startDate with D = d1 }
            let endDate' = {endDate with D = d2 }
            countDaysBetween (startDate', endDate')

        let rec US (tStart, tEnd) = 
            match (tStart, tEnd) with
            | (EndOfMonth 2, EndOfMonth 2)          -> US (tStart, {tEnd with D=30})
            | (EndOfMonth 2, _ )                    -> US ( {tStart with D = 30}, tEnd)
            | ({Y=_; M=_; D=30}, {Y=_; M=_; D=31 })
            | ({Y=_; M=_; D=31}, {Y=_; M=_; D=31 }) -> US (tStart, {tEnd with D=30})
            | ({Y=_; M=_; D=31}, _)                 -> US ( {tStart with D = 30}, tEnd)
            | _                                     -> countDaysBetween (tStart, tEnd)

        let rec ICMA (tStart, tEnd)  = 
            match (tStart, tEnd) with
            | ({Y=_; M=_; D=31}, _) -> ICMA ( {tStart with D = 30}, tEnd)
            | (_, {Y=_; M=_; D=31}) -> ICMA ( tStart , { tEnd with D = 30})
            | _                     -> countDaysBetween (tStart, tEnd)

        let rec ISDA (tStart, tEnd)  = 
            match (tStart, tEnd) with
            | (EndOfMonth _, _)             -> ISDA ( {tStart with D = 30}, tEnd)
            | (_, EndOfMonth m) when m <> 2 -> ISDA ( tStart , { tEnd with D = 30})
            | _                             -> countDaysBetween (tStart, tEnd)





                
