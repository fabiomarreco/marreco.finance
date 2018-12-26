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

        let countDaysBetween adjustment s e= (tokenizeDate s, tokenizeDate e) |> adjustment |> countDaysBetween

        module BondBasis = 
            let rec adjustDates (startDate, endDate) = 
                let d1 = min (startDate.D) 30
                let d2 = if (d1 = 30 || d1 = 31) then min (endDate.D) 30 else endDate.D
                let startDate' = {startDate with D = d1 }
                let endDate' = {endDate with D = d2 }
                (startDate', endDate')

            let countDays = countDaysBetween adjustDates

        module US = 
            let rec adjustDates (startDate, endDate) = 
                match (startDate, endDate) with
                | (EndOfMonth 2, EndOfMonth 2) -> adjustDates (startDate, {endDate with D=30})
                | (EndOfMonth 2, _ ) -> adjustDates ( {startDate with D = 30}, endDate)
                | ({Y=_; M=_; D=30}, {Y=_; M=_; D=31 })
                | ({Y=_; M=_; D=31}, {Y=_; M=_; D=31 }) -> adjustDates (startDate, {endDate with D=30})
                | ({Y=_; M=_; D=31}, _) -> adjustDates ( {startDate with D = 30}, endDate)
                | _ -> (startDate, endDate)

            let countDays = countDaysBetween adjustDates

        module 

        
        module private Adjustments = 
            let rec bondBasis (startDate, endDate) = 
                let d1 = min (startDate.D) 30
                let d2 = if (d1 = 30 || d1 = 31) then min (endDate.D) 30 else endDate.D
                let startDate' = {startDate with D = d1 }
                let endDate' = {endDate with D = d2 }
                (startDate', endDate')

            let rec US (startDate, endDate) = 
                match (startDate, endDate) with
                | (EndOfMonth 2, EndOfMonth 2) -> US (startDate, {endDate with D=30})
                | (EndOfMonth 2, _ ) -> US ( {startDate with D = 30}, endDate)
                | ({Y=_; M=_; D=30}, {Y=_; M=_; D=31 })
                | ({Y=_; M=_; D=31}, {Y=_; M=_; D=31 }) -> US (startDate, {endDate with D=30})
                | ({Y=_; M=_; D=31}, _) -> US ( {startDate with D = 30}, endDate)
                | _ -> (startDate, endDate)

            let rec ICMA (startDate, endDate)  = 
                match (startDate, endDate) with
                | ({Y=_; M=_; D=31}, _) -> ICMA ( {startDate with D = 30}, endDate)
                | (_, {Y=_; M=_; D=31}) -> ICMA ( startDate , { endDate with D = 30})
                | _ -> (startDate, endDate)
                
            let rec ISDA (startDate, endDate)  = 
                match (startDate, endDate) with
                | (EndOfMonth _, _) -> ISDA ( {startDate with D = 30}, endDate)
                | (_, EndOfMonth m) when m <> 2 -> ICMA ( startDate , { endDate with D = 30})
                | _ -> (startDate, endDate)





                
