module Tests

open System
open Xunit
open FsCheck.Xunit
open Marreco.Finance.Core
open Calendar

let simpleCalendarTestData = [|
    ((DateTime(2017,06,29)), (DateTime(2017,07,03)), 1<days>);
    ((DateTime(2017,06,29)), (DateTime(2017,07,10)), 2<days>);
    ((DateTime(2017,07,03)), (DateTime(2017,07,05)), 3<days>)
|]

[<Theory>]
[<MemberData("simpleCalendarTestData")>]
let ``When Calculating networkdays small sample`` (startDate:DateTime, endDate:DateTime, dayCount:int<days>) =
    let holidays = [DateTime(2017,06,01); DateTime(2017,06,30); DateTime(2017,07,01); DateTime(2017,07,04); DateTime(2017,10,10)]
    let calendar = Calendar("brazil", holidays)
    let actual = calendar.NetworkDays startDate endDate 
    Assert.StrictEqual(dayCount, actual)


[<Fact>]
let ``when calculating empty holidays no exception is thrown`` () = 
    let calendar = Calendar("brazil", [])
    let actual = calendar.NetworkDays (DateTime(2045,11,14)) (DateTime(2005,09,16))
    let expected = -10477<days>
    Assert.StrictEqual(expected, actual)



[<Fact>]
let ``when calculating networkdays workdays is greater or equal to networkdays`` () =
    let networkdayslesserorequaltoworkdays (holidays:list<DateTime>, startDate:DateTime, endDate:DateTime) = 
        let calendar = Calendar("brazil", holidays)
        let wdays = calendar.NetworkDays startDate endDate
        let actualdays = (int (endDate.Subtract(startDate).TotalDays)) * 1<days>
        actualdays >= wdays
    FsCheck.Check.QuickThrowOnFailure networkdayslesserorequaltoworkdays

let start = DateTime.Now.Date.AddMonths(-3);
let endDate = DateTime.Now.Date

[<Fact>]
let ``given 2 dates, when calculating weekend day count result equals brute force`` () =
    let bruteForceWeekendsequalsFormula (start:DateTime, endDate:DateTime) = 
        let (d1, d2) = if(start <= endDate) then (start.Date, endDate.Date) else (endDate.Date, start.Date);
        let expected = Seq.unfold (fun d -> if (d > d2) then None else Some(d, d.AddDays(1.0))) d1 |> Seq.choose (fun d-> match d with |Weekend -> Some(d) | _ -> None) |> Seq.length
        let actual = weekendDaysBetween start endDate
        (actual |> int) = expected
    FsCheck.Check.QuickThrowOnFailure bruteForceWeekendsequalsFormula

