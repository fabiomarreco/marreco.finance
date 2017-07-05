namespace Marreco.Finance.Tests

(*
#r "bin\\debug\\FSharp.ProjectTemplate.dll"
#r "bin\\debug\\Expecto.dll"
*)

open Expecto
open FSharp.ProjectTemplate.Calendar
open System

module CalendarTest =

  let config = { FsCheckConfig.defaultConfig with maxTest = 10000 }

  [<Tests>]
  let tests =
    testCase "simple calendar brazil" <| fun () ->
      let holidays = [DateTime(2017,06,01); DateTime(2017,06,30); DateTime(2017,07,01); DateTime(2017,07,04); DateTime(2017,10,10)]
      let calendar = Calendar("brazil", holidays)
      Expect.equal (calendar.NetworkDays (DateTime(2017,06,29)) (DateTime(2017,07,03))) 1<days> "Erro contagemm dias uteis 1"
      Expect.equal (calendar.NetworkDays (DateTime(2017,06,29)) (DateTime(2017,07,10))) 5<days> "Erro contagemm dias uteis 2"
      Expect.equal (calendar.NetworkDays (DateTime(2017,07,03)) (DateTime(2017,07,05))) 1<days> "Erro contagemm dias uteis 3"


  [<Tests>]
  let fstests = 
      testList "fscheck calendar tests"  [
          testProperty "networkdays less or equal to actual days"  <| fun holidays startDate endDate -> 
              printfn "%A" holidays 
              let calendar = Calendar("teste", holidays)
              let wdays = calendar.NetworkDays startDate endDate
              let actualDays = (int (endDate.Subtract(startDate).TotalDays)) * 1<days>
              Expect.isLessThanOrEqual wdays actualDays "Numero de dias uteis menor que dias corridos"
      ]


  