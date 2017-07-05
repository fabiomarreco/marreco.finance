namespace Marreco.Finance.Tests

(*
#r "bin\\debug\\FSharp.ProjectTemplate.dll"
#r "bin\\debug\\Expecto.dll"
*)

open Expecto
open FSharp.ProjectTemplate.Calendar
open System

module CalendarTest =


  [<Tests>]
  let tests =
    testCase "simple calendar brazil" <| fun () ->
      let holidays = [DateTime(2017,06,01); DateTime(2017,06,30); DateTime(2017,07,01); DateTime(2017,07,04)]
      let calendar = Calendar("brazil", holidays)
      Expect.equal (calendar.NetworkDays (DateTime(2017,06,29)) (DateTime(2017,07,03))) 1<days> "Erro contagemm dias uteis 1"
      Expect.equal (calendar.NetworkDays (DateTime(2017,06,29)) (DateTime(2017,07,10))) 5<days> "Erro contagemm dias uteis 2"
      Expect.equal (calendar.NetworkDays (DateTime(2017,07,03)) (DateTime(2017,07,05))) 1<days> "Erro contagemm dias uteis 3"
