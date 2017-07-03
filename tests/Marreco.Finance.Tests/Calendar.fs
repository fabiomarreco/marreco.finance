namespace Marreco.Finance.Tests

open Expecto
module CalendarTest =


  [<Tests>]
  let tests =
    testCase "yes" <| fun () ->
      let subject = "Hello World"
      Expect.equal subject "Hello world"
                   "The strings should equal"
