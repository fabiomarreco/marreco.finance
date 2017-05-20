// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Library.fs"
open FSharp.ProjectTemplate

let num = Library.hello 42
printfn "%i" num


let taxaJuros = 0.1 * 0.7 - 0.03;
let rendaMensal = 4e3;
let investperc = 0.20;
let anoFim = 60;
let anoInicio = 37;
let expectVd = 100;
//----------------------------------------
let fatorMensal = (1.0 + taxaJuros)**(1.0/12.0)
let invest = investperc * rendaMensal
let months = anoFim - anoInicio |> (*) 12
let fluxosCaixa = Seq.replicate months invest
let patrimonio = fluxosCaixa |> Seq.fold (fun acc cx -> acc * fatorMensal + cx) 0.0
printfn "PATRIMONIO: %s" (System.String.Format("{0:N0}",  patrimonio)) //-- patrimonio
let desc = [1..(expectVd - anoFim)*12] |> Seq.map (fun j -> 1.0 / ((1.0 + taxaJuros) ** (float j))) |> Seq.fold (+) 0.0 
let gasto = patrimonio / desc
patrimonio * taxaJuros / (1.0 - (1.0 + taxaJuros) ** (-480.0))

