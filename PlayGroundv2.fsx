
[<Measure>] type money;
[<Measure>] type years;
[<Measure>] type rate;

type Period = | YearPeriod of float<years>
let Annual = YearPeriod(1.0<years>)
let SemiAnnual = YearPeriod(0.5<years>)

type CompoundFrequency = 
| ContinousCompound 
| PeriodicCompound of Period //numero de * ao ano

let AnnualCompound = PeriodicCompound(Annual)


type InterestRate = | InterestRate of decimal<rate> * Period * CompoundFrequency

type Discount = 
| CompoundFactor of decimal
| DiscountFactor of decimal
| DiscountRate of InterestRate * Period


let taxa = InterestRate (12M<rate> , Annual , AnnualCompound)
(taxa, 2.0<years>) |> DiscountRate


let toDiscountFactor v = 
    match v with
    | DiscountFactor d -> d |> DiscountFactor
    | CompoundFactor c -> 1.0M/c |> DiscountFactor
    | DiscountRate (rate , period) -> 