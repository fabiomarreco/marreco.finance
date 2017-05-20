
[<Measure>] type money;
[<Measure>] type years;
[<Measure>] type rate;

type RatePeriod = | RatePeriod of float<years>
let Annual = RatePeriod(1.0<years>)

type Frequency = | Frequency of int

type CompoundFrequency = 
| Continous 
| Annualy
| Monthly
| Linear
| Periodic of int //numero de * ao ano


type InterestRate = | InterestRate of decimal<rate> * RatePeriod * CompoundFrequency

type Discount = 
| CompoundFactor of decimal
| DiscountFactor of decimal
| DiscountRate of InterestRate * float<years>


let taxa = InterestRate (12M<rate> , PerYear , Annualy)
(taxa, 2.0<years>) |> DiscountRate


let toDiscountFactor v = 
    match v with
    | DiscountFactor d -> d |> DiscountFactor
    | CompoundFactor c -> 1.0M/c |> DiscountFactor
    | DiscountRate (rate , period) -> 