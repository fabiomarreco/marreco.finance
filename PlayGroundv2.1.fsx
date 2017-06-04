type Period = 
    | Days of int
    | Months of float
    | Years of float
    
// let Annual = Years(1.0)
// let SemiAnnual = Years(0.5)

type CompoundFrequency = 
| ContinousCompound 
| PeriodicCompound of Period //numero de * ao ano

let AnnualCompound = PeriodicCompound(Years(1.0))


type InterestRate = | InterestRate of decimal * Period * CompoundFrequency


let treasuryRate = InterestRate (2M<rate> , Annual , AnnualCompound)



type Discount = 
| CompoundFactor of decimal
| DiscountFactor of decimal
| DiscountRate of InterestRate * Period


(taxa, 2.0M<years>) |> DiscountRate


let toDiscountFactor v = 
    match v with
    | DiscountFactor d -> d |> DiscountFactor
    | CompoundFactor c -> 1.0M/c |> DiscountFactor
    | DiscountRate (rate , period) -> 