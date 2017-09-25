namespace Marreco.Finance.Core
module Library = 

  [<Measure>] type money;
  [<Measure>] type yearperiod;
  [<Measure>] type rate;

  type Currency = 
  | EUR of decimal<money>
  | BRL of decimal<money>
  | USD of decimal<money>
  | JPY of decimal<money>
  | CAD of decimal<money>


  type CompoundFrequency = 
  | Continous
  | Annual
  | Monthly
  | Linear
  | Periodic of int //numero de * ao ano

  type PeriodOfRate =
  | ``Per annum``
  | ``Per month``
  | Effective

  type Interest = 
  | CompoundFactor of decimal //usando nomeclatura ARC ?
  | DiscountFactor of decimal
  | Rate of decimal<rate> * CompoundFrequency * PeriodOfRate * decimal<yearperiod>


  (12.0M<rate>, Annual, ``Per annum``, 1.0M<yearperiod>) |> Rate |> ignore
