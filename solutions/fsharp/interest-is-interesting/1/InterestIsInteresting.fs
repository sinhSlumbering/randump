module InterestIsInteresting

let interestRate (balance: decimal): single =
    if balance < 0 then (single 3.213)
    elif balance < 1000 then (single 0.5f)
    elif balance < 5000 then (single 1.621f)
    else (single 2.475f)

let interest (balance: decimal): decimal =
   balance * ((interestRate balance / 100.f) |> decimal) 

let annualBalanceUpdate(balance: decimal): decimal =
   (balance + interest balance) |> decimal

let amountToDonate(balance: decimal) (taxFreePercentage: float): int =
    if balance <= 0 then 0 
    else
       (balance * ((taxFreePercentage / (float) 100.0 * 2.0) |> decimal)) |> int
