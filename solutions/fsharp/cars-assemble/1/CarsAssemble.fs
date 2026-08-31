module CarsAssemble

let successRate (speed: int): float =
    if speed > 0 && speed <= 4 then 1.0
    elif speed >= 5 && speed <= 8 then 0.9
    elif speed = 9 then 0.8
    elif speed = 10 then 0.77
    else 0.0

let productionRatePerHour (speed: int): float =
    221.0 * successRate (speed) * (speed |> float) 

let workingItemsPerMinute (speed: int): int =
    productionRatePerHour (speed) / 60.0 |> int
