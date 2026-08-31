module GuessingGame

let reply (guess: int): string = 
    match (guess - 42) with
    | 0 -> "Correct"
    | 1 
    | -1 -> "So close"
    | i when i < 0 -> "Too low"
    | _ -> "Too high"
