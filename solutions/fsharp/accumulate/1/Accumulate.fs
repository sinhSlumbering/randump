module Accumulate

let rec acu func input output = 
    match input with
    | [] -> List.rev output
    | head :: tail -> acu func tail (func head :: output)
    
let accumulate (func: 'a -> 'b) (input: 'a list): 'b list = acu func input []
