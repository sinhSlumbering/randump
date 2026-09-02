module Sublist

type SublistType = Equal | Sublist | Superlist | Unequal


let isSubList (big: 'a list) (small: 'a list) : bool =
    if List.isEmpty small then true
    elif small.Length > big.Length then false
    else
        big
        |> List.windowed small.Length
        |> List.contains small

let sublist xs ys = 
    if xs = ys then Equal
    elif isSubList ys xs then Sublist
    elif isSubList xs ys then Superlist
    else Unequal
