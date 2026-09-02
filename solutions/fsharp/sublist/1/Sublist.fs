module Sublist

type SublistType = Equal | Sublist | Superlist | Unequal


let rec isSubList big small =
    let rec check b s =
        match b, s with
        | _, [] -> true
        | [], _ -> false
        | x::xs, y::ys when x = y -> check xs ys
        | _ -> false
    
    match big with
    | [] -> small = []
    | _::bs -> check big small || isSubList bs small

let sublist xs ys = 
    if xs = ys then Equal
    elif isSubList ys xs then Sublist
    elif isSubList xs ys then Superlist
    else Unequal
