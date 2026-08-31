module EliudsEggs

let rec eggCtr n =
    if n = 0 then 0
    else (n &&& 1) + eggCtr (n >>> 1)

let eggCount n = eggCtr n
