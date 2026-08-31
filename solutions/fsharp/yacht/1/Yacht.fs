module Yacht


type Category = 
    | Ones
    | Twos
    | Threes
    | Fours
    | Fives
    | Sixes
    | FullHouse
    | FourOfAKind
    | LittleStraight
    | BigStraight
    | Choice
    | Yacht

type Die =
    | One 
    | Two 
    | Three
    | Four 
    | Five 
    | Six

let mapDie2Num dice =
    dice
    |> List.map (fun die ->
        match die with
        | One   -> 1
        | Two   -> 2
        | Three -> 3
        | Four  -> 4
        | Five  -> 5
        | Six   -> 6
    )

let checkSetEq (dice: List<Die>) scoreSet =
    dice 
    |> mapDie2Num 
    |> Set.ofList
    |> (=) scoreSet

let littleStraight (dice: List<Die>) =
    checkSetEq dice[..4] ([1; 2; 3; 4; 5] |> Set.ofList)

let bigStraight (dice: List<Die>) =
    checkSetEq dice ([2; 3; 4; 5; 6] |> Set.ofList)

let checkYacht (dice: List<Die>) = 
    let inv = dice[0]
    dice |> List.fold (fun acc elm -> elm = inv) true

let filterAdd num dice = 
    dice
    |> mapDie2Num
    |> List.filter (fun die -> die = num)
    |> List.sum

let (|NofAKind|_|) n dice=
    dice
    |> mapDie2Num
    |> List.countBy id
    |> List.tryFind (fun (_, count) -> count >= n)
    |> Option.map fst

let fullHouseChk (dice: List<Die>) = 
    match dice, dice with
    | NofAKind 3 threeVal, NofAKind 2 twoVal when threeVal <> twoVal -> threeVal * 3 + twoVal * 2
    | _ -> 0 

let chk4ofKind (dice: List<Die>) = 
    match dice with
    | NofAKind 4 value -> value * 4
    | _ -> 0

let sumOfDice (dice: List<Die>) = 
    dice
    |> mapDie2Num
    |> List.sum    

let score category dice =
    match category with
    | Ones -> filterAdd 1 dice
    | Twos -> filterAdd 2 dice
    | Threes -> filterAdd 3 dice
    | Fours -> filterAdd 4 dice
    | Fives -> filterAdd 5 dice
    | Sixes -> filterAdd 6 dice
    | FullHouse -> fullHouseChk dice 
    | FourOfAKind -> chk4ofKind dice
    | LittleStraight -> if littleStraight dice then 30 else 0
    | BigStraight -> if bigStraight dice then 30 else 0
    | Choice -> sumOfDice dice
    | Yacht -> if checkYacht dice then 50 else 0