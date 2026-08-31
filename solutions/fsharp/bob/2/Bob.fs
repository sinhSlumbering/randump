module Bob
open System


let isUpper (input: string) : bool =
    let norm = input |> Seq.filter Char.IsLetter |> Seq.toArray |> String
    not (norm.Trim() = "") && norm |> Seq.forall Char.IsUpper
    
let response (input: string): string =
    let q = input.Trim()
    let upper = isUpper q
    let lastChar = q.EndsWith('?')

    match (upper, lastChar, q) with
    | _, _, ""       -> "Fine. Be that way!"
    | false, true, _ -> "Sure."
    | true, true, _  -> "Calm down, I know what I'm doing!"
    | true, false, _ -> "Whoa, chill out!"
    | _              -> "Whatever."