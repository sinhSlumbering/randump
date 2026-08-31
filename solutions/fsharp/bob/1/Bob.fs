module Bob
open System


let isUpper (input: string) : bool =
    let norm = input |> Seq.filter Char.IsLetter |> Seq.toArray |> String
    not (norm.Trim() = "") && norm |> Seq.forall Char.IsUpper
    
let response (input: string): string =
    let q = input.Trim()
    let upper = isUpper q

    match q with
    | "" -> "Fine. Be that way!"
    | q ->
        let lastChar = q[q.Length - 1]
        match (upper, lastChar, q) with
        | false, '?', _ -> "Sure."
        | true, '?', _  -> "Calm down, I know what I'm doing!"
        | true, _, _    -> "Whoa, chill out!"
        | _ -> "Whatever."