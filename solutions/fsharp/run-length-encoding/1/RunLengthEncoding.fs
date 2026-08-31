module RunLengthEncoding
open System

let rec enc input curr len out =
    if input = "" then 
        if len > 1 then out + sprintf "%d%c" len curr else out + sprintf "%c" curr
    elif input[0] = curr then 
        enc input[1..] curr (len + 1) out
    elif len > 1 then
        enc input[1..] input[0] 1 (out + sprintf "%d%c" len curr)
    else
        enc input[1..] input[0] 1 (out + sprintf "%c" curr)

let encode input =
    if input = "" then ""
    else
        enc input input[0] 0 ""

let rec repeatChar (ch: char) (count: int) (out: string) : string =
    if count <= 0 then out
    else repeatChar ch (count - 1) (out + string ch)

let parseCount (nums: string) : int =
    match Int32.TryParse(nums) with
    | true, n -> n
    | false, _ -> 1

let decode (input: string) : string =
    let rec dec (idx: int) (nums: string) (out: string) : string =
        if idx >= input.Length then
            out
        elif Char.IsDigit(input[idx]) then
            dec (idx + 1) (nums + string input[idx]) out
        else
            let count = parseCount nums
            let expanded = repeatChar input[idx] count ""
            dec (idx + 1) "" (out + expanded)

    dec 0 "" ""