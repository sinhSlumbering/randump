module PasswordChecker
open System

type PasswordError =
    | LessThan12Characters
    | MissingUppercaseLetter
    | MissingLowercaseLetter
    | MissingDigit
    | MissingSymbol

let (|CheckLessThan12|_|) (pattern: string) =
    if pattern.Length < 12 then Some () else None

let (|CheckMissingUpperCase|_|) (pattern: string) =
    if pattern |> String.exists (Char.IsUpper) then None else Some ()

let (|CheckMissingLowerCase|_|) (pattern: string) =
    if pattern |> String.exists (Char.IsLower) then None else Some ()

let (|CheckMissingDigit|_|) (pattern: string) =
    if pattern |> String.exists (Char.IsDigit) then None else Some ()

let (|CheckMissingSymbol|_|) (pattern: string) =
    if pattern |> String.exists (fun c -> Char.IsSymbol(c) || c = '&' || c = '@' || c = '!') then None else Some ()

let checkPassword (password: string) : Result<string, PasswordError> =
    match password with
    | CheckLessThan12       -> Error LessThan12Characters
    | CheckMissingUpperCase -> Error MissingUppercaseLetter
    | CheckMissingLowerCase -> Error MissingLowercaseLetter
    | CheckMissingDigit     -> Error MissingDigit
    | CheckMissingSymbol    -> Error MissingSymbol
    | _                     -> Ok password 


let getStatusMessage (result: Result<string, PasswordError>) : string =
    match result with
    | Error LessThan12Characters   -> "Error: does not have at least 12 characters"
    | Error MissingUppercaseLetter -> "Error: does not have at least one uppercase letter"
    | Error MissingLowercaseLetter -> "Error: does not have at least one lowercase letter"
    | Error MissingDigit           -> "Error: does not have at least one digit"
    | Error MissingSymbol          -> "Error: does not have at least one symbol"
    | Ok _ -> "OK"