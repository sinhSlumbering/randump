module TracksOnTracksOnTracks

let newList: string list = []

let existingList: string list = ["F#"; "Clojure"; "Haskell"]

let addLanguage (language: string) (languages: string list): string list =
    language :: languages

let countLanguages (languages: string list): int = languages.Length

let rec listRevRec (og: list<string>) (acc: list<string>) =
    if og = [] then acc
    else listRevRec og[1..] (og.Head :: acc)

let reverseList(languages: string list): string list = languages |> List.fold (fun acc elm -> elm :: acc) [] 

let excitingList (languages: string list): bool = 
    match languages with
    | head :: tail when head = "F#" -> true
    | [_; "F#"]
    | [_; "F#"; _] -> true
    | _ -> false