module Anagram


let isAnagram (target: string) (sourceWord: string) : bool =
    let clean (word: string) =
        word.ToLower().ToCharArray() |> Array.sort
    (sourceWord.ToLower() <> target.ToLower()) && (clean sourceWord = clean target)

let findAnagrams (sources: List<string>) (target: string) = 
    sources
    |> List.filter (isAnagram target)