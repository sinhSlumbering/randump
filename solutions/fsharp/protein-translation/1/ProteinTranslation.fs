module ProteinTranslation
let (|StartsWith|_|) pattern code =

    if code = "" || code.Length < 3 then None
    else 
        if code[..2] = pattern then Some () else None 



let rec translate rna (proteins: List<string>) = 
    match rna with
    | StartsWith "AUG" -> translate rna[3..] (proteins @ ["Methionine"])
    | StartsWith "UUU" 
    | StartsWith "UUC" -> translate rna[3..] (proteins @ ["Phenylalanine"])
    | StartsWith "UUA"
    | StartsWith "UUG" -> translate rna[3..] (proteins @ ["Leucine"])
    | StartsWith "UCU" 
    | StartsWith "UCC" 
    | StartsWith "UCA" 
    | StartsWith "UCG" -> translate rna[3..] (proteins @ ["Serine"])
    | StartsWith "UAU"
    | StartsWith "UAC" -> translate rna[3..] (proteins @ ["Tyrosine"])
    | StartsWith "UGU"
    | StartsWith "UGC" -> translate rna[3..] (proteins @ ["Cysteine"])
    | StartsWith "UGG" -> translate rna[3..] (proteins @ ["Tryptophan"])
    | _ -> proteins
    

let proteins rna = 
    translate rna []