module EliudsEggs

open System.Numerics

let eggCount n = BitOperations.PopCount(uint32 n)