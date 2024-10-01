module Helpers


let pairOptionals (x : 'T seq) (y : 'U seq) : ('T option * 'U option) seq =
    let x = x |> Seq.toArray
    let y = y |> Seq.toArray

    let n = System.Math.Max( x.Length, y.Length )

    let safeGet (a : 't []) i = Array.tryItem i a 
    [|
        for i in 0 .. (n-1) do
            (safeGet x i, safeGet y i)
    |]

let createIdGenerator() =
    let mutable i = 0
    (fun () -> 
        i <- i  + 1
        i)