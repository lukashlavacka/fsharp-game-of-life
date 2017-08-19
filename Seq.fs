module Seq

let takeEvery (n: int) (s: seq<'T>) =
    s
        |> Seq.mapi (fun i e -> if i % n = 0 then Some(e) else None)
        |> Seq.choose id