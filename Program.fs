open System

type Cell = Alive | Dead
type World = Cell[,]

module Array2D =
    let flatten<'T> = Seq.cast<'T> >> Seq.toArray

let initRandomWorld x y: World = 
    let rnd = Random()
    Array2D.init x y (fun x y -> if rnd.Next(2) > 0 then Alive else Dead)

let world: World = initRandomWorld 3 3

/// <summary>Given the 3x3 neighbourhood determines if the center should be alive</summary>
let isAlive (neighbors: Cell[,]): Cell =
    match neighbors |> Array2D.flatten |> Array.toList |> List.sumBy (fun cell -> if cell = Alive then 1 else 0) with
        | 4 | 5 -> Alive
        | _ -> Dead

[<EntryPoint>]
let main argv: int =
    initRandomWorld 3 3 |> isAlive |> printfn "%A"
    0 // return an integer exit code
