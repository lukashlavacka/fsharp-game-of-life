open System
open GameOfLife

let world: World = initRandomWorld 3 3

[<EntryPoint>]
let main argv: int =
    world
        |> Pretty.world true
        |> printfn "%O"
    world
        |> Array2D.translate Array2D.Mode.Donut Dead (1,0)
        |> Pretty.world true
        |> printfn "%O"
    world
        |> Array2D.translate Array2D.Mode.Donut Dead (0,1)
        |> Pretty.world true
        |> printfn "%O"
    world
        |> Array2D.translate Array2D.Mode.Donut Dead (1,1)
        |> Pretty.world true
        |> printfn "%O"
    0 // return an integer exit code
