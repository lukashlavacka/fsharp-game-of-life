open System
open GameOfLife

// let world: World = initRandomWorld 3 3
let world: World = (World(array2D [[0;1;0];[0;0;1];[1;1;1]])).Pad 2 0
[<EntryPoint>]
let main argv: int =
    {0..20}
        |> Seq.map (fun i -> world |> lifeRec Array2D.Mode.Donut i |> Pretty.world true) 
        |> String.concat "\n"
        |> printfn "%O"
    0 // return an integer exit code
