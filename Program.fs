open System
open GameOfLife

let world: World =
    Array2D.create 10 10 0
        |> Array2D.insertAt (Life.Shapes.Spaceship.glider) (0, 0)
        |> Array2D.insertAt (Life.Shapes.Spaceship.glider |> Array2D.flipX) (6, 0)
        |> Array2D.insertAt (Life.Shapes.Spaceship.glider |> Array2D.flipY) (0, 6)
        |> Array2D.insertAt (Life.Shapes.Spaceship.glider |> Array2D.flipXY) (6, 6)

[<EntryPoint>]
let main argv: int =
    Life.recursiveSeq Array2D.TranslateMode.MoebiusX 20 world
        |> Seq.map (Pretty.world true)
        |> String.concat "\n"
        |> printfn "%O"
    0 // return an integer exit code
