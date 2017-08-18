open System
open GameOfLife

let world: World =
    Array2D.create 30 30 0 // 2 (fun x -> if x > 0 then 1 else 0)
        |> Array2D.insertAt (Life.Shapes.Methuselah.diehard) (10, 10) // |> Array2D.pad 2 0) (2, 2)
        // |> Array2D.insertAt (Life.Shapes.Spaceship.glider |> Array2D.flipX) (6, 0)
        // |> Array2D.insertAt (Life.Shapes.Spaceship.glider |> Array2D.flipY) (0, 6)
        // |> Array2D.insertAt (Life.Shapes.Spaceship.glider |> Array2D.flipXY) (6, 6)

[<EntryPoint>]
let main argv: int =
    Life.recursiveSeq Array2D.TranslateMode.Donut 200 world
        |> Seq.takeEvery 20
        |> Seq.map (Pretty.world true)
        |> String.concat "\n"
        |> printfn "%O"
    0 // return an integer exit code
