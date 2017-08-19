open System
open GameOfLife

let world: World =
    Array2D.create 200 200 cZero // 2 (fun x -> if x > 0 then 1 else 0)
        |> Array2D.insertAt (GameOfLife.Shapes.Methuselah.rPentomino) (100, 100) // |> Array2D.pad 2 0) (2, 2)
        // |> Array2D.insertAt (Life.Shapes.Spaceship.glider |> Array2D.flipX) (6, 0)
        // |> Array2D.insertAt (Life.Shapes.Spaceship.glider |> Array2D.flipY) (0, 6)
        // |> Array2D.insertAt (Life.Shapes.Spaceship.glider |> Array2D.flipXY) (6, 6)

[<EntryPoint>]
let main argv: int =
    GameOfLife.recursiveSeq Array2D.TranslateMode.Zero 1000 world
        // |> Seq.length
        |> Seq.last |> Pretty.world cZero true
        // |> Seq.takeEvery 20
        // |> Seq.map (Pretty.world true)
        // |> String.concat "\n"
        |> printfn "%O"
    0 // return an integer exit code
