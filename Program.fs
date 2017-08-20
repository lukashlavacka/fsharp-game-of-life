open System
open GameOfLife

let world: World =
    Array2D.create 10 10 cZero // 2 (fun x -> if x > 0 then 1 else 0)
        |> Array2D.insertAt (GameOfLife.Shapes.Methuselah.rPentomino) (3, 3) // |> Array2D.pad 2 0) (2, 2)
        // |> Array2D.insertAt (Life.Shapes.Spaceship.glider |> Array2D.flipX) (6, 0)
        // |> Array2D.insertAt (Life.Shapes.Spaceship.glider |> Array2D.flipY) (0, 6)
        // |> Array2D.insertAt (Life.Shapes.Spaceship.glider |> Array2D.flipXY) (6, 6)

[<EntryPoint>]
let main argv: int =
    GameOfLife.recursiveSeq Array2D.TranslateMode.Donut 100 world
        // |> Seq.length
        // |> Seq.last |> Pretty.world cZero true
        |> Seq.takeFirstNthLastIndex 10 |> Seq.toArray |> pretty true
        |> printfn "%O"
    0 // return an integer exit code
