open System
open GameOfLife

let world: World =
    Array2D.create 21 21 cZero // 2 (fun x -> if x > 0 then 1 else 0)
        // |> Array2D.insertAtCenter (GameOfLife.Shapes.Methuselah.diehard) // |> Array2D.pad 2 0) (2, 2)
        |> Array2D.insertAtQuadrant 1 (GameOfLife.Shapes.Spaceship.glider |> Array2D.flipX )
        |> Array2D.insertAtQuadrant 2 (GameOfLife.Shapes.Spaceship.glider                  )
        |> Array2D.insertAtQuadrant 3 (GameOfLife.Shapes.Spaceship.glider |> Array2D.flipY )
        |> Array2D.insertAtQuadrant 4 (GameOfLife.Shapes.Spaceship.glider |> Array2D.flipXY)

[<EntryPoint>]
let main argv: int =

    GameOfLife.recursiveSeq Array2D.TranslateMode.Donut 20 world
        // |> Seq.length
        // |> Seq.last |> Pretty.world cZero true
        |> Seq.takeFirstNthLastIndex 5 |> Seq.toArray |> pretty true
        |> printfn "%O"
    0 // return an integer exit code
