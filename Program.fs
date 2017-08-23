open System
open GameOfLife

let world: World =
    Array2D.create 100 100 cZero
        |> Array2D.insertAtCenter (GameOfLife.Shapes.Methuselah.rPentomino)
        // |> Array2D.insertAtQuadrant 1 (GameOfLife.Shapes.Spaceship.glider |> Array2D.flipX )
        // |> Array2D.insertAtQuadrant 2 (GameOfLife.Shapes.Spaceship.glider                  )
        // |> Array2D.insertAtQuadrant 3 (GameOfLife.Shapes.Spaceship.glider |> Array2D.flipY )
        // |> Array2D.insertAtQuadrant 4 (GameOfLife.Shapes.Spaceship.glider |> Array2D.flipXY)

[<EntryPoint>]
let main argv: int =
    let sw = Diagnostics.Stopwatch.StartNew()
    GameOfLife.recursiveSeq Array2D.TranslateMode.Donut 10000 world
        // |> Seq.length
        |> Seq.takeWhileUnique Array2D.hash |> Seq.takeFirstNthLastIndex 10001 |> Seq.toArray |> pretty false
        |> printfn "%A"
    sw.Stop()
    printfn "%O" sw.Elapsed
    0 // return an integer exit code
