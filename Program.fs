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
    [|0..20|]
        |> Array.map (
            (fun f a b c-> f a c b) // swaps 2nd and 3rd parameter so we can be passing i
                <| Life.recursive // function being swapped
                <| Array2D.TranslateMode.MoebiusX // first parameter
                <| world // 3rd parameter now is second
        ) // applies i to swapped lifeRec
        |> Pretty.worlds true
        |> printfn "%O"
    0 // return an integer exit code
