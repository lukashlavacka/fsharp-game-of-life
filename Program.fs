open System
open GameOfLife

// let world: World = World.CreateRandom 3 3 |> World.Pad 2 0
let world: World = 
    Array2D.create 10 10 0
        |> Array2D.insertAt (Common.glider |> Array2D.pad 2 0) (2, 2)

// let world2: World =
//     array2D [[0;0;0;0;1];[0;0;0;0;1];[0;0;0;0;1];[0;0;0;0;0];[0;0;0;0;0]]
//         |> Array2D.pad 2 0

[<EntryPoint>]
let main argv: int =
    [|0..4|]
        |> Array.map (
            (fun f a b c-> f a c b) // swaps 2nd and 3rd parameter so we can be passing i
                <| Life.recursive // function being swapped
                <| Array2D.TranslateMode.MoebiusX // first parameter
                <| world // 3rd parameter now is second
        ) // applies i to swapped lifeRec
        |> Pretty.worlds true
        |> printfn "%O"
    0 // return an integer exit code
