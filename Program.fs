open System
open GameOfLife

let world: World = World.CreateRandom 3 3 |> World.Pad 2 0
// let world: World = (World(array2D [[1;1;0;0;1];[0;0;0;0;0];[0;0;0;0;0];[0;0;0;0;0];[0;0;0;0;0]])) |> World.Pad 2 0

[<EntryPoint>]
let main argv: int =
    [|0..4|]
        |> Array.map (
            (fun f a b c-> f a c b) // swaps 2nd and 3rd parameter so we can be passing i
                <| lifeRec // function being swapped
                <| Array2D.Mode.Donut // first parameter
                <| world // 3rd parameter now is second
        ) // applies i to swapped lifeRec
        |> Pretty.worlds true
        |> printfn "%O"
    0 // return an integer exit code
