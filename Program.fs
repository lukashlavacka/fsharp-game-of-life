open System

type Cell = Alive | Dead
type World = Cell[,]

module Array2D =
    type Mode = Zero | Donut | CylinderX | CylinderY
    let flatten<'T> = Seq.cast<'T> >> Seq.toArray
    let toArray<'T> (arr: 'T[,]) = Array.init (Array2D.length1 arr) (fun i -> arr.[i, *] )
    let translate<'T> (mode: Mode) (zero: 'T) (x: int, y: int) (arr: 'T[,]) =
        match mode with
            | Zero -> arr |> Array2D.mapi (fun iy ix _ ->
                    if
                        ix - x < 0 ||
                        ix - x >= Array2D.length1 arr ||
                        iy - y < 0 ||
                        iy - y >= Array2D.length2 arr
                    then zero
                    else arr.[ix - x, iy - y]
                )
            | Donut -> arr |> Array2D.mapi (fun iy ix _ ->
                    if ix - x < 0 then arr.[Array2D.length1 arr + ix - x, iy - y]
                    else if ix - x >= Array2D.length1 arr then arr.[(ix - x) % Array2D.length1 arr, iy - y]
                    else if iy - y < 0 then arr.[ix - x, Array2D.length2 arr + iy - y]
                    else if iy - y >= Array2D.length2 arr then arr.[ix - x, (iy - y) % Array2D.length2 arr]
                    else arr.[ix - x, iy - y]
                )                      
            | _ -> arr

module Pretty =
    let cell = function
        | Alive -> "1"
        | _ -> "0"
    let row (pretty: bool) (r: Cell[]) = 
        (if pretty then "|" else "") +
        (r |> Array.map cell |> String.concat "") +        
        (if pretty then "|" else "")
    let world (pretty: bool) (w: World) =
        (if pretty then (Array.fold (fun c _ -> c + "_") "_" w.[0,*]) + "_\n" else "") +
        (w |> Array2D.toArray |> Array.map (row pretty) |> String.concat "\n") +
        (if pretty then "\n‾" + (Array.fold (fun c _ -> c + "‾") "‾" w.[0,*]) else "")
       
let initRandomWorld x y: World = 
    let rnd = Random()
    Array2D.init x y (fun x y -> if rnd.Next(2) > 0 then Alive else Dead)

/// <summary>Given the 3x3 neighbourhood determines if the center should be alive</summary>
let isAlive (neighbors: Cell[,]): Cell =
    match neighbors |> Array2D.flatten |> Array.toList |> List.sumBy (fun cell -> if cell = Alive then 1 else 0) with
        | 4 | 5 -> Alive
        | _ -> Dead
        
let world: World = initRandomWorld 3 3

[<EntryPoint>]
let main argv: int =
    world
        |> Array2D.translate Array2D.Mode.Donut Dead (0,0)
        |> Pretty.world true
        |> printfn "%O"
    world
        |> Array2D.translate Array2D.Mode.Donut Dead (0,-1)
        |> Pretty.world true
        |> printfn "%O"
    0 // return an integer exit code
