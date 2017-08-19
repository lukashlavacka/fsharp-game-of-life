module Array2D

open System

type TranslateMode = Zero | Donut | CylinderX | CylinderY | MoebiusX | MoebiusY | MoebiusXY
/// Flattens Array2D to Array
let flatten<'T> (arr: 'T[,]) = arr |> Seq.cast<'T> |> Seq.toArray
/// Converts Array2D to Array of Arrays
let toArray<'T> (arr: 'T[,]) = Array.init (Array2D.length1 arr) (fun i -> arr.[i, *] )
let translateX<'T> (mode: TranslateMode) (zero: 'T) (o: int) (arr: 'T[,]) =
    match mode with
        | Zero | CylinderY | MoebiusY -> arr |> Array2D.mapi (fun iy ix _ ->
                if
                    ix - o < 0 ||
                    ix - o >= Array2D.length2 arr
                then zero
                else arr.[iy, ix - o]
            )
        | Donut | CylinderX -> arr |> Array2D.mapi (fun iy ix _ ->
                let xLength = Array2D.length1 arr
                let mx = o % xLength
                if ix - mx < 0 then arr.[iy, ix + xLength - mx]
                else if ix - mx >= xLength then arr.[iy, (ix - mx) % xLength]
                else arr.[iy, ix - mx]
            )
        | MoebiusX | MoebiusXY -> arr |> Array2D.mapi (fun iy ix _ ->
                let xLength = Array2D.length1 arr
                let yLength = Array2D.length2 arr
                let mx = o % xLength
                let doubleLoop = (o / xLength) % 2 = 1
                if doubleLoop then
                    if ix - mx < 0 then arr.[iy, ix + xLength - mx]
                    else if ix - mx >= xLength then arr.[iy, (ix - mx) % xLength]
                    else arr.[iy, ix - mx]
                else
                    if ix - mx < 0 then arr.[yLength - iy - 1, ix + xLength - mx]
                    else if ix - mx >= xLength then arr.[yLength - iy - 1, (ix - mx) % xLength]
                    else arr.[iy, ix - mx]
            )
let translateY<'T> (mode: TranslateMode) (zero: 'T) (o: int) (arr: 'T[,]) =
    match mode with
        | Zero | CylinderX | MoebiusX -> arr |> Array2D.mapi (fun iy ix _ ->
                if
                    iy - o < 0 ||
                    iy - o >= Array2D.length1 arr
                then zero
                else arr.[iy - o, ix]
            )
        | Donut | CylinderY -> arr |> Array2D.mapi (fun iy ix _ ->
                let yLength = Array2D.length2 arr
                let my = o % yLength
                if iy - my < 0 then arr.[iy + yLength - my, ix]
                else if iy - my >= yLength then arr.[(iy - my) % yLength, ix]
                else arr.[iy - my, ix]
            )
        | MoebiusY | MoebiusXY -> arr |> Array2D.mapi (fun iy ix _ ->
                let xLength = Array2D.length1 arr
                let yLength = Array2D.length2 arr
                let my = o % yLength
                let doubleLoop = (o / yLength) % 2 = 1
                if doubleLoop then
                    if iy - my < 0 then arr.[iy + yLength - my, ix]
                    else if iy - my >= yLength then arr.[(iy - my) % yLength, ix]
                    else arr.[iy - my, ix]
                else
                    if iy - my < 0 then arr.[iy + yLength - my, xLength - ix - 1]
                    else if iy - my >= yLength then arr.[(iy - my) % yLength, xLength - ix - 1]
                    else arr.[iy - my, ix]
            )
let translate<'T> (mode: TranslateMode) (zero: 'T) (x: int, y: int) = translateX mode zero y >> translateY mode zero x
let insertAt<'T> (src: 'T[,]) (x: int, y: int) (dst: 'T[,]) =
    Array2D.init (Array2D.length1 dst) (Array2D.length2 dst) (fun ix iy ->
        if ix >= x && ix < (x + Array2D.length1 src) && iy >= y && iy < (y + Array2D.length2 src)
        then src.[ix - x, iy - y]
        else dst.[ix, iy]
    )
let pad<'T> (padSize: int) (padWith: 'T) (arr: 'T[,]) =
    Array2D.init ( Array2D.length1 arr + 2 * padSize) (Array2D.length2 arr + 2 * padSize) (fun x y ->
        if x < padSize || y < padSize || x >= (Array2D.length1 arr + padSize) || y >= (Array2D.length2 arr + padSize)
        then padWith
        else arr.[x - padSize, y-padSize]
    )
let initRandom (x: int) (y: int) (randomMax: int) (r: int -> 'T) =
    let rnd = Random()
    Array2D.init x y (fun x y -> r (rnd.Next(randomMax)) )
let cloneWith (w: 'T) (arr: 'T[,]) =
    arr
        |> Array2D.map (fun _ -> w)
let toOne (zero: 'T) (one: 'T) (arr: 'T[,]) =
    arr
        |> Array2D.map (fun v -> if v = zero then v else one )
let isEmpty (zero: 'T) = flatten >> Array.exists (fun v -> v <> zero) >> not
let zip (arr1: 'T[,]) (arr2: 'T[,]) =
    arr1 |>
        Array2D.mapi (fun x y c -> (c, arr2.[x,y]))
let add (addFn: 'T -> 'T -> 'T) (arr1: 'T[,]) (arr2: 'T[,]) =
    arr1 |>
        Array2D.mapi (fun x y c -> addFn c arr2.[x,y])
/// Sets true value where both arrays have non zero, otherwise sets zero
let andAnother (zero: 'T) (trueVal: 'T) (arr1: 'T[,]) (arr2: 'T[,]) =
    arr1 |>
        Array2D.mapi (fun x y c -> if c <> zero && arr2.[x,y] <> zero then trueVal else zero)
/// Sets true value where both arrays have non zero, otherwise sets zero
let andAnotherTuple (zero: 'T) (trueVal: 'T) (arr1: 'T[,], arr2: 'T[,]) =
    arr1 |>
        Array2D.mapi (fun x y c -> if c <> zero && arr2.[x,y] <> zero then trueVal else zero)
/// Sets trueVal where array has cmp value, otherwise sets zero
let andVal (zero: 'T) (trueVal: 'T) (cmp: 'T) (arr1: 'T[,]) =
    arr1 |>
        Array2D.mapi (fun x y c -> if c = cmp then trueVal else zero)
/// Sets true value where any array is non zero, otherwise sets zero
let orAnother (zero: 'T) (trueVal: 'T) (arr1: 'T[,]) (arr2: 'T[,]) =
    arr1 |>
        Array2D.mapi (fun x y c -> if c = zero && arr2.[x,y] = zero then zero else trueVal)
let flipX<'T> (arr: 'T[,]) =
    arr |>
        Array2D.mapi (fun x y _-> arr.[Array2D.length1 arr - x - 1, y])
let flipY<'T> (arr: 'T[,]) =
    arr |>
        Array2D.mapi (fun x y _-> arr.[x, Array2D.length2 arr - y - 1])
let flipXY<'T> : 'T[,] -> 'T[,] = flipX >> flipY
let equals (arr1: 'T[,]) (arr2: 'T[,]) =
    arr1
        |> zip arr2
        |> flatten
        |> Array.exists (fun (a, b) -> a <> b)
        |> not