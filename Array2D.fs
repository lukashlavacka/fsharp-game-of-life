module Array2D

open Operators

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
    Array2D.mapi (fun iy ix _ ->
        if iy >= y && iy < (y + Array2D.length1 src) && ix >= x && ix < (x + Array2D.length2 src)
        then src.[iy - y, ix - x]
        else dst.[iy, ix]
    ) dst
let insertAtCenter<'T> (src: 'T[,]) (dst: 'T[,]) =
    insertAt src ((Array2D.length2 dst - Array2D.length2 src) / 2, (Array2D.length1 dst - Array2D.length1 src) / 2) dst
let insertAtQuadrant<'T> (q: int) (src: 'T[,]) (dst: 'T[,]) =
    match q with
    | 1 -> insertAt src ((Array2D.length2 dst / 4 * 3) - (Array2D.length2 src / 2), (Array2D.length1 dst / 4    ) - (Array2D.length1 src / 2)) dst
    | 2 -> insertAt src ((Array2D.length2 dst / 4    ) - (Array2D.length2 src / 2), (Array2D.length1 dst / 4    ) - (Array2D.length1 src / 2)) dst
    | 3 -> insertAt src ((Array2D.length2 dst / 4    ) - (Array2D.length2 src / 2), (Array2D.length1 dst / 4 * 3) - (Array2D.length1 src / 2)) dst
    | 4 -> insertAt src ((Array2D.length2 dst / 4 * 3) - (Array2D.length2 src / 2), (Array2D.length1 dst / 4 * 3) - (Array2D.length1 src / 2)) dst
    | _ -> insertAtCenter src dst
let pad<'T> (padSize: int) (padWith: 'T) (arr: 'T[,]) =
    Array2D.init ( Array2D.length1 arr + 2 * padSize) (Array2D.length2 arr + 2 * padSize) (fun y x ->
        if y < padSize || x < padSize || y >= (Array2D.length1 arr + padSize) || x >= (Array2D.length2 arr + padSize)
        then padWith
        else arr.[y - padSize, x-padSize]
    )
let initRandom (x: int) (y: int) (randomMax: int) (r: int -> 'T) =
    let rnd = System.Random()
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
        Array2D.mapi (fun y x c -> (c, arr2.[y,x]))
let add (addFn: 'T -> 'T -> 'T) (arr1: 'T[,]) (arr2: 'T[,]) =
    arr1 |>
        Array2D.mapi (fun y x c -> addFn c arr2.[y,x])
/// Sets true value where both arrays have non zero, otherwise sets zero
let andAnother (zero: 'T) (trueVal: 'T) (arr1: 'T[,]) (arr2: 'T[,]) =
    arr1 |>
        Array2D.mapi (fun y x c -> if c <> zero && arr2.[y,x] <> zero then trueVal else zero)
/// Sets trueVal where array has cmp value, otherwise sets zero
let andVal (zero: 'T) (trueVal: 'T) (cmp: 'U) (arr1: 'U[,]) =
    arr1 |>
        Array2D.map (fun v -> if v = cmp then trueVal else zero)
/// Sets true value where any array is non zero, otherwise sets zero
let orAnother (zero: 'T) (trueVal: 'T) (arr1: 'T[,]) (arr2: 'T[,]) =
    arr1 |>
        Array2D.mapi (fun y x c -> if c = zero && arr2.[y,x] = zero then zero else trueVal)
let flipX<'T> (arr: 'T[,]) =
    arr |>
        Array2D.mapi (fun y x _-> arr.[y, Array2D.length2 arr - x - 1])
let flipY<'T> (arr: 'T[,]) =
    arr |>
        Array2D.mapi (fun y x _-> arr.[Array2D.length1 arr - y - 1, x])
let flipXY<'T> : 'T[,] -> 'T[,] = flipX >> flipY
let equals (arr1: 'T[,]) (arr2: 'T[,]) =
    arr1
        |> zip arr2
        |> flatten
        |> Array.exists (fun (a, b) -> a <> b)
        |> not

let boolToInt (arr: bool[,]): list<uint64> =
    arr
        |> Seq.cast<bool>
        |> Seq.toList
        |> List.chunkBySize 64
        |> List.map (fun lst ->
            if List.length lst > 64 then raise (System.ArgumentException("List must be shorter than 64"))
            lst
                |> List.mapi (-&-)
                |> List.fold (fun c (i, v) ->
                        if v then
                            c ||| (1UL <<< (64 - i))
                        else c
                    ) 0UL
        )

let hash (arr: bool[,]) =
    arr
        |> boolToInt
        |> List.reduce (^^^)
