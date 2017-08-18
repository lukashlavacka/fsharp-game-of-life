module GameOfLife

open System

module Array2D =
    type TranslateMode = Zero | Donut | CylinderX | CylinderY | MoebiusX | MoebiusY
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
            | MoebiusX -> arr |> Array2D.mapi (fun iy ix _ ->
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
            | MoebiusY -> arr |> Array2D.mapi (fun iy ix _ ->
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
    let add (arr1: 'T[,]) (arr2: 'T[,]) =
        arr1 |>
            Array2D.mapi (fun x y c -> c + arr2.[x,y])
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

type Cell = int
type World = Cell[,]

module Pretty =
    let cell (isPretty: bool) (c: Cell) =
        if c > 0 then (if isPretty then "x" else c.ToString()) else (if isPretty then " " else "0")
    let row (isPretty: bool) (r: Cell[]) =
        (if isPretty then "|" else "") +
        (r |> Array.map (cell isPretty) |> String.concat "") +
        (if isPretty then "|" else "")
    let world (isPretty: bool) (w: World) =
        (if isPretty then (Array.fold (fun c _ -> c + "_") "_" w.[0,*]) + "_\n" else "") +
        (w |> Array2D.toArray |> Array.map (row isPretty) |> String.concat "\n") +
        (if isPretty then "\n_" + (Array.fold (fun c _ -> c + "_") "_" w.[0,*]) else "")
    let worlds (isPretty: bool) (ws: World[]) =
        ws |> Array.map (world isPretty) |> String.concat("\n")

#nowarn "0058"
#nowarn "0064"
module Life =
    let one (mode: Array2D.TranslateMode) (w: World) =
        [|3;4|]
        |> Array.map (fun i -> Array2D.andVal 0 1 i (
                (Array.map ((Array2D.translate mode 0) >> ((fun f -> f w))) [|
                    (-1,-1);
                    (-1, 0);
                    (-1, 1);
                    ( 0,-1);
                    ( 0, 0);
                    ( 0, 1);
                    ( 1,-1);
                    ( 1, 0);
                    ( 1, 1);
                |])
                |> Array.reduce Array2D.add
            ))
        |> Array.zip [| Array2D.cloneWith 1 w; w |]
        |> Array.map (Array2D.andAnotherTuple 0 1)
        |> Array.reduce (Array2D.orAnother 0 1)

    let rec recursive (mode: Array2D.TranslateMode) (i: int) (w: World) =
        if
            i < 1 ||
            Array2D.isEmpty 0 w ||
            Array2D.equals w (one mode w)
        then w
        else recursive mode (i - 1) (one mode w)
    let rec recursiveSeq (mode: Array2D.TranslateMode) (upTo: int) (w: World): seq<World> =
        seq {
            if
                upTo < 1 ||
                Array2D.isEmpty 0 w ||
                Array2D.equals w (one mode w)
            then ()
            else
                yield w
                yield! recursiveSeq mode (upTo - 1) (one mode w)
        }
    module Shapes =
        module StillLife =
            let block =
                array2D [
                    [1;1];
                    [1;1]
                ]
                |> Array2D.pad 1 0
            let beehive =
                array2D [
                    [0;1;1;0];
                    [1;0;0;1];
                    [0;1;1;0];
                ]
                |> Array2D.pad 1 0
            let loaf =
                array2D [
                    [0;1;1;0];
                    [1;0;0;1];
                    [0;1;0;1];
                    [0;0;1;0];
                ]
                |> Array2D.pad 1 0
            let boat =
                array2D [
                    [1;1;0];
                    [1;0;1];
                    [0;1;0]
                ]
                |> Array2D.pad 1 0
            let tub =
                array2D [
                    [0;1;0];
                    [1;0;1];
                    [0;1;0]
                ]
                |> Array2D.pad 1 0
        module Spaceship =
            let glider =
                array2D [
                    [0;0;1];
                    [1;0;1];
                    [0;1;1]
                ]
                |> Array2D.pad 1 0
            let lightWeight =
                array2D [
                    [0;1;0;0;1];
                    [1;0;0;0;0];
                    [1;0;0;0;1];
                    [1;1;1;1;0];
                ]
                |> Array2D.pad 1 0
#warn "0058"
#warn "0064"