module GameOfLife

open System

module Array2D =
    type Mode = Zero | Donut | CylinderX | CylinderY | MoebiusX | MoebiusY
    let flatten<'T> (arr: 'T[,]) = arr |> Seq.cast<'T> |> Seq.toArray
    let toArray<'T> (arr: 'T[,]) = Array.init (Array2D.length1 arr) (fun i -> arr.[i, *] )
    let translateX<'T> (mode: Mode) (zero: 'T) (o: int) (arr: 'T[,]) =
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
    let translateY<'T> (mode: Mode) (zero: 'T) (o: int) (arr: 'T[,]) =
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
    let translate<'T> (mode: Mode) (zero: 'T) (x: int, y: int) = translateX mode zero y >> translateY mode zero x
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

type Cell = int
type World(w: Cell[,]) =
    member this.World = w
    member this.Length1 = Array2D.length1 w
    member this.Length2 = Array2D.length2 w
    member this.CloneWith (c: Cell) =
        Array2D.create this.Length1 this.Length2 c |> World
    static member Pad (padSize: int) (padWith: Cell) (this: World): World =
        Array2D.init (this.Length1 + 2 * padSize) (this.Length2 + 2 * padSize) (fun x y ->
            if x < padSize || y < padSize || x >= (this.Length1 + padSize) || y >= (this.Length2 + padSize)
            then padWith
            else this.World.[x - padSize, y-padSize]
        )
        |> World
    static member Create x y v =
        Array2D.create x y v |> World
    static member CreateRandom x y =
        let rnd = Random()
        Array2D.init x y (fun x y -> if rnd.Next(2) > 0 then 1 else 0) |> World
    static member (+) (this: World, other: World): World =
       this.World
        |> Array2D.mapi (fun x y c -> c + other.World.[x,y])
        |> World
    static member (~+) (this: World, other: World): World =
       this.World
        |> Array2D.mapi (fun x y c -> c + other.World.[x,y])
        |> World
    static member AndInt (other: int) (this: World): World =
        this.World
        |> Array2D.map (fun v -> if v = other then 1 else 0)        
        |> World
    static member (&&&) (this: World, other: int): World =
        this.World
        |> Array2D.mapi (fun x y c -> if c = other then 1 else 0 )        
        |> World
    static member (&&&) (other: int, this: World): World =
        this.World
        |> Array2D.mapi (fun x y c -> if c = other then 1 else 0 )        
        |> World
    static member (&&&) (this: World, other: World): World =
        this.World
        |> Array2D.mapi (fun x y c -> if c > 0 && c = other.World.[x,y] then 1 else 0 )        
        |> World
    static member (|||) (this: World, other: World): World =
        this.World
        |> Array2D.mapi (fun x y c -> if c > 0 || other.World.[x,y] > 0 then 1 else 0 )        
        |> World

module Pretty =
    let cell (isPretty: bool) (c: Cell) =
        if c > 0 then (if isPretty then "x" else c.ToString()) else (if isPretty then " " else "0")
    let row (isPretty: bool) (r: Cell[]) = 
        (if isPretty then "|" else "") +
        (r |> Array.map (cell isPretty) |> String.concat "") +        
        (if isPretty then "|" else "")
    let world (isPretty: bool) (w: World) =
        (if isPretty then (Array.fold (fun c _ -> c + "_") "_" w.World.[0,*]) + "_\n" else "") +
        (w.World |> Array2D.toArray |> Array.map (row isPretty) |> String.concat "\n") +
        (if isPretty then "\n_" + (Array.fold (fun c _ -> c + "_") "_" w.World.[0,*]) else "")
    let worlds (isPretty: bool) (ws: World[]) =
        ws |> Array.map (world isPretty) |> String.concat("\n")




#nowarn "0058"
let life (mode: Array2D.Mode) (w: World) =
    [|3;4|]
    |> Array.map (fun i -> World.AndInt i (
            (Array.map ((Array2D.translate mode 0) >> ((fun f -> f w.World) >> World)) [|
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
            |> Array.reduce (+)
        ))
    |> Array.zip [| w.CloneWith 1; w |]
    |> Array.map (fun (a, b) -> a &&& b)
    |> Array.reduce (|||)
#warn "0058"

let rec lifeRec (mode: Array2D.Mode) (i: int) (w: World) =
    if i < 1 then w
    else lifeRec mode (i - 1) (life mode w)

module Common =
    let glider = array2D [[0;0;1];[1;0;1];[0;1;1]]