module GameOfLife

open System

module Array2D =
    type Mode = Zero | Donut | CylinderX | CylinderY
    let flatten<'T> (arr: 'T[,]) = arr |> Seq.cast<'T> |> Seq.toArray
    let toArray<'T> (arr: 'T[,]) = Array.init (Array2D.length1 arr) (fun i -> arr.[i, *] )
    let translateX<'T> (mode: Mode) (zero: 'T) (o: int) (arr: 'T[,]) =
        match mode with
            | Zero -> arr |> Array2D.mapi (fun iy ix _ ->
                    if
                        ix - o < 0 ||
                        ix - o >= Array2D.length2 arr
                    then zero
                    else arr.[iy, ix - o]
                )
            | Donut -> arr |> Array2D.mapi (fun iy ix _ ->
                    let xLength = Array2D.length1 arr
                    let mx = o % xLength
                    if ix - mx < 0 then arr.[iy, ix + xLength - mx]
                    else if ix - mx >= xLength then arr.[iy, (ix - mx) % xLength]
                    else arr.[iy, ix - mx]
                )                      
            | _ -> arr
    let translateY<'T> (mode: Mode) (zero: 'T) (o: int) (arr: 'T[,]) =
        match mode with
            | Zero -> arr |> Array2D.mapi (fun iy ix _ ->
                    if
                        iy - o < 0 ||
                        iy - o >= Array2D.length1 arr
                    then zero
                    else arr.[iy - o, ix]
                )
            | Donut -> arr |> Array2D.mapi (fun iy ix _ ->            
                    let yLength = Array2D.length2 arr
                    let my = o % yLength
                    if iy - my < 0 then arr.[iy + yLength - my, ix]
                    else if iy - my >= yLength then arr.[(iy - my) % yLength, ix]
                    else arr.[iy - my, ix]
                )                      
            | _ -> arr
    let translate<'T> (mode: Mode) (zero: 'T) (x: int, y: int) = translateX mode zero y >> translateY mode zero x

type Cell = int
type World(w: Cell[,]) =
    member this.World = w
    member this.Length1 = Array2D.length1 w
    member this.Length2 = Array2D.length2 w
    member this.Pad (padSize: int) (padWith: Cell): World =
        Array2D.init (this.Length1 + 2 * padSize) (this.Length2 + 2 * padSize) (fun x y ->
            if x < padSize || y < padSize || x >= (this.Length1 + padSize) || y >= (this.Length2 + padSize)
            then padWith
            else this.World.[x - padSize, y-padSize]
        )
        |> World
    static member Create x y v =
        Array2D.create x y v |> World
    static member (+) (this: World, other: World): World =
       this.World
        |> Array2D.mapi (fun x y c -> c + other.World.[x,y])
        |> World
    static member (~+) (this: World, other: World): World =
       this.World
        |> Array2D.mapi (fun x y c -> c + other.World.[x,y])
        |> World
    // static member (*) (this: World, power: int): World =
    //     this
    //     |> life
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

let initRandomWorld x y: World = 
    let rnd = Random()
    World(Array2D.init x y (fun x y -> if rnd.Next(2) > 0 then 1 else 0))

/// <summary>Given the 3x3 neighbourhood determines if the center should be 1</summary>
let is1 (neighbors: Cell[,]): Cell =
    match neighbors |> Array2D.flatten |> Array.toList |> List.sumBy (fun cell -> if cell = 1 then 1 else 0) with
        | 4 | 5 -> 1
        | _ -> 0

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
    |> Array.zip [| World.Create w.Length1 w.Length2 1; w |]
    |> Array.map (fun (a, b) -> a &&& b)
    |> Array.reduce (|||)
#warn "0058"

let rec lifeRec (mode: Array2D.Mode) (i: int) (w: World) =
    if i < 1 then w
    else lifeRec mode (i - 1) (life mode w)