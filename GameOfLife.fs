module GameOfLife

open Operators

type Cell = byte
/// Defines function adding 2 Cells together
let addFn (a: Cell) (b: Cell): Cell = a + b
let cZero: Cell = 0uy
let cOne: Cell = 1uy

type World = Cell[,]

module Pretty =
    let cell (isPretty: bool) (c: Cell) =
        if c <> cZero then
            if isPretty then "x" else c.ToString()
        else
            if isPretty then " " else "0"
    let row (isPretty: bool) (r: Cell[]) =
        (if isPretty then "|" else "") +
        (r |> Array.map (cell isPretty) |> String.concat "") +
        (if isPretty then "|" else "")
    let world (isPretty: bool) (w: World) =
        (if isPretty then (Array.fold (fun c _ -> c + "_") "_" w.[0,*]) + "_\n" else "") +
        (w |> Array2D.toArray |> Array.map (row isPretty) |> String.concat "\n") +
        (if isPretty then "\n_" + (Array.fold (fun c _ -> c + "_") "_" w.[0,*]) else "")
    let indexedWorld (isPretty: bool) (w: World, i: int) =
        "Iteration: " + (string i) + "\n" + world isPretty w
    let worlds (isPretty: bool) (ws: World[]) =
        ws |> Array.map (world isPretty) |> String.concat("\n")
    let indexedWorlds (isPretty: bool) (ws: (World * int)[]) =
        ws |> Array.map (indexedWorld isPretty) |> String.concat("\n")
    type Printable =
        | Cell of Cell
        | Row of Cell[]
        | World of Cell[,]
        | Worlds of Cell[,][]
        | IndexedWorld of Cell[,] * int
        | IndexedWorlds of (Cell[,] * int)[]
        | None
    let makePrintable p =
        match box p with
        | :? Cell as c -> Printable.Cell(c)
        | :? (Cell[]) as r -> Printable.Row(r)
        | :? World as w -> Printable.World(w)
        | :? (World[]) as ws -> Printable.Worlds(ws)
        | :? (World * int) as iw -> Printable.IndexedWorld(iw)
        | :? ((World * int)[]) as iws -> Printable.IndexedWorlds(iws)
        | _ -> None
    let prettyPrintable (isPretty: bool) = function
        | Printable.Cell(t) -> cell isPretty t
        | Printable.Row(t) -> row isPretty t
        | Printable.World(t) -> world isPretty t
        | Printable.Worlds(t) -> worlds isPretty t
        | Printable.IndexedWorld(t, i) -> indexedWorld isPretty (t, i)
        | Printable.IndexedWorlds(t) -> indexedWorlds isPretty t
        | Printable.None -> ""

let pretty (isPretty: bool) = Pretty.makePrintable >> Pretty.prettyPrintable isPretty


#nowarn "0058"
#nowarn "0064"
let one (mode: Array2D.TranslateMode) (w: World) =
    Array.Parallel.map ((Array2D.andVal cZero cOne) >> (swap (
        (Array.Parallel.map
            ((Array2D.translate mode cZero) >> (swap w))
            (Array.allPairs [|-1;0;1|] [|-1;0;1|])
        )
        |> Array.reduce (Array2D.add addFn)
    ))) <| [|3uy;4uy|]
    |> Array.zip [| Array2D.cloneWith cOne w; w |]
    |> Array.Parallel.map ((<||) (Array2D.andAnother cZero cOne))
    |> Array.reduce (Array2D.orAnother cZero cOne)

let rec recursive (mode: Array2D.TranslateMode) (i: int) (w: World) =
    if
        i < 1 ||
        Array2D.isEmpty cZero w ||
        Array2D.equals w (one mode w)
    then w
    else recursive mode (i - 1) (one mode w)
let rec recursiveSeq (mode: Array2D.TranslateMode) (upTo: int) (w: World): seq<World> =
    seq {
        if
            upTo <= 1 ||
            Array2D.isEmpty cZero w ||
            Array2D.equals w (one mode w)
        then yield w
        else
            yield w
            yield! recursiveSeq mode (upTo - 1) (one mode w)
    }
module Shapes =
    module StillLife =
        let block =
            array2D [
                [cOne;cOne];
                [cOne;cOne]
            ]
            |> Array2D.pad 1 cZero
        let beehive =
            array2D [
                [cZero;cOne;cOne;cZero];
                [cOne;cZero;cZero;cOne];
                [cZero;cOne;cOne;cZero];
            ]
            |> Array2D.pad 1 cZero
        let loaf =
            array2D [
                [cZero;cOne;cOne;cZero];
                [cOne;cZero;cZero;cOne];
                [cZero;cOne;cZero;cOne];
                [cZero;cZero;cOne;cZero];
            ]
            |> Array2D.pad 1 cZero
        let boat =
            array2D [
                [cOne;cOne;cZero];
                [cOne;cZero;cOne];
                [cZero;cOne;cZero]
            ]
            |> Array2D.pad 1 cZero
        let tub =
            array2D [
                [cZero;cOne;cZero];
                [cOne;cZero;cOne];
                [cZero;cOne;cZero]
            ]
            |> Array2D.pad 1 cZero
    module Spaceship =
        let glider =
            array2D [
                [cZero;cZero;cOne];
                [cOne;cZero;cOne];
                [cZero;cOne;cOne]
            ]
            |> Array2D.pad 1 cZero
        let lightWeight =
            array2D [
                [cZero;cOne;cZero;cZero;cOne];
                [cOne;cZero;cZero;cZero;cZero];
                [cOne;cZero;cZero;cZero;cOne];
                [cOne;cOne;cOne;cOne;cZero];
            ]
            |> Array2D.pad 1 cZero
    module Methuselah =
        let diehard =
            array2D [
                [cZero;cZero;cZero;cZero;cZero;cZero;cZero;cZero];
                [cOne;cOne;cZero;cZero;cZero;cZero;cOne;cZero];
                [cZero;cOne;cZero;cZero;cZero;cOne;cOne;cOne];
            ]
            |> Array2D.pad 1 cZero
        let rPentomino =
            array2D [
                [cZero;cOne;cOne];
                [cOne;cOne;cZero];
                [cZero;cOne;cZero]
            ]
            |> Array2D.pad 1 cZero
#warn "0058"
#warn "0064"