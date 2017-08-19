module GameOfLife

type Cell = byte
/// Defines function adding 2 Cells together
let addFn (a: Cell) (b: Cell): Cell = a + b
let cZero: Cell = 0uy
let cOne: Cell = 1uy


type World = Cell[,]

module Pretty =
    let cell (zero: Cell) (isPretty: bool) (c: Cell) =
        if c <> zero then
            if isPretty then "x" else c.ToString()
        else
            if isPretty then " " else "0"
    let row (zero: Cell) (isPretty: bool) (r: Cell[]) =
        (if isPretty then "|" else "") +
        (r |> Array.map (cell zero isPretty) |> String.concat "") +
        (if isPretty then "|" else "")
    let world (zero: Cell) (isPretty: bool) (w: World) =
        (if isPretty then (Array.fold (fun c _ -> c + "_") "_" w.[0,*]) + "_\n" else "") +
        (w |> Array2D.toArray |> Array.map (row zero isPretty) |> String.concat "\n") +
        (if isPretty then "\n_" + (Array.fold (fun c _ -> c + "_") "_" w.[0,*]) else "")
    let worlds (zero: Cell) (isPretty: bool) (ws: World[]) =
        ws |> Array.map (world zero isPretty) |> String.concat("\n")

#nowarn "0058"
#nowarn "0064"
let one (mode: Array2D.TranslateMode) (w: World) =
    Array.map (fun i -> Array2D.andVal cZero cOne i (
        (Array.map ((Array2D.translate mode cZero) >> ((fun f -> f w))) [|
            (-1,-1);( 0,-1);( 1,-1);
            (-1, 0);( 0, 0);( 1, 0);
            (-1, 1);( 0, 1);( 1, 1);
        |])
        |> Array.reduce (Array2D.add addFn)
    )) <| [|3uy;4uy|]
    |> Array.zip [| Array2D.cloneWith cOne w; w |]
    |> Array.map (Array2D.andAnotherTuple cZero cOne)
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
            upTo < 1 ||
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