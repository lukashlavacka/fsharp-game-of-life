module Tests

open System
open Xunit
open GameOfLife

[<Fact>]
let ``Array2D.translate does nothing`` () =
    let input = Array2D.init 3 3 (+)
    let output = input |> Array2D.translate Array2D.Mode.Donut 1 (0,0)
    Assert.Equal(input, output)

[<Fact>]
let ``Array2D.translate does something`` () =
    let input = Array2D.init 3 3 (+)
    let output = input |> Array2D.translate Array2D.Mode.Donut 1 (1,0)
    Assert.NotEqual(input, output)

[<Fact>]
let ``Array2D.translate adds`` () =
    let input = Array2D.init 3 3 (+)
    let output1 = input |> Array2D.translate Array2D.Mode.Donut 1 (2,0)
    let output2 = input |> Array2D.translate Array2D.Mode.Donut 1 (1,0) |> Array2D.translate Array2D.Mode.Donut 1 (1,0)
    Assert.Equal(output1, output2)

[<Fact>]
let ``Array2D.translate cancels`` () =
    let input = Array2D.init 3 3 (+)
    let output1 = input |> Array2D.translate Array2D.Mode.Donut 1 (1,0)
    let output2 = output1 |> Array2D.translate Array2D.Mode.Donut 1 (-1,0)
    Assert.Equal(input, output2)


[<Fact>]
let ``Array2D.translate larger than size`` () =
    let input = Array2D.init 3 3 (+)
    let output1 = input |> Array2D.translate Array2D.Mode.Donut 1 (1,0)
    let output2 = input |> Array2D.translate Array2D.Mode.Donut 1 (7,0)
    Assert.Equal(output1, output2)

[<Fact>]
let ``Array2D.translate negative larger than size`` () =
    let input = Array2D.init 3 3 (+)
    let output1 = input |> Array2D.translate Array2D.Mode.Donut 1 (-1,0)
    let output2 = input |> Array2D.translate Array2D.Mode.Donut 1 (-7,0)
    Assert.Equal(output1, output2)