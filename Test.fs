module Tests

open System
open Xunit
open GameOfLife

module ``Array2D `` =
    [<Fact>]
    let ``flatten works`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let expected = [1;2;3;4;5;6;7;8;9]
        let actual = input |> Array2D.flatten
        Assert.Equal(expected, actual)
    [<Fact>]
    let ``toArray works`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let expected = [|[|1;2;3|];[|4;5;6|];[|7;8;9|]|]
        let actual = input |> Array2D.toArray
        Assert.Equal<int[]>(expected, actual)

module ``Array2D translate`` =
    [<Fact>]
    let ``does nothing`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let actual = input |> Array2D.translate Array2D.Mode.Donut 1 (0,0)
        Assert.Equal(input, actual)

    [<Fact>]
    let ``does something`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let actual = input |> Array2D.translate Array2D.Mode.Donut 1 (1,0)
        Assert.NotEqual(input, actual)

    [<Fact>]
    let ``adds`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let actual1 = input |> Array2D.translate Array2D.Mode.Donut 1 (2,0)
        let actual2 = input |> Array2D.translate Array2D.Mode.Donut 1 (1,0) |> Array2D.translate Array2D.Mode.Donut 1 (1,0)
        Assert.Equal(actual1, actual2)

    [<Fact>]
    let ``cancels`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let actual1 = input |> Array2D.translate Array2D.Mode.Donut 1 (1,0)
        let actual2 = actual1 |> Array2D.translate Array2D.Mode.Donut 1 (-1,0)
        Assert.Equal(input, actual2)


    [<Fact>]
    let ``larger than size`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let actual1 = input |> Array2D.translate Array2D.Mode.Donut 1 (1,0)
        let actual2 = input |> Array2D.translate Array2D.Mode.Donut 1 (7,0)
        Assert.Equal(actual1, actual2)

    [<Fact>]
    let ``negative larger than size`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let actual1 = input |> Array2D.translate Array2D.Mode.Donut 1 (-1,0)
        let actual2 = input |> Array2D.translate Array2D.Mode.Donut 1 (-7,0)
        Assert.Equal(actual1, actual2)

    [<Fact>]
    let ``X zero does nothing`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let actual = input |> Array2D.translateX Array2D.Mode.Zero 0 0
        Assert.Equal(input, actual)

    [<Fact>]
    let ``X zero does something`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let actual = input |> Array2D.translateX Array2D.Mode.Zero 0 1
        Assert.NotEqual(input, actual)

    [<Fact>]
    let ``X zero 1`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let expected = array2D [[0;1;2];[0;4;5];[0;7;8]]
        let actual = input |> Array2D.translateX Array2D.Mode.Zero 0 1
        Assert.Equal(expected, actual)
        
    [<Fact>]
    let ``X zero -1`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let expected = array2D [[2;3;0];[5;6;0];[8;9;0]]
        let actual = input |> Array2D.translateX Array2D.Mode.Zero 0 -1
        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Y zero does nothing`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let actual = input |> Array2D.translateY Array2D.Mode.Zero 0 0
        Assert.Equal(input, actual)

    [<Fact>]
    let ``Y zero does something`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let actual = input |> Array2D.translateY Array2D.Mode.Zero 0 1
        Assert.NotEqual(input, actual)

    [<Fact>]
    let ``Y zero 1`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let expected = array2D [[0;0;0];[1;2;3];[4;5;6]]
        let actual = input |> Array2D.translateY Array2D.Mode.Zero 0 1
        Assert.Equal(expected, actual)
        
    [<Fact>]
    let ``Y zero -1`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let expected = array2D [[4;5;6];[7;8;9];[0;0;0]]
        let actual = input |> Array2D.translateY Array2D.Mode.Zero 0 -1
        Assert.Equal(expected, actual)

    [<Fact>]
    let ``X donut does nothing`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let actual = input |> Array2D.translateX Array2D.Mode.Donut 0 0
        Assert.Equal(input, actual)

    [<Fact>]
    let ``X donut does something`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let actual = input |> Array2D.translateX Array2D.Mode.Donut 0 1
        Assert.NotEqual(input, actual)

    [<Fact>]
    let ``X donut 1`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let expected = array2D [[3;1;2];[06;4;5];[9;7;8]]
        let actual = input |> Array2D.translateX Array2D.Mode.Donut 0 1
        Assert.Equal(expected, actual)
        
    [<Fact>]
    let ``X donut -1`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let expected = array2D [[2;3;1];[5;6;4];[8;9;7]]
        let actual = input |> Array2D.translateX Array2D.Mode.Donut 0 -1
        Assert.Equal(expected, actual)

    [<Fact>]
    let ``X donut 4`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let expected = array2D [[3;1;2];[06;4;5];[9;7;8]]
        let actual = input |> Array2D.translateX Array2D.Mode.Donut 0 4
        Assert.Equal(expected, actual)
        
    [<Fact>]
    let ``X donut -4`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let expected = array2D [[2;3;1];[5;6;4];[8;9;7]]
        let actual = input |> Array2D.translateX Array2D.Mode.Donut 0 -4
        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Y donut does nothing`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let actual = input |> Array2D.translateY Array2D.Mode.Donut 0 0
        Assert.Equal(input, actual)

    [<Fact>]
    let ``Y donut does something`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let actual = input |> Array2D.translateY Array2D.Mode.Donut 0 1
        Assert.NotEqual(input, actual)

    [<Fact>]
    let ``Y donut 1`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let expected = array2D [[7;8;9];[1;2;3];[4;5;6]]
        let actual = input |> Array2D.translateY Array2D.Mode.Donut 0 1
        Assert.Equal(expected, actual)
        
    [<Fact>]
    let ``Y donut -1`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let expected = array2D [[4;5;6];[7;8;9];[1;2;3]]
        let actual = input |> Array2D.translateY Array2D.Mode.Donut 0 -1
        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Y donut 4`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let expected = array2D [[7;8;9];[1;2;3];[4;5;6]]
        let actual = input |> Array2D.translateY Array2D.Mode.Donut 0 4
        Assert.Equal(expected, actual)
        
    [<Fact>]
    let ``Y donut -4`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let expected = array2D [[4;5;6];[7;8;9];[1;2;3]]
        let actual = input |> Array2D.translateY Array2D.Mode.Donut 0 -4
        Assert.Equal(expected, actual)

module ``pretty`` =
    [<Fact>]
    let ``cell alive pretty works`` () =
        let input = Alive
        let expected = "■"
        let actual = input |> Pretty.cell true
        Assert.Equal(expected, actual)
    [<Fact>]
    let ``cell alive unpretty works`` () =
        let input = Alive
        let expected = "1"
        let actual = input |> Pretty.cell false
        Assert.Equal(expected, actual)
    [<Fact>]
    let ``cell dead pretty works`` () =
        let input = Dead
        let expected = " "
        let actual = input |> Pretty.cell true
        Assert.Equal(expected, actual)
    [<Fact>]
    let ``cell dead unpretty works`` () =
        let input = Dead
        let expected = "0"
        let actual = input |> Pretty.cell false
        Assert.Equal(expected, actual)
    [<Fact>]
    let ``row unpretty works`` () =
        let input = [| Alive; Dead; Alive |]
        let expected = "101"
        let actual = input |> Pretty.row false
        Assert.Equal(expected, actual)
    [<Fact>]
    let ``row pretty works`` () =
        let input = [| Alive; Dead; Alive |]
        let expected = "|■ ■|"
        let actual = input |> Pretty.row true
        Assert.Equal(expected, actual)
    [<Fact>]
    let ``world unpretty works`` () =
        let input = array2D [[Alive; Dead; Alive];[Alive; Dead; Alive];[Alive; Dead; Alive]]
        let expected = "101\n101\n101"
        let actual = input |> Pretty.world false
        Assert.Equal(expected, actual)
    [<Fact>]
    let ``world pretty works`` () =
        let input = array2D [[Alive; Dead; Alive];[Alive; Dead; Alive];[Alive; Dead; Alive]]
        let expected = "_____\n|■ ■|\n|■ ■|\n|■ ■|\n‾‾‾‾‾"
        let actual = input |> Pretty.world true
        Assert.Equal(expected, actual)