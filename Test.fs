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

    [<Fact>]
    let ``Y cylinderY works`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let expected = array2D [[7;8;9];[1;2;3];[4;5;6]]
        let actual = input |> Array2D.translateY Array2D.Mode.CylinderY 0 1
        Assert.Equal(expected, actual)
    [<Fact>]
    let ``Y cylinderX works`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let expected = array2D [[0;0;0];[1;2;3];[4;5;6]]
        let actual = input |> Array2D.translateY Array2D.Mode.CylinderX 0 1
        Assert.Equal(expected, actual)
    [<Fact>]
    let ``X cylinderY works`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let expected = array2D [[0;1;2];[0;4;5];[0;7;8]]
        let actual = input |> Array2D.translateX Array2D.Mode.CylinderY 0 1
        Assert.Equal(expected, actual)
    [<Fact>]
    let ``X cylinderX works`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let expected = array2D [[3;1;2];[6;4;5];[9;7;8]]
        let actual = input |> Array2D.translateX Array2D.Mode.CylinderX 0 1
        Assert.Equal(expected, actual)
    [<Fact>]
    let ``cylinderY works`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let expected = array2D [[0;7;8];[0;1;2];[0;4;5]]
        let actual = input |> Array2D.translate Array2D.Mode.CylinderY 0 (1, 1)
        Assert.Equal(expected, actual)
    [<Fact>]
    let ``cylinderX works`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let expected = array2D [[0;0;0];[3;1;2];[6;4;5]]
        let actual = input |> Array2D.translate Array2D.Mode.CylinderX 0 (1, 1)
        Assert.Equal(expected, actual)
    [<Fact>]
    let ``X moebiusX works`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let expected = array2D [[9;1;2];[6;4;5];[3;7;8]]
        let actual = input |> Array2D.translateX Array2D.Mode.MoebiusX 0 1
        Assert.Equal(expected, actual)
    [<Fact>]
    let ``X moebiusX double loop works`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let expected = array2D [[3;1;2];[6;4;5];[9;7;8]]
        let actual = input |> Array2D.translateX Array2D.Mode.CylinderX 0 4
        Assert.Equal(expected, actual)
    [<Fact>]
    let ``Y moebiusY works`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let expected = array2D [[9;8;7];[1;2;3];[4;5;6]]
        let actual = input |> Array2D.translateY Array2D.Mode.MoebiusY 0 1
        Assert.Equal(expected, actual)
    [<Fact>]
    let ``Y moebiusY double loop works`` () =
        let input = array2D [[1;2;3];[4;5;6];[7;8;9]]
        let expected = array2D [[7;8;9];[1;2;3];[4;5;6]]
        let actual = input |> Array2D.translateY Array2D.Mode.CylinderY 0 4
        Assert.Equal(expected, actual)

module ``pretty`` =
    [<Fact>]
    let ``cell 1 pretty works`` () =
        let input = 1
        let expected = "x"
        let actual = input |> Pretty.cell true
        Assert.Equal(expected, actual)
    [<Fact>]
    let ``cell 1 unpretty works`` () =
        let input = 1
        let expected = "1"
        let actual = input |> Pretty.cell false
        Assert.Equal(expected, actual)
    [<Fact>]
    let ``cell 0 pretty works`` () =
        let input = 0
        let expected = " "
        let actual = input |> Pretty.cell true
        Assert.Equal(expected, actual)
    [<Fact>]
    let ``cell 0 unpretty works`` () =
        let input = 0
        let expected = "0"
        let actual = input |> Pretty.cell false
        Assert.Equal(expected, actual)
    [<Fact>]
    let ``row unpretty works`` () =
        let input = [| 1; 0; 1 |]
        let expected = "101"
        let actual = input |> Pretty.row false
        Assert.Equal(expected, actual)
    [<Fact>]
    let ``row pretty works`` () =
        let input = [| 1; 0; 1 |]
        let expected = "|x x|"
        let actual = input |> Pretty.row true
        Assert.Equal(expected, actual)
    [<Fact>]
    let ``world unpretty works`` () =
        let input = World(array2D [[1; 0; 1];[1; 0; 1];[1; 0; 1]])
        let expected = "101\n101\n101"
        let actual = input |> Pretty.world false
        Assert.Equal(expected, actual)
    [<Fact>]
    let ``world pretty works`` () =
        let input = World(array2D [[1; 0; 1];[1; 0; 1];[1; 0; 1]])
        let expected = "_____\n|x x|\n|x x|\n|x x|\n_____"
        let actual = input |> Pretty.world true
        Assert.Equal(expected, actual)

module ``life`` =
    [<Fact>]
    let `` evolution one`` () =
        let input = World(array2D [[0;0;0;0;0];[0;0;0;0;0];[0;1;1;1;0];[0;0;0;0;0];[0;0;0;0;0]])
        let expected = World(array2D [[0;0;0;0;0];[0;0;1;0;0];[0;0;1;0;0];[0;0;1;0;0];[0;0;0;0;0]])
        let actual = input |> life Array2D.Mode.Zero
        Assert.Equal(expected.World, actual.World)
    [<Fact>]
    let `` evolution two`` () =
        let input = World(array2D [[0;0;0;0;0];[0;0;0;0;0];[0;1;1;1;0];[0;0;0;0;0];[0;0;0;0;0]])
        let expected = World(array2D [[0;0;0;0;0];[0;0;0;0;0];[0;1;1;1;0];[0;0;0;0;0];[0;0;0;0;0]])
        let actual = input |> life Array2D.Mode.Zero |> life Array2D.Mode.Zero
        Assert.Equal(expected.World, actual.World)
    [<Fact>]
    let `` evolution Donut one`` () =
        let input = World(array2D [[1;1;0;0;1];[0;0;0;0;0];[0;0;0;0;0];[0;0;0;0;0];[0;0;0;0;0]])
        let expected = World(array2D [[1;0;0;0;0];[1;0;0;0;0];[0;0;0;0;0];[0;0;0;0;0];[1;0;0;0;0]])
        let actual = input |> life Array2D.Mode.Donut
        Assert.Equal(expected.World, actual.World)
    [<Fact>]
    let `` evolution Donut two`` () =
        let input = World(array2D [[1;0;0;1;1];[0;0;0;0;0];[0;0;0;0;0];[0;0;0;0;0];[0;0;0;0;0]])
        let expected = World(array2D [[1;0;0;1;1];[0;0;0;0;0];[0;0;0;0;0];[0;0;0;0;0];[0;0;0;0;0]])
        let actual = input |> life Array2D.Mode.Donut |> life Array2D.Mode.Donut
        Assert.Equal(expected.World, actual.World)
    [<Fact>]
    let ``Rec two works`` () =
        let input = World(array2D [[0;0;0;0;0];[0;0;0;0;0];[0;1;1;1;0];[0;0;0;0;0];[0;0;0;0;0]])
        let expected = input |> life Array2D.Mode.Zero |> life Array2D.Mode.Zero
        let actual = input |> lifeRec Array2D.Mode.Zero 2
        Assert.Equal(expected.World, actual.World)
