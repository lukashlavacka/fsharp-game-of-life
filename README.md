# Conway's Game Of Life in F# and ASP.NET Core 2.0

This implementation uses as source for its approach and logic my favourite programming video of all times [Conway's Game Of Life in APL](https://www.youtube.com/watch?v=a9xAKttWgP4)

The whole logic that generates the next iteration is in one function that takes as its input the current state as a 2D multidimensional array and generates the next iteration.

```fs
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
```

The only other input into this function (`Array2D.TranslateMode`) is how reading outside of the boundary of the 2D array should behave. I have implemented:

- Zero - reading all cells outside the 2D array are considered dead)
- Torus (or a donut) - the 2D array wraps from sides and top/bottom
-	Cylinder - the 2D array wraps only from the sides or only from top/bottom
- Möbius strip - the 2D array wraps only from the sides or only from top/bottom and flips
