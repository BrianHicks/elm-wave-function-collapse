module GridTests exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Grid exposing (Grid)
import SlowGrid
import Test exposing (..)


getTest : Test
getTest =
    let
        overInUnderFuzzer =
            Fuzz.intRange -1 1
    in
    describe "get"
        [ fuzz2 overInUnderFuzzer overInUnderFuzzer "only returns values in bounds" <|
            \row column ->
                let
                    grid =
                        Grid.initialize { rows = 1, columns = 1 } (always ())
                in
                if row == 0 && column == 0 then
                    Expect.equal (Just ()) (Grid.get { row = row, column = column } grid)

                else
                    Expect.equal Nothing (Grid.get { row = row, column = column } grid)
        ]


getAndSetTest : Test
getAndSetTest =
    fuzz coordsFuzzer "setting a value and then getting the same value should work" <|
        \coords ->
            blank10x10
                |> Grid.set coords 1
                |> Grid.get coords
                |> Expect.equal (Just 1)


windowsTest : Test
windowsTest =
    describe "windows"
        [ test "creates a moving window over the grid data" <|
            \_ ->
                Grid.fromRowsAndColumns
                    [ [ 1, 2 ]
                    , [ 3, 4 ]
                    ]
                    |> Grid.windows { width = 2, height = 2 }
                    |> Expect.equal
                        (Grid.fromRowsAndColumns
                            [ [ Grid.fromRowsAndColumns [ [ 1, 2 ], [ 3, 4 ] ]
                              , Grid.fromRowsAndColumns [ [ 2, 1 ], [ 4, 3 ] ]
                              ]
                            , [ Grid.fromRowsAndColumns [ [ 3, 4 ], [ 1, 2 ] ]
                              , Grid.fromRowsAndColumns [ [ 4, 3 ], [ 2, 1 ] ]
                              ]
                            ]
                        )
        ]



{- Tests for compatibility with the older, slower, but known-good version. -}


compatibilityTest : Test
compatibilityTest =
    describe "compatibility with the array-of-arrays implementation"
        [ fuzz2 (Fuzz.intRange 0 2) (Fuzz.intRange 0 2) "initialize" <|
            \rows columns ->
                let
                    expected =
                        SlowGrid.toArrays (SlowGrid.initialize { rows = rows, columns = columns } identity)

                    actual =
                        Grid.toArrays (Grid.initialize { rows = rows, columns = columns } identity)
                in
                Expect.equal expected actual
        , fuzz2 (Fuzz.intRange 0 2) (Fuzz.intRange 0 2) "rotate" <|
            \rows columns ->
                let
                    expected =
                        SlowGrid.initialize { rows = rows, columns = columns } identity
                            |> SlowGrid.rotate
                            |> SlowGrid.toArrays

                    actual =
                        Grid.initialize { rows = rows, columns = columns } identity
                            |> Grid.rotate
                            |> Grid.toArrays
                in
                Expect.equal expected actual
        , fuzz2 (Fuzz.intRange 0 4) (Fuzz.intRange 0 4) "windows" <|
            \rows columns ->
                let
                    windowSize =
                        { width = columns // 2, height = rows // 2 }

                    expected =
                        SlowGrid.initialize { rows = rows, columns = columns } identity
                            |> SlowGrid.windows windowSize
                            |> SlowGrid.map SlowGrid.toArrays
                            |> SlowGrid.toArrays

                    actual =
                        Grid.initialize { rows = rows, columns = columns } identity
                            |> Grid.windows windowSize
                            |> Grid.map Grid.toArrays
                            |> Grid.toArrays
                in
                Expect.equal expected actual
        ]


blank10x10 : Grid Int
blank10x10 =
    Grid.initialize { rows = 10, columns = 10 } (always 0)


coordsFuzzer : Fuzzer { row : Int, column : Int }
coordsFuzzer =
    Fuzz.map2 (\row column -> { row = row, column = column })
        (Fuzz.intRange 0 9)
        (Fuzz.intRange 0 9)
