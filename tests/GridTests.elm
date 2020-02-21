module GridTests exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Grid exposing (Grid)
import Test exposing (..)


getTest : Test
getTest =
    let
        grid =
            Grid.fromRowsAndColumns [ [ () ] ]
    in
    describe "get"
        [ test "in-bounds coordinates should return the value at the coordinates" <|
            \_ ->
                Expect.equal (Just ()) (Grid.get { row = 0, column = 0 } grid)
        , test "coordinates above the top row should return Nothing" <|
            \_ ->
                Expect.equal Nothing (Grid.get { row = -1, column = 0 } grid)
        , test "coordinates below the bottom row should return Nothing" <|
            \_ ->
                Expect.equal Nothing (Grid.get { row = 1, column = 0 } grid)
        , test "coordinates to the left of the leftmost column should return Nothing" <|
            \_ ->
                Expect.equal Nothing (Grid.get { row = 0, column = -1 } grid)
        , test "coordinates to the right of the rightmost row should return Nothing" <|
            \_ ->
                Expect.equal Nothing (Grid.get { row = 0, column = 1 } grid)
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


blank10x10 : Grid Int
blank10x10 =
    Grid.initialize { rows = 10, columns = 10 } (always 0)


coordsFuzzer : Fuzzer { row : Int, column : Int }
coordsFuzzer =
    Fuzz.map2 (\row column -> { row = row, column = column })
        (Fuzz.intRange 0 9)
        (Fuzz.intRange 0 9)
