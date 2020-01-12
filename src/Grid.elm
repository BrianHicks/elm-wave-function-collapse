module Grid exposing (FromRowsAndColumnsProblem, Grid, fromRowsAndColumns, fromRowsAndColumnsArray, get, getWrapping, indexedMap, initialize, map, rotate, set, toArrays, topLeft, update, view, windows)

import Array exposing (Array)
import Color.Transparent as Color exposing (Color)
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs exposing (css, style)
import Set


type Grid a
    = Grid
        { -- rows x columns
          items : Array a
        , width : Int
        , height : Int
        }


type FromRowsAndColumnsProblem
    = MoreThanOneWidth (List Int)


initialize : { rows : Int, columns : Int } -> ({ row : Int, column : Int } -> a) -> Grid a
initialize { rows, columns } init =
    Grid
        { items =
            Array.initialize (rows * columns)
                (\i ->
                    init
                        { row = i // rows
                        , column = modBy rows i
                        }
                )
        , width = columns
        , height = rows
        }


{-| Construct a grid from a list of lists. The outer list is a list of rows,
and the inner lists are values in columns.

If the sizes of the column arrays (the inner ones) don't match up, you'll get a
`MoreThanOneWidth` error back from this function.

-}
fromRowsAndColumns : List (List a) -> Grid a
fromRowsAndColumns rowsAndColumns =
    Array.fromList (List.map Array.fromList rowsAndColumns)
        |> fromRowsAndColumnsArray


fromRowsAndColumnsArray : Array (Array a) -> Grid a
fromRowsAndColumnsArray rowsAndColumns =
    Grid
        { items = Array.foldr Array.append Array.empty rowsAndColumns
        , width =
            Array.get 0 rowsAndColumns
                |> Maybe.map Array.length
                |> Maybe.withDefault 0
        , height = Array.length rowsAndColumns
        }


toArrays : Grid a -> Array (Array a)
toArrays (Grid grid) =
    Array.initialize grid.height
        (\row ->
            Array.slice
                (row * grid.width)
                (row * grid.width + grid.width)
                grid.items
        )


{-| Rotate a grid 90° clockwise.
-}
rotate : Grid a -> Grid a
rotate ((Grid { width, height }) as grid) =
    let
        newItems =
            List.range 0 (width - 1)
                |> List.map
                    (\col ->
                        column col grid
                            |> Maybe.withDefault Array.empty
                            |> Array.foldr Array.push Array.empty
                    )
                |> List.foldr Array.append Array.empty
    in
    Grid
        { items = newItems
        , height = width
        , width = height
        }


column : Int -> Grid a -> Maybe (Array a)
column colNum (Grid { items, height, width }) =
    List.range 0 (height - 1)
        |> List.foldl
            (\row soFar ->
                Maybe.andThen
                    (\arr ->
                        Array.get (row * width + colNum) items
                            |> Maybe.map (\val -> Array.push val arr)
                    )
                    soFar
            )
            (Just Array.empty)


{-| Get a number of windows over the given grid data. Windows wrap around the
edges of the input grid. We include tuples here, as they're useful as IDs.
-}
windows : { width : Int, height : Int } -> Grid a -> Grid (Grid a)
windows sizes ((Grid { width, height, items }) as grid) =
    Grid
        { items =
            Array.initialize (height * width)
                (\cell ->
                    cropWrapping
                        { row = cell // width
                        , column = modBy width cell
                        , width = sizes.width
                        , height = sizes.height
                        }
                        grid
                )
        , width = width
        , height = height
        }


cropWrapping : { row : Int, column : Int, width : Int, height : Int } -> Grid a -> Grid a
cropWrapping bounds (Grid grid) =
    let
        expanded =
            toArrays (Grid grid)
                |> Array.map (\row -> Array.append row row)
                |> (\all -> Array.append all all)

        wrappedRow =
            modBy grid.height bounds.row

        wrappedColumn =
            modBy grid.width bounds.column
    in
    Grid
        { items =
            expanded
                |> Array.slice wrappedRow (wrappedRow + bounds.height)
                |> Array.map (Array.slice wrappedColumn (wrappedColumn + bounds.width))
                |> Array.foldr Array.append Array.empty
        , width = bounds.width
        , height = bounds.height
        }


get : { row : Int, column : Int } -> Grid a -> Maybe a
get coords (Grid { items, width }) =
    Array.get (coords.row * width + coords.column) items


{-| Still a maybe because the grid could be empty
-}
getWrapping : { row : Int, column : Int } -> Grid a -> Maybe a
getWrapping coords ((Grid { width, height }) as grid) =
    get
        { row = modBy height coords.row
        , column = modBy width coords.column
        }
        grid


topLeft : Grid a -> Maybe a
topLeft =
    get { row = 0, column = 0 }


set : { row : Int, column : Int } -> a -> Grid a -> Grid a
set coords newValue (Grid grid) =
    Grid
        { grid
            | items =
                Array.set
                    (coords.row * grid.width + coords.column)
                    newValue
                    grid.items
        }


update : (a -> a) -> { row : Int, column : Int } -> Grid a -> Grid a
update fn coords grid =
    case get coords grid of
        Just item ->
            set coords (fn item) grid

        Nothing ->
            grid


indexedMap : ({ row : Int, column : Int } -> a -> b) -> Grid a -> Grid b
indexedMap fn (Grid grid) =
    Grid
        { items =
            Array.indexedMap
                (\i val ->
                    fn
                        { row = modBy grid.width i
                        , column = remainderBy grid.width i
                        }
                        val
                )
                grid.items
        , width = grid.width
        , height = grid.height
        }


map : (a -> b) -> Grid a -> Grid b
map fn =
    indexedMap (\_ a -> fn a)


{-| TODO: could probably do this with CSS grids but I'm not sure how.
-}
view : (a -> Html msg) -> Grid a -> Html msg
view viewItem grid =
    toArrays grid
        |> Array.map (Array.map viewItem >> Array.toList >> Html.tr [])
        |> Array.toList
        |> Html.table [ css [ Css.borderCollapse Css.collapse ] ]
