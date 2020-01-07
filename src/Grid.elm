module Grid exposing (FromRowsAndColumnsProblem, Grid, fromRowsAndColumns, fromRowsAndColumnsArray, get, rotate, view, windows)

import Array exposing (Array)
import Color.Transparent as Color exposing (Color)
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs exposing (css, style)
import Set


type Grid a
    = Grid
        { -- rows x columns
          items : Array (Array a)
        , width : Int
        , height : Int
        }


type FromRowsAndColumnsProblem
    = MoreThanOneWidth (List Int)


{-| Construct a grid from a list of lists. The outer list is a list of rows,
and the inner lists are values in columns.

If the sizes of the column arrays (the inner ones) don't match up, you'll get a
`MoreThanOneWidth` error back from this function.

-}
fromRowsAndColumns : List (List a) -> Result FromRowsAndColumnsProblem (Grid a)
fromRowsAndColumns rowsAndColumns =
    Array.fromList (List.map Array.fromList rowsAndColumns)
        |> fromRowsAndColumnsArray


fromRowsAndColumnsArray : Array (Array a) -> Result FromRowsAndColumnsProblem (Grid a)
fromRowsAndColumnsArray rowsAndColumns =
    let
        widths =
            rowsAndColumns
                |> Array.foldl (\row soFar -> Set.insert (Array.length row) soFar) Set.empty
                |> Set.toList
    in
    case widths of
        [] ->
            (Ok << Grid)
                { items = rowsAndColumns
                , width = 0
                , height = Array.length rowsAndColumns
                }

        [ width ] ->
            (Ok << Grid)
                { items = rowsAndColumns
                , width = width
                , height = Array.length rowsAndColumns
                }

        a :: b :: _ ->
            Err (MoreThanOneWidth widths)


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
                |> Array.fromList
    in
    Grid
        { items = newItems
        , height = width
        , width = height
        }


column : Int -> Grid a -> Maybe (Array a)
column colNum (Grid { items, height }) =
    List.range 0 (height - 1)
        |> List.foldl
            (\row soFar ->
                Maybe.andThen
                    (\arr ->
                        items
                            |> Array.get row
                            |> Maybe.andThen (Array.get colNum)
                            |> Maybe.map (\val -> Array.push val arr)
                    )
                    soFar
            )
            (Just Array.empty)


{-| Get a number of windows over the given grid data.
-}
windows : { width : Int, height : Int } -> Grid a -> List (Grid a)
windows sizes (Grid { width, height, items }) =
    let
        columns =
            List.range 0 (width - 1)

        rows =
            List.range 0 (height - 1)

        -- when we reach the edge, we just need to wrap around.
        -- Repeating once per axis should do it!
        expanded =
            Array.initialize (height * 2)
                (\i ->
                    let
                        row =
                            items
                                |> Array.get (modBy height i)
                                |> Maybe.withDefault Array.empty
                    in
                    Array.append row row
                )
    in
    -- get coordinates
    List.concatMap
        (\row ->
            List.map
                (\col ->
                    Grid
                        { items =
                            expanded
                                |> Array.slice row (row + sizes.height)
                                |> Array.map (Array.slice col (col + sizes.width))
                        , width = sizes.width
                        , height = sizes.height
                        }
                )
                columns
        )
        rows


get : { row : Int, column : Int } -> Grid a -> Maybe a
get coords (Grid { items }) =
    items
        |> Array.get coords.row
        |> Maybe.andThen (Array.get coords.column)


{-| TODO: could probably do this with CSS grids but I'm not sure how.
-}
view : (a -> Html msg) -> Grid a -> Html msg
view viewItem (Grid { items }) =
    items
        |> Array.map (Array.map viewItem >> Array.toList >> Html.tr [])
        |> Array.toList
        |> Html.table [ css [ Css.borderCollapse Css.collapse ] ]