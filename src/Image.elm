module Image exposing (Image, crop, fromRowsAndColumns, rotate, view, windows)

import Array exposing (Array)
import Color.Transparent as Color exposing (Color)
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs exposing (css, style)
import Set


type Image
    = Image
        { -- rows x columns
          items : Array (Array Color)
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
fromRowsAndColumns : List (List Color) -> Result FromRowsAndColumnsProblem Image
fromRowsAndColumns rowsAndColumns =
    let
        arrayified =
            Array.fromList (List.map Array.fromList rowsAndColumns)

        widths =
            arrayified
                |> Array.foldl (\row soFar -> Set.insert (Array.length row) soFar) Set.empty
                |> Set.toList
    in
    case widths of
        [] ->
            (Ok << Image)
                { items = arrayified
                , width = 0
                , height = Array.length arrayified
                }

        [ width ] ->
            (Ok << Image)
                { items = arrayified
                , width = width
                , height = Array.length arrayified
                }

        a :: b :: _ ->
            Err (MoreThanOneWidth widths)


{-| Zoom in on a portion of a grid. If the row and column constraints are out
of bounds, this function returns `Nothing`.
-}
crop : { row : Int, column : Int, width : Int, height : Int } -> Image -> Maybe Image
crop rect (Image grid) =
    if rect.row > grid.height || rect.column > grid.width then
        Nothing

    else
        (Just << Image)
            { items =
                grid.items
                    |> Array.slice rect.row (rect.row + rect.height)
                    |> Array.map (Array.slice rect.column (rect.column + rect.width))
            , width = rect.width
            , height = rect.height
            }


{-| Rotate a grid 90° clockwise.
-}
rotate : Image -> Image
rotate ((Image { width, height }) as grid) =
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
    Image
        { items = newItems
        , height = width
        , width = height
        }


column : Int -> Image -> Maybe (Array Color)
column colNum (Image { items, height }) =
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


{-| Get a number of windows over the given grid data. This is for WCF.
-}
windows : { width : Int, height : Int } -> Image -> List Image
windows sizes ((Image { width, height }) as grid) =
    let
        columns =
            List.range 0 (width - abs sizes.width)

        rows =
            List.range 0 (height - abs sizes.height)
    in
    -- get coordinates
    rows
        |> List.map (\row -> List.map (\col -> ( row, col )) columns)
        |> List.concat
        -- get a list of crops
        |> List.filterMap
            (\( row, col ) ->
                crop
                    { row = row
                    , column = col
                    , width = sizes.width
                    , height = sizes.height
                    }
                    grid
            )


{-| TODO: could probably do this with CSS grids but I'm not sure how.
-}
view : Image -> Html msg
view (Image { items }) =
    items
        |> Array.map
            (Array.map
                (\color ->
                    Html.td
                        [ style "background-color" (Color.toRGBAString color)
                        , Attrs.width 5
                        , Attrs.height 5
                        ]
                        []
                )
                >> Array.toList
                >> Html.tr []
            )
        |> Array.toList
        |> Html.table [ css [ Css.borderCollapse Css.collapse ] ]
