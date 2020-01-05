module Grid2d exposing (Grid, crop, fromRowsAndColumns, view, windows)

import Array exposing (Array)
import Css exposing (Color)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
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


fromRowsAndColumns : List (List a) -> Result FromRowsAndColumnsProblem (Grid a)
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
            (Ok << Grid)
                { items = arrayified
                , width = 0
                , height = Array.length arrayified
                }

        [ width ] ->
            (Ok << Grid)
                { items = arrayified
                , width = width
                , height = Array.length arrayified
                }

        a :: b :: _ ->
            Err (MoreThanOneWidth widths)


crop : { row : Int, column : Int, width : Int, height : Int } -> Grid a -> Maybe (Grid a)
crop rect (Grid grid) =
    if rect.row > grid.height || rect.column > grid.width then
        Nothing

    else
        (Just << Grid)
            { items =
                grid.items
                    |> Array.slice rect.row (rect.row + rect.height)
                    |> Array.map (Array.slice rect.column (rect.column + rect.width))
            , width = rect.width
            , height = rect.height
            }


windows : Int -> Grid a -> List (Grid a)
windows dirtySize ((Grid { width, height }) as grid) =
    let
        size =
            abs dirtySize

        columns =
            List.range 0 (width - size)

        rows =
            List.range 0 (height - size)
    in
    -- get coordinates
    rows
        |> List.map (\row -> List.map (\col -> ( row, col )) columns)
        |> List.concat
        -- get a list of crops
        |> List.filterMap
            (\( row, column ) ->
                crop
                    { row = row
                    , column = column
                    , width = size
                    , height = size
                    }
                    grid
            )


{-| TODO: could probably do this with CSS grids but I'm not sure how.
-}
view : (a -> Html msg) -> Grid a -> Html msg
view viewItem (Grid { items }) =
    items
        |> Array.map
            (Array.map (\item -> Html.td [] [ viewItem item ])
                >> Array.toList
                >> Html.tr []
            )
        |> Array.toList
        |> Html.table [ css [ Css.borderCollapse Css.collapse ] ]
