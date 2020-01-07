module Wave exposing (Wave, init, view)

import Array exposing (Array)
import AssocSet as Set exposing (Set)
import Cell exposing (Cell)
import Css
import Grid exposing (Grid)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)


type Wave a
    = Wave { items : Grid (Cell a) }


init : { width : Int, height : Int } -> List a -> Wave a
init { width, height } windows =
    let
        rowsAndColumns =
            Cell.fromList windows
                |> always
                |> Array.initialize width
                |> always
                |> Array.initialize height
    in
    case Grid.fromRowsAndColumnsArray rowsAndColumns of
        Ok grid ->
            Wave { items = grid }

        -- TODO: there should be a better way to deal with this if this
        -- becomes a library.
        Err problem ->
            Debug.todo (Debug.toString problem)


view : (Set a -> Html msg) -> Wave a -> Html msg
view viewItems (Wave { items }) =
    Grid.view
        (\cell ->
            case Cell.state cell of
                Cell.Blocked ->
                    Html.td [] [ Html.text "X" ]

                Cell.Done item ->
                    viewItems (Set.singleton item)

                Cell.Remaining remaining ->
                    Html.td [] [ viewItems remaining ]
        )
        items
