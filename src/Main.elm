module Main exposing (..)

import Browser
import Grid2d exposing (Grid)
import Html as RootHtml
import Html.Styled as Html


letters : Grid String
letters =
    case
        Grid2d.fromRowsAndColumns
            [ [ "a", "b", "c", "d" ]
            , [ "e", "f", "g", "h" ]
            , [ "i", "j", "k", "l" ]
            , [ "m", "n", "o", "p" ]
            ]
    of
        Ok grid ->
            grid

        Err problem ->
            Debug.todo (Debug.toString problem)


main : RootHtml.Html msg
main =
    letters
        |> Grid2d.view Html.text
        |> Html.toUnstyled
