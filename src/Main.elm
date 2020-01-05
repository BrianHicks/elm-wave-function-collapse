module Main exposing (..)

import Browser
import Grid2d
import Html.Styled as Html


main =
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
                |> Grid2d.view Html.text
                |> Html.toUnstyled

        Err whatevs ->
            Html.toUnstyled (Html.text (Debug.toString whatevs))
