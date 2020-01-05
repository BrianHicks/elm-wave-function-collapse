module Main exposing (..)

import Browser
import Grid2d exposing (Grid)
import Html as RootHtml
import Html.Styled as Html


letters : Grid String
letters =
    case
        Grid2d.fromRowsAndColumns
            [ [ "┌", "─", "─", "┐" ]
            , [ "│", "┌", "┐", "│" ]
            , [ "│", "└", "┘", "│" ]
            , [ "└", "─", "─", "┘" ]
            ]
    of
        Ok grid ->
            grid

        Err problem ->
            Debug.todo (Debug.toString problem)


main : RootHtml.Html msg
main =
    Html.toUnstyled <|
        Html.div []
            [ Html.h1 [] [ Html.text "Wave Function Collapse" ]
            , Html.h2 [] [ Html.text "Source Image" ]
            , Grid2d.view Html.text letters
            , Html.h2 [] [ Html.text "Windows" ]
            ]
