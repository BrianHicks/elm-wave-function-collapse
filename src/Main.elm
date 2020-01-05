module Main exposing (..)

import Browser
import Css
import Grid2d exposing (Grid)
import Html as RootHtml
import Html.Styled as Html
import Html.Styled.Attributes exposing (css)


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
            , letters
                |> Grid2d.windows { width = 2, height = 2 }
                |> List.map (Grid2d.view Html.text)
                |> List.map (List.singleton >> Html.div [ css [ Css.border3 (Css.px 1) Css.solid (Css.hex "000") ] ])
                |> Html.section [ css [ Css.displayFlex ] ]
            ]
