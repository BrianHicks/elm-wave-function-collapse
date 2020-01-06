module Main exposing (..)

import Browser
import Css
import Html as RootHtml
import Html.Styled as Html
import Html.Styled.Attributes exposing (css)
import Image exposing (Image)


boxes : Image String
boxes =
    case
        Image.fromRowsAndColumns
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


letters : Image String
letters =
    case
        Image.fromRowsAndColumns
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
    Html.toUnstyled <|
        Html.div []
            [ Html.h1 [] [ Html.text "Wave Function Collapse" ]
            , Html.h2 [] [ Html.text "Source Image" ]
            , Image.view Html.text letters
            , Html.h2 [] [ Html.text "Windows" ]
            , letters
                |> Image.windows { width = 2, height = 2 }
                |> List.map (Image.view Html.text)
                |> List.map (List.singleton >> Html.div [ css [ Css.border3 (Css.px 1) Css.solid (Css.hex "000") ] ])
                |> Html.section [ css [ Css.displayFlex ] ]
            ]
