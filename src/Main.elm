module Main exposing (..)

import Browser
import Color.Transparent as Color
import Css
import Html as RootHtml
import Html.Styled as Html
import Html.Styled.Attributes exposing (css)
import Image exposing (Image)


recurse : Image
recurse =
    let
        -- Transparent
        t =
            Color.fromRGBA { red = 0, green = 0, blue = 0, alpha = Color.transparent }

        -- White
        w =
            Color.fromRGBA { red = 255, green = 255, blue = 255, alpha = Color.opaque }

        -- Green
        g =
            Color.fromRGBA { red = 62, green = 192, blue = 108, alpha = Color.opaque }

        -- Key (black)
        k =
            Color.fromRGBA { red = 43, green = 45, blue = 45, alpha = Color.opaque }
    in
    case
        Image.fromRowsAndColumns
            [ List.repeat 14 t
            , [ t, k, k, k, k, k, k, k, k, k, k, k, k, t ]
            , [ t, k, t, t, t, t, t, t, t, t, t, t, k, t ]
            , [ t, k, t, k, k, k, k, k, k, k, k, t, k, t ]
            , [ t, k, t, k, k, k, k, k, k, k, k, t, k, t ]
            , [ t, k, t, g, k, g, k, g, k, k, k, t, k, t ]
            , [ t, k, t, k, k, k, k, k, k, k, k, t, k, t ]
            , [ t, k, t, k, g, g, k, g, g, k, k, t, k, t ]
            , [ t, k, t, k, k, k, k, k, k, k, k, t, k, t ]
            , [ t, k, t, k, k, k, k, k, k, k, k, t, k, t ]
            , [ t, k, t, t, t, t, t, t, t, t, t, t, k, t ]
            , [ t, k, k, k, k, k, k, k, k, k, k, k, k, t ]
            , [ t, t, t, t, t, k, k, k, k, t, t, t, t, t ]
            , [ t, t, k, k, k, k, k, k, k, k, k, k, t, t ]
            , [ t, k, k, k, w, k, w, k, w, k, w, k, k, t ]
            , [ t, k, k, w, k, w, k, w, k, w, k, k, k, t ]
            , [ t, k, k, k, k, k, k, k, k, k, k, k, k, t ]
            , List.repeat 14 t
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
            , Image.view recurse
            , Html.h2 [] [ Html.text "Windows" ]
            , recurse
                |> Image.windows { width = 3, height = 3 }
                |> List.map Image.view
                |> List.map (List.singleton >> Html.div [ css [ Css.border3 (Css.px 1) Css.solid (Css.hex "000"), Css.display Css.inlineBlock ] ])
                |> Html.section []
            ]
