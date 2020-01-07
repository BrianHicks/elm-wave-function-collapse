module Main exposing (..)

import Browser
import Color.Transparent as Color
import Css
import Css.Reset as Reset
import Grid
import Html as RootHtml
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Image exposing (Image)
import Wave


recurse : Image
recurse =
    let
        -- Transparent
        t =
            Color.fromRGBA { red = 255, green = 255, blue = 255, alpha = Color.transparent }

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
        Grid.fromRowsAndColumns
            [ List.repeat 14 t
            , [ t, k, k, k, k, k, k, k, k, k, k, k, k, t ]
            , [ t, k, t, t, t, t, t, t, t, t, t, t, k, t ]
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
        Html.div
            [ css
                [ Css.fontFamily Css.sansSerif
                , Css.margin2 (Css.rem 2) Css.auto
                , Css.width (Css.pct 80)
                ]
            ]
            [ Reset.meyerV2
            , Reset.borderBoxV201408
            , h1 [ Html.text "Wave Function Collapse" ]
            , h2 [ Html.text "Source Image" ]
            , Image.view recurse
            , h2 [ Html.text "3×3 Windows" ]
            , recurse
                |> Grid.windows { width = 3, height = 3 }
                |> List.map Image.view
                |> List.map
                    (\image ->
                        Html.div
                            [ css
                                [ Css.border3 (Css.px 1) Css.solid (Css.hex "000")
                                , Css.display Css.inlineBlock
                                , Css.margin (Css.px 5)
                                ]
                            ]
                            [ image ]
                    )
                |> Html.section []
            , h2 [ Html.text "Wave" ]
            , recurse
                |> Grid.windows { width = 3, height = 3 }
                |> Wave.init { width = 20, height = 20 }
                |> Debug.toString
                |> Html.text
            ]


h1 : List (Html msg) -> Html msg
h1 contents =
    Html.h1
        [ css
            [ Css.fontSize (Css.rem 2)
            , Css.lineHeight (Css.rem 2.5)
            , Css.marginBottom (Css.rem 0.5)
            , Css.fontWeight Css.bold
            ]
        ]
        contents


h2 : List (Html msg) -> Html msg
h2 contents =
    Html.h2
        [ css
            [ Css.fontSize (Css.rem 1.25)
            , Css.lineHeight (Css.rem 1.5)
            ]
        ]
        contents
