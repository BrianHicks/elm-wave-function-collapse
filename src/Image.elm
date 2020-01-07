module Image exposing (Image, view)

import Array exposing (Array)
import Color.Transparent as Color exposing (Color)
import Css
import Grid exposing (Grid)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs exposing (css, style)
import Set


type alias Image =
    Grid Color


view : Image -> Html msg
view =
    Grid.view
        (\color ->
            Html.td
                [ style "background-color" (Color.toRGBAString color)
                , Attrs.width 10
                , Attrs.height 10
                ]
                []
        )
