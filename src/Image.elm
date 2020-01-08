module Image exposing (Image, bars, recurse, view, viewColor)

import Array exposing (Array)
import Color.Transparent as Color exposing (Color)
import Css
import Grid exposing (Grid)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs exposing (css, style)
import Set


type alias Image =
    Grid Color


viewColor : Color -> Html msg
viewColor color =
    Html.td
        [ style "background-color" (Color.toRGBAString color)
        , Attrs.width 10
        , Attrs.height 10
        ]
        []


view : Image -> Html msg
view =
    Grid.view viewColor


recurse : Image
recurse =
    let
        -- Transparent
        t =
            Color.fromRGBA { red = 200, green = 200, blue = 200, alpha = Color.opaque }

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
            , [ t, k, w, w, w, w, w, w, w, w, w, w, k, t ]
            , [ t, k, w, k, k, k, k, k, k, k, k, w, k, t ]
            , [ t, k, w, g, k, g, k, g, k, k, k, w, k, t ]
            , [ t, k, w, k, k, k, k, k, k, k, k, w, k, t ]
            , [ t, k, w, k, g, g, k, g, g, k, k, w, k, t ]
            , [ t, k, w, k, k, k, k, k, k, k, k, w, k, t ]
            , [ t, k, w, k, k, k, k, k, k, k, k, w, k, t ]
            , [ t, k, w, w, w, w, w, w, w, w, w, w, k, t ]
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


bars : Image
bars =
    let
        b =
            Color.fromRGBA { red = 0, green = 0, blue = 255, alpha = Color.opaque }

        w =
            Color.fromRGBA { red = 200, green = 200, blue = 200, alpha = Color.opaque }

        k =
            Color.fromRGBA { red = 0, green = 0, blue = 0, alpha = Color.opaque }
    in
    case
        Grid.fromRowsAndColumns
            [ [ k, k, k, k, k ]
            , [ b, b, b, b, b ]
            , [ w, w, w, w, w ]
            , [ b, b, b, b, b ]
            , [ k, k, k, k, k ]
            ]
    of
        Ok grid ->
            grid

        Err problem ->
            Debug.todo (Debug.toString problem)
