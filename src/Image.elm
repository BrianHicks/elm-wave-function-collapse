module Image exposing (Image, bars, nri, nyan, recurse, view, viewColor, waves)

import Array exposing (Array)
import Color.Transparent as Color exposing (Color)
import Css
import Grid exposing (Grid)
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attrs exposing (css, style)
import Palette.Tango as Tango
import Set


type alias Image =
    Grid Color


viewColor : List (Attribute msg) -> Color -> Html msg
viewColor attrs color =
    Html.td
        ([ style "background-color" (Color.toRGBAString color)
         , Attrs.width 10
         , Attrs.height 10
         ]
            ++ attrs
        )
        []


view : List (Attribute msg) -> Image -> Html msg
view attrs image =
    Grid.view (viewColor attrs) image


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


bars : Image
bars =
    let
        b =
            Color.fromColor Color.opaque Tango.skyBlue1

        k =
            Color.fromRGBA { red = 45, green = 45, blue = 45, alpha = Color.opaque }
    in
    Grid.fromRowsAndColumns
        [ [ k, b, k, k, k ]
        , [ b, b, b, b, b ]
        , [ k, b, k, k, k ]
        , [ k, b, k, k, k ]
        , [ k, b, k, k, k ]
        ]


waves : Image
waves =
    let
        l =
            Color.fromColor Color.opaque Tango.skyBlue1

        d =
            Color.fromColor Color.opaque Tango.skyBlue3
    in
    Grid.fromRowsAndColumns
        [ [ d, d, d, d, d, d, d, l ]
        , [ l, d, d, d, d, d, d, d ]
        , [ d, l, l, l, d, d, d, d ]
        , [ d, d, d, d, l, l, d, d ]
        , [ d, d, d, d, d, d, l, l ]
        , [ l, l, l, d, d, d, d, d ]
        , [ d, d, d, l, l, d, d, d ]
        , [ d, d, d, d, d, l, l, d ]
        ]


nyan : Image
nyan =
    let
        -- Transparent
        t =
            Color.fromRGBA { red = 255, green = 255, blue = 255, alpha = Color.transparent }

        -- White
        w =
            Color.fromRGBA { red = 255, green = 255, blue = 255, alpha = Color.opaque }

        -- Pink
        p =
            Color.fromRGBA { red = 255, green = 128, blue = 128, alpha = Color.opaque }

        -- Grey
        g =
            Color.fromRGBA { red = 128, green = 128, blue = 128, alpha = Color.opaque }

        -- Key (black)
        k =
            Color.fromRGBA { red = 43, green = 45, blue = 45, alpha = Color.opaque }
    in
    Grid.fromRowsAndColumns
        [ List.repeat 18 t
        , [ t, t, k, k, t, t, t, t, t, t, t, t, t, k, k, t, t, t ]
        , [ t, t, k, g, k, t, t, t, t, t, t, t, k, g, g, k, t, t ]
        , [ t, t, k, g, g, k, t, t, t, t, t, k, g, g, g, k, t, t ]
        , [ t, t, k, g, g, g, k, k, k, k, k, g, g, g, g, k, t, t ]
        , [ t, k, g, g, g, g, g, g, g, g, g, g, g, g, g, k, t, t ]
        , [ t, k, g, g, g, g, g, g, g, g, g, g, g, g, g, k, t, t ]
        , [ t, k, g, g, g, g, g, g, g, g, g, g, g, g, g, g, k, t ]
        , [ t, k, g, g, g, w, k, g, g, g, g, g, w, k, g, g, k, t ]
        , [ t, k, g, g, g, k, k, g, g, g, k, g, k, k, g, g, k, t ]
        , [ t, k, g, p, p, g, g, g, g, g, g, g, g, g, p, p, k, t ]
        , [ t, k, g, p, p, g, k, g, g, k, g, g, k, g, p, p, k, t ]
        , [ t, t, k, g, g, g, k, k, k, k, k, k, k, g, g, k, t, t ]
        , [ t, t, t, k, g, g, g, g, g, g, g, g, g, g, k, t, t, t ]
        , [ t, t, t, t, k, k, k, k, k, k, k, k, k, k, t, t, t, t ]
        , List.repeat 18 t
        ]


nri : Image
nri =
    let
        w =
            Color.fromRGBA { red = 255, green = 255, blue = 255, alpha = Color.opaque }

        l =
            Color.fromRGBA { red = 247, green = 127, blue = 163, alpha = Color.opaque }

        p =
            Color.fromRGBA { red = 243, green = 52, blue = 108, alpha = Color.opaque }
    in
    Grid.fromRowsAndColumns
        [ List.repeat 32 p
        , List.repeat 32 p
        , List.repeat 32 p
        , List.repeat 32 p
        , List.repeat 32 p
        , List.repeat 32 p
        , [ p, p, p, p, p, p, p, p, p, l, l, l, l, p, p, l, w, w, w, w, l, p, p, p, p, p, p, p, p, p, p, p ]
        , [ p, p, p, p, p, p, p, p, l, w, w, w, w, l, l, w, w, w, w, w, w, l, p, p, p, p, p, p, p, p, p, p ]
        , [ p, p, p, p, p, p, p, p, l, w, w, w, w, l, w, w, w, w, w, w, w, w, l, p, p, p, p, p, p, p, p, p ]
        , [ p, p, p, p, p, p, p, p, l, w, w, w, w, w, w, w, w, w, w, w, w, w, w, p, p, p, p, p, p, p, p, p ]
        , [ p, p, p, p, p, p, p, p, l, w, w, w, w, w, l, p, p, l, w, w, w, w, w, l, p, p, p, p, p, p, p, p ]
        , [ p, p, p, p, p, p, p, p, l, w, w, w, w, l, p, p, p, p, l, w, w, w, w, l, p, p, p, p, p, p, p, p ]
        , [ p, p, p, p, p, p, p, p, l, w, w, w, w, l, p, p, p, p, l, w, w, w, w, l, p, p, p, p, p, p, p, p ]
        , [ p, p, p, p, p, p, p, p, l, w, w, w, w, l, p, p, p, p, l, w, w, w, w, l, p, p, p, p, p, p, p, p ]
        , [ p, p, p, p, p, p, p, p, l, w, w, w, w, l, p, p, p, p, l, w, w, w, w, l, p, p, p, p, p, p, p, p ]
        , [ p, p, p, p, p, p, p, p, l, w, w, w, w, l, p, p, p, p, l, w, w, w, w, l, p, p, p, p, p, p, p, p ]
        , [ p, p, p, p, p, p, p, p, l, w, w, w, w, l, p, p, p, p, l, w, w, w, w, l, p, p, p, p, p, p, p, p ]
        , [ p, p, p, p, p, p, p, p, l, w, w, w, w, l, p, p, p, p, l, w, w, w, w, l, p, p, p, p, p, p, p, p ]
        , [ p, p, p, p, p, p, p, p, l, w, w, w, w, l, p, p, p, p, l, w, w, w, w, l, p, p, p, p, p, p, p, p ]
        , [ p, p, p, p, p, p, p, p, l, w, w, w, w, l, p, p, p, p, l, w, w, w, w, l, p, p, p, p, p, p, p, p ]
        , [ p, p, p, p, p, p, p, p, l, w, w, w, w, l, p, p, p, p, l, w, w, w, w, l, p, p, p, p, p, p, p, p ]
        , [ p, p, p, p, p, p, p, p, l, w, w, w, w, l, p, p, p, p, l, w, w, w, w, l, p, p, p, p, p, p, p, p ]
        , [ p, p, p, p, p, p, p, p, l, w, w, w, w, l, p, p, p, p, l, w, w, w, w, l, p, p, p, p, p, p, p, p ]
        , [ p, p, p, p, p, p, p, p, l, w, w, w, w, l, p, p, p, p, l, w, w, w, w, l, p, p, p, p, p, p, p, p ]
        , [ p, p, p, p, p, p, p, p, l, w, w, w, w, l, p, p, p, p, l, w, w, w, w, l, p, p, p, p, p, p, p, p ]
        , [ p, p, p, p, p, p, p, p, l, w, w, w, w, l, p, p, p, p, l, w, w, w, w, l, p, p, p, p, p, p, p, p ]
        , List.repeat 32 p
        , List.repeat 32 p
        , List.repeat 32 p
        , List.repeat 32 p
        , List.repeat 32 p
        , List.repeat 32 p
        ]
