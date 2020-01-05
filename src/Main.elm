module Main exposing (..)

import Browser
import Css
import Html.Styled as Html
import Image2d


main =
    { pixels = [ Css.rgb 255 0 0, Css.rgb 255 255 255, Css.rgb 255 255 255, Css.rgb 255 0 0 ]
    , width = 2
    , height = 2
    }
        |> Image2d.view 10
        |> Html.toUnstyled
