module Image2d exposing (..)

import Css exposing (Color)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import List.Extra exposing (greedyGroupsOf)


type alias Image =
    { pixels : List Color
    , width : Int
    , height : Int
    }


toHtml : Float -> Image -> Html msg
toHtml scale { pixels, width } =
    pixels
        |> greedyGroupsOf width
        |> List.map
            (List.map
                (\color ->
                    Html.td
                        [ css
                            [ Css.backgroundColor color
                            , Css.width (Css.px scale)
                            , Css.height (Css.px scale)
                            ]
                        ]
                        []
                )
                >> Html.tr []
            )
        |> Html.table [ css [ Css.borderCollapse Css.collapse ] ]
