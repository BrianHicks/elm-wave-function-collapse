module Main exposing (..)

import Adjacency
import AssocSet as Set
import Browser
import Color.Transparent as Color
import Css
import Css.Reset as Reset
import Grid
import Heap
import Html as RootHtml
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css, style)
import Html.Styled.Events as Events
import Image exposing (Image)
import Process
import Random
import Task
import Wave exposing (Wave)


type alias Model =
    { image : Image
    , windowSize : { width : Int, height : Int }
    , windows : List Image
    , wave : Wave
    , waveSize : { width : Int, height : Int }
    , seed : Random.Seed
    , running : Bool
    }


type Msg
    = Reset { width : Int, height : Int }
    | Start
    | Stop
    | Step


init : () -> ( Model, Cmd Msg )
init _ =
    let
        image =
            Image.bars

        windowSize =
            { width = 3, height = 3 }

        windows =
            Grid.windows windowSize image
    in
    ( { image = image
      , windowSize = windowSize
      , windows = windows
      , wave = Wave.init { width = 10, height = 10 } windows
      , waveSize = { width = 10, height = 10 }
      , seed = Random.initialSeed 0
      , running = False
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset dimensions ->
            ( { model
                | wave = Wave.init dimensions model.windows
                , waveSize = dimensions
                , running = False
              }
            , Cmd.none
            )

        Step ->
            let
                ( newWave, newSeed ) =
                    Wave.step model.seed model.wave
            in
            ( { model | wave = newWave, seed = newSeed }
            , if model.running then
                Task.perform (\_ -> Step) (Process.sleep 0)

              else
                Cmd.none
            )

        Start ->
            update Step { model | running = True }

        Stop ->
            ( { model | running = False }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


view : Model -> RootHtml.Html Msg
view model =
    Html.toUnstyled <|
        Html.div
            [ css
                [ Css.fontFamily Css.sansSerif
                , Css.margin2 (Css.rem 1) Css.auto
                , Css.padding2 (Css.rem 1.5) (Css.rem 2)
                , Css.width (Css.pct 80)
                , Css.backgroundColor (Css.hex "F0F8FF")
                ]
            ]
            [ Reset.meyerV2
            , Reset.borderBoxV201408
            , h1 [ Html.text "Wave Function Collapse" ]
            , h2 [ Html.text "Source Image" ]
            , Image.view model.image
            , Html.details []
                [ Html.summary [] [ Html.text "Windows" ]
                , model.windows
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
                ]
            , h2 [ Html.text "Wave" ]
            , if model.running then
                Html.button [ Events.onClick Stop ] [ Html.text "Stop" ]

              else
                Html.button [ Events.onClick Start ] [ Html.text "Start" ]
            , Html.button [ Events.onClick Step ] [ Html.text "Step" ]
            , Html.button [ Events.onClick (Reset { width = 10, height = 10 }) ] [ Html.text "Reset (10x10)" ]
            , Html.button [ Events.onClick (Reset { width = 20, height = 20 }) ] [ Html.text "Reset (20x20)" ]
            , Wave.view
                (\colors ->
                    let
                        { reds, blues, greens, opacities } =
                            colors
                                |> Set.toList
                                |> List.filterMap Grid.topLeft
                                |> List.foldl
                                    (\color soFar ->
                                        let
                                            rgba =
                                                Color.toRGBA color
                                        in
                                        { reds = rgba.red :: soFar.reds
                                        , blues = rgba.blue :: soFar.blues
                                        , greens = rgba.green :: soFar.greens
                                        , opacities = Color.opacityToFloat rgba.alpha :: soFar.opacities
                                        }
                                    )
                                    { reds = [], greens = [], blues = [], opacities = [] }

                        average items =
                            List.sum items / toFloat (List.length items)
                    in
                    Image.viewColor
                        (Color.fromRGBA
                            { red = average reds
                            , green = average greens
                            , blue = average blues
                            , alpha = Color.customOpacity (average opacities)
                            }
                        )
                )
                model.wave
            , Html.div [ css [ Css.displayFlex, Css.justifyContent Css.spaceAround ] ]
                [ let
                    entropies =
                        Wave.getEntropies model.wave
                  in
                  Html.div []
                    [ Html.text "Entropy Info"
                    , Html.p []
                        [ Html.text "Next: "
                        , case Heap.peek entropies of
                            Nothing ->
                                Html.text "Nothing"

                            Just { row, column, entropy } ->
                                Html.span []
                                    [ Html.text (String.fromFloat entropy)
                                    , Html.text " @ "
                                    , Html.text (String.fromInt row)
                                    , Html.text ","
                                    , Html.text (String.fromInt column)
                                    ]
                        ]
                    , Html.p []
                        [ Html.text "Total: "
                        , Html.text (String.fromInt (Heap.size entropies))
                        ]
                    , entropies
                        |> Heap.toList
                        |> List.foldl
                            (\item -> Grid.update (Heap.push item) { row = item.row, column = item.column })
                            (Grid.initialize { rows = model.waveSize.height, columns = model.waveSize.width }
                                (\_ -> Heap.empty (Heap.smallest |> Heap.by .entropy))
                            )
                        |> Grid.view
                            (\heap ->
                                Html.td [ css [ Css.padding (Css.px 2) ] ]
                                    [ Heap.peek heap
                                        |> Maybe.map (.entropy >> String.fromFloat >> String.left 4)
                                        |> Maybe.withDefault "0"
                                        |> Html.text
                                    ]
                            )
                    ]
                , Html.div []
                    [ Html.text "Rules"
                    , Wave.getRules model.wave
                        |> Adjacency.toList
                        |> List.sortBy
                            (\{ from, offsetRows, offsetColumns } ->
                                ( Color.toRGBAString from
                                , offsetRows
                                , offsetColumns
                                )
                            )
                        |> List.map
                            (\{ from, to, offsetRows, offsetColumns } ->
                                let
                                    viewColor color =
                                        Html.div
                                            [ style "background-color" (Color.toRGBAString color)
                                            , css [ Css.width (Css.px 10), Css.height (Css.px 10) ]
                                            ]
                                            []
                                in
                                Html.tr []
                                    [ Html.td
                                        [ css [ Css.float Css.right ] ]
                                        [ viewColor from ]
                                    , Html.td
                                        [ css [ Css.textAlign Css.right ] ]
                                        [ Html.text (String.fromInt offsetRows) ]
                                    , Html.td
                                        [ css [ Css.textAlign Css.right ] ]
                                        [ Html.text (String.fromInt offsetColumns) ]
                                    , Html.td [ css [ Css.displayFlex ] ]
                                        (Set.toList to
                                            |> List.map viewColor
                                        )
                                    ]
                            )
                        |> (::)
                            (Html.tr []
                                [ Html.th [] [ Html.text "From" ]
                                , Html.th [] [ Html.text "↓" ]
                                , Html.th [] [ Html.text "→" ]
                                , Html.th [] [ Html.text "Allow" ]
                                ]
                            )
                        |> Html.table
                            [ css
                                [ Css.borderSpacing (Css.px 3)
                                , Css.borderCollapse Css.separate
                                ]
                            ]
                    ]
                ]
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
