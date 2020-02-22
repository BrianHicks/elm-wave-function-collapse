module Main exposing (..)

import Adjacency
import Array
import Browser
import Color.Transparent as Color
import Css
import Css.Reset as Reset
import Dict exposing (Dict)
import Grid exposing (Grid)
import Heap
import Html as RootHtml
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (css, style)
import Html.Styled.Events as Events
import Image exposing (Image)
import Murmur3
import Process
import Random
import Set exposing (Set)
import Task
import Wave exposing (Wave)


type alias Model =
    { -- OPTIONS
      options : Options
    , draftOptions : Options

    -- DISPLAY OPTIONS
    , dimUnknown : Bool

    -- INTERNAL STATE
    , running : Bool
    , wave : Wave Color.Color Int
    , seed : Random.Seed
    }


type alias Options =
    { image : Image
    , windowSize : { width : Int, height : Int }
    , waveSize : { width : Int, height : Int }
    }


type Msg
    = Start
    | Stop
    | Step
    | KeepRunning
    | Reset
    | SetDimUnknown Bool
    | SetRandomSeed Random.Seed
    | SetWaveSizeWidth String
    | SetWaveSizeHeight String
    | SetWindowSize { width : Int, height : Int }
    | SetImage Image
    | ResetDraftOptions
    | ApplyOptions


init : () -> ( Model, Cmd Msg )
init _ =
    let
        options =
            { image = Image.nyan
            , windowSize = { width = 2, height = 2 }
            , waveSize = { width = 20, height = 20 }
            }
    in
    ( { options = options
      , draftOptions = options
      , dimUnknown = True
      , running = False
      , wave =
            Wave.load
                { windowSize = options.windowSize
                , waveSize = options.waveSize
                , hash = imageHash
                }
                options.image
      , seed = Random.initialSeed 0
      }
    , Random.generate SetRandomSeed Random.independentSeed
    )


imageHash : Image -> Int
imageHash =
    Grid.toArrays
        >> Array.foldr
            (\row rowString ->
                Array.foldr (\value string -> Color.toRGBAString value ++ string)
                    rowString
                    row
            )
            ""
        >> Murmur3.hashString 0


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Step ->
            let
                ( newWave, newSeed ) =
                    Wave.step model.seed model.wave
            in
            ( { model | wave = newWave, seed = newSeed }
            , Cmd.none
            )

        Start ->
            update KeepRunning { model | running = True }

        Stop ->
            ( { model | running = False }, Cmd.none )

        KeepRunning ->
            if model.running then
                model
                    |> update Step
                    |> Tuple.mapSecond
                        (\cmd ->
                            Cmd.batch
                                [ Task.perform (\_ -> KeepRunning) (Process.sleep 0)
                                , cmd
                                ]
                        )

            else
                ( model, Cmd.none )

        Reset ->
            ( { model
                | wave =
                    Wave.load
                        { windowSize = model.options.windowSize
                        , waveSize = model.options.waveSize
                        , hash = imageHash
                        }
                        model.options.image
              }
            , Cmd.none
            )

        SetDimUnknown dimUnknown ->
            ( { model | dimUnknown = dimUnknown }, Cmd.none )

        SetRandomSeed seed ->
            ( { model | seed = seed }, Cmd.none )

        SetWaveSizeWidth input ->
            case String.toInt input of
                Just width ->
                    let
                        oldDraft =
                            model.draftOptions

                        oldWaveSize =
                            model.draftOptions.waveSize
                    in
                    ( { model | draftOptions = { oldDraft | waveSize = { oldWaveSize | width = width } } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SetWaveSizeHeight input ->
            case String.toInt input of
                Just height ->
                    let
                        oldDraft =
                            model.draftOptions

                        oldWaveSize =
                            model.draftOptions.waveSize
                    in
                    ( { model | draftOptions = { oldDraft | waveSize = { oldWaveSize | height = height } } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SetWindowSize windowSize ->
            let
                oldDraft =
                    model.draftOptions
            in
            ( { model | draftOptions = { oldDraft | windowSize = windowSize } }, Cmd.none )

        SetImage image ->
            let
                oldDraft =
                    model.draftOptions
            in
            ( { model | draftOptions = { oldDraft | image = image } }, Cmd.none )

        ResetDraftOptions ->
            ( { model | draftOptions = model.options }
            , Cmd.none
            )

        ApplyOptions ->
            ( { model
                | options = model.draftOptions
                , running = False
                , wave =
                    Wave.load
                        { windowSize = model.draftOptions.windowSize
                        , waveSize = model.draftOptions.waveSize
                        , hash = imageHash
                        }
                        model.draftOptions.image
              }
            , Cmd.none
            )


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
            , h2 [ Html.text "Options" ]
            , Html.form
                [ css [ Css.displayFlex ]
                , Events.onSubmit ApplyOptions
                ]
                [ fieldset
                    [ legend [ Html.text "Wave" ]
                    , Html.text "Wave Size"
                    , Html.div []
                        [ Html.input
                            [ Attributes.type_ "number"
                            , Attributes.min "1"
                            , Attributes.value (String.fromInt model.draftOptions.waveSize.width)
                            , Events.onInput SetWaveSizeWidth
                            ]
                            []
                        , Html.text "×"
                        , Html.input
                            [ Attributes.type_ "number"
                            , Attributes.min "1"
                            , Attributes.value (String.fromInt model.draftOptions.waveSize.height)
                            , Events.onInput SetWaveSizeHeight
                            ]
                            []
                        ]
                    , Html.text "Window Size"
                    , [ ( { width = 1, height = 1 }, "absurd" )
                      , ( { width = 2, height = 2 }, "default" )
                      , ( { width = 3, height = 3 }, "expensive" )
                      ]
                        |> List.map
                            (\( size, comment ) ->
                                Html.label []
                                    [ Html.input
                                        [ Attributes.type_ "radio"
                                        , Attributes.attribute "radiogroup" "windowsize"
                                        , Attributes.checked (model.draftOptions.windowSize == size)
                                        , Events.onCheck (\_ -> SetWindowSize size)
                                        ]
                                        []
                                    , Html.text (String.fromInt size.width ++ "×" ++ String.fromInt size.height ++ " (" ++ comment ++ ")")
                                    ]
                            )
                        |> Html.div []
                    , Html.text "Image"
                    , [ ( Image.bars, "Bars" )
                      , ( Image.mondrianCompositionIIinRedBlueAndYellow, "Composition II" )
                      , ( Image.nyan, "Nyan Cat" )
                      , ( Image.recurse, "RC Logo" )
                      , ( Image.waves, "Waves" )
                      ]
                        |> List.map
                            (\( image, name ) ->
                                Html.label []
                                    [ Html.input
                                        [ Attributes.type_ "radio"
                                        , Attributes.attribute "radiogroup" "image"
                                        , Attributes.checked (model.draftOptions.image == image)
                                        , Events.onCheck (\_ -> SetImage image)
                                        ]
                                        []
                                    , Html.text name
                                    ]
                            )
                        |> Html.div []
                    , if model.draftOptions /= model.options then
                        Html.div []
                            [ Html.button
                                [ Attributes.type_ "button" -- submit is default
                                , Events.onClick ResetDraftOptions
                                ]
                                [ Html.text "Reset Options" ]
                            , Html.button
                                [ Attributes.type_ "submit"
                                , Events.onClick ApplyOptions
                                ]
                                [ Html.text "Apply" ]
                            ]

                      else
                        Html.text ""
                    ]
                , fieldset
                    [ legend [ Html.text "View" ]
                    , Html.label []
                        [ Html.input
                            [ Attributes.type_ "checkbox"
                            , Attributes.checked model.dimUnknown
                            , Events.onCheck SetDimUnknown
                            ]
                            []
                        , Html.text "Toggle Dimming Unknown Cells"
                        ]
                    ]
                ]
            , h2 [ Html.text "Wave" ]
            , Html.p []
                [ if model.running then
                    Html.button [ Events.onClick Stop ] [ Html.text "Stop" ]

                  else
                    Html.button [ Events.onClick Start ] [ Html.text "Start" ]
                , Html.button [ Events.onClick Step ] [ Html.text "Step" ]
                , Html.button [ Events.onClick Reset ] [ Html.text "Reset" ]
                ]
            , Html.div [ css [ Css.displayFlex ] ]
                [ Image.view [] model.options.image
                , Wave.view
                    (\windows cell ->
                        case cell of
                            Wave.Collapsed tile ->
                                case Dict.get tile windows |> Maybe.andThen Grid.topLeft of
                                    Just color ->
                                        Image.viewColor [] color

                                    Nothing ->
                                        Html.text "invalid index"

                            Wave.Open indexes ->
                                let
                                    { reds, blues, greens, opacities } =
                                        indexes
                                            |> Set.toList
                                            |> List.filterMap (\i -> Dict.get i windows)
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
                                if Set.isEmpty indexes then
                                    Html.td
                                        [ css
                                            [ Css.lineHeight (Css.px 10)
                                            , Css.fontSize (Css.px 10)
                                            , Css.textAlign Css.center
                                            ]
                                        , Attributes.width 10
                                        , Attributes.height 10
                                        ]
                                        [ Html.text "×" ]

                                else
                                    Image.viewColor
                                        [ Attributes.attribute "data-count" (String.fromInt (Set.size indexes))
                                        , style "opacity"
                                            (if model.dimUnknown then
                                                "0.5"

                                             else
                                                "1"
                                            )
                                        ]
                                        (Color.fromRGBA
                                            { red = average reds
                                            , green = average greens
                                            , blue = average blues
                                            , alpha = Color.customOpacity (average opacities)
                                            }
                                        )
                    )
                    model.wave
                ]
            ]


h1 : List (Html msg) -> Html msg
h1 =
    Html.h1
        [ css
            [ Css.fontSize (Css.rem 2)
            , Css.lineHeight (Css.rem 2.5)
            , Css.marginBottom (Css.rem 0.5)
            , Css.fontWeight Css.bold
            ]
        ]


h2 : List (Html msg) -> Html msg
h2 =
    Html.h2
        [ css
            [ Css.fontSize (Css.rem 1.25)
            , Css.lineHeight (Css.rem 1.5)
            ]
        ]


fieldset : List (Html msg) -> Html msg
fieldset =
    Html.fieldset
        [ css
            [ Css.border3 (Css.px 1) Css.solid (Css.hex "000")
            , Css.padding (Css.px 10)
            , Css.margin (Css.px 5)
            ]
        ]


legend : List (Html msg) -> Html msg
legend =
    Html.legend [ css [ Css.padding2 (Css.px 3) (Css.px 6) ] ]
