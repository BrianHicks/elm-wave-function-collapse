module Wave exposing (Wave, init, view)

import Array exposing (Array)
import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)
import Cell exposing (Cell)
import Css
import Grid exposing (Grid)
import Heap exposing (Heap)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)


type Wave a
    = Wave
        { items : Grid (Cell a)
        , probabilities : Dict a Int
        , entropies : Heap { row : Int, column : Int, entropy : Float }
        }


init : { width : Int, height : Int } -> List a -> Wave a
init { width, height } windows =
    let
        rowsAndColumns =
            Cell.fromList windows
                |> always
                |> Array.initialize width
                |> always
                |> Array.initialize height

        probabilities =
            List.foldl
                (\key dict ->
                    Dict.update
                        key
                        (\count ->
                            case count of
                                Nothing ->
                                    Just 1

                                Just n ->
                                    Just (n + 1)
                        )
                        dict
                )
                Dict.empty
                windows

        startingEntropy =
            entropy probabilities (Set.fromList windows)
    in
    case Grid.fromRowsAndColumnsArray rowsAndColumns of
        Ok grid ->
            Wave
                { items = grid
                , probabilities = probabilities
                , entropies =
                    List.range 0 height
                        |> List.concatMap
                            (\row ->
                                List.map
                                    (\col ->
                                        { row = row
                                        , column = col
                                        , entropy = startingEntropy
                                        }
                                    )
                                    (List.range 0 width)
                            )
                        |> Heap.fromList (Heap.smallest |> Heap.by .entropy)
                }

        -- TODO: there should be a better way to deal with this if this
        -- becomes a library.
        Err problem ->
            Debug.todo (Debug.toString problem)


entropy : Dict a Int -> Set a -> Float
entropy probabilities possibilities =
    let
        total =
            toFloat (List.sum (Dict.values probabilities))
    in
    possibilities
        |> Set.toList
        |> List.map
            (\item ->
                let
                    frequency =
                        probabilities
                            |> Dict.get item
                            |> Maybe.withDefault 0
                            |> toFloat

                    probability =
                        frequency / total
                in
                -probability * logBase 2 probability
            )
        |> List.sum


view : (Set a -> Html msg) -> Wave a -> Html msg
view viewItems (Wave { items }) =
    Grid.view
        (\cell ->
            case Cell.state cell of
                Cell.Blocked ->
                    Html.td [] [ Html.text "X" ]

                Cell.Done item ->
                    viewItems (Set.singleton item)

                Cell.Remaining remaining ->
                    Html.td [] [ viewItems remaining ]
        )
        items
