module Wave exposing (..)

import Adjacency
import Dict exposing (Dict)
import Grid exposing (Grid)
import Heap exposing (Heap)
import Html.Styled as Html exposing (Html)
import Set exposing (Set)


type Wave
    = Wave
        { weights : Dict Int Int
        , rules : Adjacency.Rules
        , entropy : Heap { coords : { row : Int, column : Int }, entropy : Int }
        , items : Grid (Set Int)
        }


init : Adjacency.Rules -> Dict Int Int -> { width : Int, height : Int } -> Wave
init rules weights dimensions =
    let
        initialEntropy =
            entropy weights (Set.fromList (Dict.keys weights))
    in
    Wave
        { weights = weights
        , rules = rules
        , entropy =
            List.range 0 (dimensions.width - 1)
                |> List.concatMap
                    (\row ->
                        List.map
                            (\column ->
                                { coords = { row = row, column = column }
                                , entropy = initialEntropy
                                }
                            )
                            (List.range 0 (dimensions.height - 1))
                    )
                |> Heap.fromList
                    (Heap.smallest
                        |> Heap.by .entropy
                        |> Heap.thenBy (.coords >> .row)
                        |> Heap.thenBy (.coords >> .column)
                    )
        , items =
            Grid.fromDimensions (always (Set.fromList (Dict.keys weights)))
                { rows = dimensions.height
                , columns = dimensions.width
                }
        }


entropy : Dict comparable Int -> Set comparable -> Int
entropy probabilities possibilities =
    possibilities
        |> Set.toList
        |> List.map
            (\item ->
                probabilities
                    |> Dict.get item
                    |> Maybe.withDefault 0
            )
        |> List.sum


view : (Set Int -> Html msg) -> Wave -> Html msg
view fn (Wave { items }) =
    Grid.view fn items
