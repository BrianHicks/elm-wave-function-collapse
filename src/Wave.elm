module Wave exposing (Wave, init, step, view)

import Adjacency
import Dict exposing (Dict)
import Grid exposing (Grid)
import Heap exposing (Heap)
import Html.Styled as Html exposing (Html)
import Random exposing (Seed)
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


step : Random.Seed -> Wave -> ( Wave, Random.Seed )
step seed (Wave wave) =
    case Heap.pop wave.entropy of
        Just ( { coords }, newEntropy ) ->
            collapse seed coords (Wave { wave | entropy = newEntropy })

        Nothing ->
            -- quittin' time!
            ( Wave wave, seed )


{-| Step one!
-}
collapse : Random.Seed -> { row : Int, column : Int } -> Wave -> ( Wave, Random.Seed )
collapse seed coords (Wave wave) =
    case Grid.get coords wave.items of
        Just remaining ->
            let
                generator =
                    wave.weights
                        |> Dict.toList
                        |> List.filterMap
                            (\( k, weight ) ->
                                if Set.member k remaining then
                                    Just ( toFloat weight, k )

                                else
                                    Nothing
                            )
                        |> (\weights ->
                                case weights of
                                    [] ->
                                        Random.constant Nothing

                                    [ ( _, only ) ] ->
                                        Random.constant (Just only)

                                    first :: rest ->
                                        Random.map Just (Random.weighted first rest)
                           )
            in
            case Random.step generator seed of
                ( Just final, newSeed ) ->
                    ( Wave { wave | items = Grid.update (\_ -> Set.singleton final) coords wave.items }
                    , newSeed
                    )

                ( Nothing, _ ) ->
                    -- something went wrong and we eliminated all possible
                    -- values before getting here. We're definitely blocked
                    -- from moving forward, and should just bail (and maybe be
                    -- louder here in the future?)
                    ( Wave wave, seed )

        Nothing ->
            -- we requested something outside the grid for some reason? No-op.
            ( Wave wave, seed )


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
