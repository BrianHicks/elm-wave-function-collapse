module Wave exposing (Wave, getEntropy, init, step, view)

import Adjacency
import Dict exposing (Dict)
import Grid exposing (Grid)
import Heap exposing (Heap)
import Html.Styled as Html exposing (Html)
import Random exposing (Seed)
import Set exposing (Set)


type alias Entropy =
    { coords : { row : Int, column : Int }
    , entropy : Int
    }


type Cell
    = Done Int
    | Remaining (Set Int)


type Wave
    = Wave
        { weights : Dict Int Int
        , rules : Adjacency.Rules
        , entropy : Heap Entropy
        , items : Grid Cell
        }



-- DEBUG INFO


getEntropy : Wave -> Heap Entropy
getEntropy (Wave guts) =
    guts.entropy



-- END DEBUG


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
            Grid.fromDimensions (always (Remaining (Set.fromList (Dict.keys weights))))
                { rows = dimensions.height
                , columns = dimensions.width
                }
        }


step : Random.Seed -> Wave -> ( Wave, Random.Seed )
step seed (Wave wave) =
    let
        _ =
            Debug.log "entropy is" wave.entropy
    in
    case Heap.pop wave.entropy of
        Just ( { coords }, poppedEntropy ) ->
            collapse seed coords (Wave { wave | entropy = poppedEntropy })

        Nothing ->
            -- quittin' time!
            ( Wave wave, seed )


{-| Step one!
-}
collapse : Random.Seed -> { row : Int, column : Int } -> Wave -> ( Wave, Random.Seed )
collapse seed coords (Wave wave) =
    case Grid.get coords wave.items of
        Just (Remaining remaining) ->
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
                    ( propagate
                        final
                        coords
                        (Wave { wave | items = Grid.update (\_ -> Done final) coords wave.items })
                    , newSeed
                    )

                ( Nothing, _ ) ->
                    -- something went wrong and we eliminated all possible
                    -- values before getting here. We're definitely blocked
                    -- from moving forward, and should just bail (and maybe be
                    -- louder here in the future?)
                    ( Wave wave, seed )

        Just (Done _) ->
            -- we requested something that was already done, possibly because
            -- it was on the heap twice. No-op!
            ( Wave wave, seed )

        Nothing ->
            -- we requested something outside the grid for some reason? No-op.
            ( Wave wave, seed )


propagate : Int -> { row : Int, column : Int } -> Wave -> Wave
propagate finalValue coords (Wave wave) =
    case Dict.get finalValue wave.rules of
        Just rules ->
            rules
                |> List.filterMap
                    (\rule ->
                        let
                            target =
                                case rule.direction of
                                    Adjacency.Up ->
                                        { coords | column = coords.row - 1 }

                                    Adjacency.Down ->
                                        { coords | column = coords.row + 1 }

                                    Adjacency.Left ->
                                        { coords | row = coords.column - 1 }

                                    Adjacency.Right ->
                                        { coords | row = coords.column + 1 }
                        in
                        propagateAndGetEntropy target rule.to wave.weights wave.items
                    )
                |> List.foldl
                    (\( target, propagated, propagatedEntropy ) guts ->
                        { guts
                            | items = Grid.update (always (Remaining propagated)) target guts.items
                            , entropy = Heap.push propagatedEntropy guts.entropy
                        }
                    )
                    wave
                |> Wave

        Nothing ->
            -- no such rules for this final value? Weird but OK, I guess?
            Wave wave


propagateAndGetEntropy :
    { row : Int, column : Int }
    -> Set Int
    -> Dict Int Int
    -> Grid Cell
    -> Maybe ( { row : Int, column : Int }, Set Int, Entropy )
propagateAndGetEntropy coords restriction weights grid =
    case Grid.get coords grid of
        Just (Remaining current) ->
            let
                restricted =
                    Set.intersect current restriction
            in
            Just
                ( coords
                , restricted
                , { coords = coords, entropy = entropy weights restricted }
                )

        Just (Done _) ->
            Nothing

        Nothing ->
            Nothing


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
    Grid.view
        (\cell ->
            case cell of
                Done a ->
                    fn (Set.singleton a)

                Remaining remaining ->
                    fn remaining
        )
        items
