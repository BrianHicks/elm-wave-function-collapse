module Wave exposing (Wave, getEntropy, init, step, view)

import Adjacency
import Dict exposing (Dict)
import Direction
import Grid exposing (Grid)
import Heap exposing (Heap)
import Html.Styled as Html exposing (Html)
import Random exposing (Seed)
import Set exposing (Set)


type alias Entropy =
    { coords : { row : Int, column : Int }
    , entropy : Int
    }


type Cell comparable
    = Done comparable
    | Remaining (Set comparable)


type Wave comparable
    = Wave
        { weights : Dict comparable Int
        , rules : Adjacency.Rules comparable
        , entropy : Heap Entropy
        , items : Grid (Cell comparable)
        }



-- DEBUG INFO


getEntropy : Wave comparable -> Heap Entropy
getEntropy (Wave guts) =
    guts.entropy



-- END DEBUG


init : Adjacency.Rules comparable -> Dict comparable Int -> { width : Int, height : Int } -> Wave comparable
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


step : Random.Seed -> Wave comparable -> ( Wave comparable, Random.Seed )
step seed (Wave wave) =
    case Heap.pop wave.entropy of
        Just ( { coords }, poppedEntropy ) ->
            collapse seed coords (Wave { wave | entropy = poppedEntropy })

        Nothing ->
            -- quittin' time!
            ( Wave wave, seed )


{-| Step one!
-}
collapse : Random.Seed -> { row : Int, column : Int } -> Wave comparable -> ( Wave comparable, Random.Seed )
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
            -- it was on the heap twice. Ignore it and pop the next item.
            step seed (Wave wave)

        Nothing ->
            -- we requested something outside the grid for some reason? No-op.
            ( Wave wave, seed )


propagate : { row : Int, column : Int } -> Wave comparable -> Wave comparable
propagate coords wave =
    propagateHelp [ coords ] wave


propagateHelp : List { row : Int, column : Int } -> Wave comparable -> Wave comparable
propagateHelp coordses (Wave wave) =
    case coordses of
        [] ->
            -- stack empty, we're done
            Wave wave

        coords :: rest ->
            let
                maybeRules =
                    Maybe.andThen
                        (\cell ->
                            case cell of
                                Done id ->
                                    Dict.get id wave.rules

                                Remaining possibilities ->
                                    -- TODO: probably should only do this in
                                    -- one pass. This code is going to get
                                    -- called a lot.
                                    possibilities
                                        |> Set.toList
                                        |> List.filterMap (\id -> Dict.get id wave.rules)
                                        |> List.concat
                                        |> Adjacency.combineRules
                                        |> Just
                        )
                        (Grid.get coords wave.items)

                ( propagatedWave, propagatedCoordses ) =
                    case maybeRules of
                        Just rules ->
                            rules
                                |> List.filterMap
                                    (\rule ->
                                        propagateAndGetEntropy
                                            (Direction.move coords rule.direction)
                                            rule.to
                                            wave.weights
                                            wave.items
                                    )
                                |> List.foldl
                                    (\( target, propagated, propagatedEntropy ) ( guts, toPropagate ) ->
                                        ( { guts
                                            | items = Grid.update (always (Remaining propagated)) target guts.items
                                            , entropy = Heap.push propagatedEntropy guts.entropy
                                          }
                                        , target :: toPropagate
                                        )
                                    )
                                    ( wave, rest )

                        Nothing ->
                            -- no such rules for this final value? Weird but OK, I guess?
                            ( wave, rest )
            in
            propagateHelp propagatedCoordses (Wave propagatedWave)


propagateAndGetEntropy :
    { row : Int, column : Int }
    -> Set comparable
    -> Dict comparable Int
    -> Grid (Cell comparable)
    -> Maybe ( { row : Int, column : Int }, Set comparable, Entropy )
propagateAndGetEntropy coords restriction weights grid =
    -- TODO: I could probably make the output tiled by using
    -- Grid.getWrapping here. Probably worth exploring later!
    case Grid.get coords grid of
        Just (Remaining current) ->
            let
                restricted =
                    Set.intersect current restriction
            in
            if restricted == current then
                -- no change! Don't consider this one changed
                Nothing

            else
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


view : (Set comparable -> Html msg) -> Wave comparable -> Html msg
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
