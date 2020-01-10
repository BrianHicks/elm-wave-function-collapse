module Wave exposing (Wave, getEntropy, init, step, view)

import Adjacency
import Dict exposing (Dict)
import Direction exposing (Direction)
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
    = Open (Dict ( Direction, comparable ) Int)
    | Collapsed comparable


enabledForCell : Cell comparable -> Set comparable
enabledForCell cell =
    case cell of
        Open enablers ->
            enablers
                |> Dict.keys
                |> List.map Tuple.second
                |> Set.fromList

        Collapsed _ ->
            Set.empty


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
        initialCell =
            Dict.foldl
                (\_ rules_ outFromRules ->
                    List.foldl
                        (\{ direction, to } outFromRule ->
                            List.foldl
                                (\value ->
                                    Dict.update ( direction, value )
                                        (\maybeCount ->
                                            case maybeCount of
                                                Nothing ->
                                                    Just 1

                                                Just count ->
                                                    Just (count + 1)
                                        )
                                )
                                outFromRule
                                (Set.toList to)
                        )
                        outFromRules
                        rules_
                )
                Dict.empty
                rules
                |> Open

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
            Grid.fromDimensions (always initialCell)
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
        Just ((Open _) as enablers) ->
            let
                remaining =
                    enabledForCell enablers

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
                        (Set.remove final remaining
                            |> Set.toList
                            |> List.map (\toRemove -> ( coords, toRemove ))
                        )
                        (Wave { wave | items = Grid.update (\_ -> Collapsed final) coords wave.items })
                    , newSeed
                    )

                ( Nothing, _ ) ->
                    -- something went wrong and we eliminated all possible
                    -- values before getting here. We're definitely blocked
                    -- from moving forward, and should just bail (and maybe be
                    -- louder here in the future?)
                    ( Wave wave, seed )

        Just (Collapsed _) ->
            -- we requested something that was already collapsed, possibly
            -- because it was on the heap twice. Ignore it and pop the next
            -- item.
            step seed (Wave wave)

        Nothing ->
            -- we requested something outside the grid for some reason? No-op.
            ( Wave wave, seed )


propagate :
    List ( { row : Int, column : Int }, comparable )
    -> Wave comparable
    -> Wave comparable
propagate removals (Wave wave) =
    case removals of
        [] ->
            -- stack empty, we're done
            Wave wave

        ( coords, toRemove ) :: rest ->
            case Dict.get toRemove wave.rules of
                -- count down enablers in that direction.
                -- if enablers becomes zero, recur and remove the instance
                Just rules ->
                    rules
                        |> List.concatMap
                            (\{ direction, to } -> Set.toList to |> List.map (\id -> ( direction, id )))
                        |> List.foldl
                            (\( direction, id ) ( innerWave, innerRest {- TODO: better names -} ) ->
                                -- TODO: could use wrapping get here to make
                                -- rules wrap around edges. Explore later!
                                let
                                    target =
                                        Direction.move coords direction
                                in
                                case Grid.get target wave.items of
                                    Just (Collapsed _) ->
                                        -- we don't need to eliminated already-collapsed cells.
                                        -- I don't know if this is correct, though... what if
                                        -- there is a conflict? We wouldn't know about it by
                                        -- ignoring this case. If output seems weird, it may
                                        -- be worth looking here.
                                        ( innerWave, innerRest )

                                    Nothing ->
                                        ( innerWave, innerRest )

                                    Just (Open enablers) ->
                                        let
                                            key =
                                                ( direction, id )

                                            newEnablers =
                                                Dict.update key
                                                    (Maybe.andThen
                                                        (\count ->
                                                            if count == 1 then
                                                                Nothing

                                                            else
                                                                Just (count - 1)
                                                        )
                                                    )
                                                    enablers

                                            newCell =
                                                Open newEnablers
                                        in
                                        ( { innerWave
                                            | entropy =
                                                if enablers /= newEnablers then
                                                    Heap.push
                                                        { coords = target
                                                        , entropy = entropy innerWave.weights (enabledForCell newCell)
                                                        }
                                                        innerWave.entropy

                                                else
                                                    innerWave.entropy
                                            , items = Grid.set target newCell innerWave.items
                                          }
                                        , if enablers /= newEnablers && Dict.get key newEnablers == Nothing then
                                            ( target, id ) :: innerRest

                                          else
                                            innerRest
                                        )
                            )
                            ( wave, rest )
                        |> (\( newWave, newRest ) -> propagate newRest (Wave newWave))

                Nothing ->
                    -- no rules for this case, which is unusual but probably fine
                    propagate rest (Wave wave)


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
                Collapsed a ->
                    fn (Set.singleton a)

                Open enablers ->
                    fn (enabledForCell cell)
        )
        items
