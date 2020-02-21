module Wave exposing (Cell(..), Wave, getEntropy, init, step, view)

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
    , entropy : Float
    }


type Cell comparable
    = Open (Set comparable)
    | Collapsed comparable


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
            Open (Set.fromList (Dict.keys weights))

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
            Grid.initialize
                { rows = dimensions.height
                , columns = dimensions.width
                }
                (always initialCell)
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
        Just (Open remaining) ->
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
                        [ coords ]
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
    List { row : Int, column : Int }
    -> Wave comparable
    -> Wave comparable
propagate todo (Wave wave) =
    case todo of
        [] ->
            -- stack empty, we're done
            Wave wave

        target :: rest ->
            case Grid.get target wave.items of
                Nothing ->
                    -- requested cell was out of bounds. Ignore and move on.
                    propagate rest (Wave wave)

                Just cell ->
                    -- Starting at the given cell, constrain neighbor cells to
                    -- possibilities compatible with the ones in this cell. If
                    -- we reduce the number of possibilities, we then
                    -- propagate again based on that new cell.
                    --
                    -- While doing this, if a cell becomes finalized (that is,
                    -- we remove all possibilites but one) we mark them as such
                    -- and continue propagation.
                    --
                    -- If a cell becomes blocked (that is, we remove all
                    -- possibilities) we mark it as such and stop propagation.
                    let
                        ( propagatedWave, propagatedTodo ) =
                            List.foldl (propagateInDirection target cell)
                                ( Wave wave, rest )
                                [ Direction.up, Direction.down, Direction.left, Direction.right ]
                    in
                    -- be careful that this can be TCO'd
                    propagate propagatedTodo propagatedWave


propagateInDirection :
    { row : Int, column : Int }
    -> Cell comparable
    -> Direction
    -> ( Wave comparable, List { row : Int, column : Int } )
    -> ( Wave comparable, List { row : Int, column : Int } )
propagateInDirection source sourceCell direction ( Wave wave, todo ) =
    let
        target =
            Direction.move source direction
    in
    case Grid.get target wave.items of
        Nothing ->
            -- out of bounds, skip
            ( Wave wave, todo )

        Just (Collapsed _) ->
            -- we don't need to consider collapsed cells
            ( Wave wave, todo )

        Just (Open remaining) ->
            let
                possibleInDirection =
                    case sourceCell of
                        Collapsed value ->
                            Dict.get ( value, direction ) wave.rules
                                |> Maybe.withDefault Set.empty

                        Open remaining_ ->
                            remaining_
                                |> Set.toList
                                |> List.filterMap (\value -> Dict.get ( value, direction ) wave.rules)
                                |> List.foldl Set.union Set.empty

                reduced =
                    Set.intersect remaining possibleInDirection
            in
            if reduced == remaining {- i.e. it didn't change -} then
                ( Wave wave, todo )

            else
                ( Wave
                    { wave
                        | items = Grid.set target (Open reduced) wave.items
                        , entropy =
                            Heap.push
                                { coords = target
                                , entropy = entropy wave.weights reduced
                                }
                                wave.entropy
                    }
                , if Set.isEmpty reduced then
                    -- we're blocked and should not propagate further
                    []

                  else if not (List.member target todo) then
                    target :: todo

                  else
                    todo
                )


entropy : Dict comparable Int -> Set comparable -> Float
entropy weights possibilities =
    let
        total =
            Dict.values weights |> List.sum |> toFloat
    in
    Set.toList possibilities
        |> List.map
            (\item ->
                case Dict.get item weights of
                    Just weight ->
                        let
                            prob =
                                toFloat weight / total
                        in
                        -(prob * logBase 2 prob)

                    Nothing ->
                        0.0
            )
        |> List.sum


view : (Cell comparable -> Html msg) -> Wave comparable -> Html msg
view viewCell (Wave { items }) =
    Grid.view viewCell items
