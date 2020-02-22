module Wave exposing (Cell(..), Wave, init, load, step, view)

import Adjacency
import Array
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


type Wave a comparable
    = Wave
        { weights : Dict comparable Int
        , rules : Adjacency.Rules comparable
        , entropy : Heap Entropy
        , items : Grid (Cell comparable)
        , windows : Dict comparable (Grid a)
        }



-- END DEBUG


{-| TODO: this should maybe be init and the thing that is `init` now could be
called `custom`? Or something?
-}
load :
    { windowSize : { width : Int, height : Int }
    , waveSize : { width : Int, height : Int }
    , hash : Grid a -> comparable
    }
    -> Grid a
    -> Wave a comparable
load options grid =
    let
        windows : Grid (Grid a)
        windows =
            Grid.windows options.windowSize grid

        withIndex : Grid ( comparable, Grid a )
        withIndex =
            Grid.map
                (\window ->
                    ( options.hash window
                    , window
                    )
                )
                windows

        indexes : Dict comparable (Grid a)
        indexes =
            -- wow there is a lot of conversion happening here. Probably should
            -- come back and make it more efficient sometime.
            withIndex
                |> Grid.toArrays
                |> Array.foldl Array.append Array.empty
                |> Array.toList
                |> Dict.fromList

        weights : Dict comparable Int
        weights =
            -- wow there is a lot of conversion happening here. Probably should
            -- come back and make it more efficient sometime.
            withIndex
                |> Grid.toArrays
                |> Array.foldl Array.append Array.empty
                |> Array.foldl
                    (\( id, _ ) dict ->
                        Dict.update id
                            (\maybeCount ->
                                case maybeCount of
                                    Just count ->
                                        Just (count + 1)

                                    Nothing ->
                                        Just 1
                            )
                            dict
                    )
                    Dict.empty

        rules : Adjacency.Rules comparable
        rules =
            withIndex
                |> Grid.map Tuple.first
                |> Adjacency.fromIds
                |> Adjacency.finalize
    in
    init indexes rules weights options.waveSize


init : Dict comparable (Grid a) -> Adjacency.Rules comparable -> Dict comparable Int -> { width : Int, height : Int } -> Wave a comparable
init windows rules weights dimensions =
    let
        initialCell =
            Open (Set.fromList (Dict.keys weights))

        initialEntropy =
            entropy weights (Set.fromList (Dict.keys weights))
    in
    Wave
        { windows = windows
        , weights = weights
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


step : Random.Seed -> Wave a comparable -> ( Wave a comparable, Random.Seed )
step seed (Wave wave) =
    case Heap.pop wave.entropy of
        Just ( { coords }, poppedEntropy ) ->
            collapse seed coords (Wave { wave | entropy = poppedEntropy })

        Nothing ->
            -- quittin' time!
            ( Wave wave, seed )


{-| Step one!
-}
collapse : Random.Seed -> { row : Int, column : Int } -> Wave a comparable -> ( Wave a comparable, Random.Seed )
collapse seed coords (Wave wave) =
    case Grid.get coords wave.items of
        Just (Open remaining) ->
            let
                generator =
                    remaining
                        |> Set.foldl
                            (\current acc ->
                                case Dict.get current wave.weights of
                                    Just weight ->
                                        ( toFloat weight, current ) :: acc

                                    Nothing ->
                                        acc
                            )
                            []
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
    -> Wave a comparable
    -> Wave a comparable
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
    -> ( Wave a comparable, List { row : Int, column : Int } )
    -> ( Wave a comparable, List { row : Int, column : Int } )
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


view : (Dict comparable (Grid a) -> Cell comparable -> Html msg) -> Wave a comparable -> Html msg
view viewCell (Wave { items, windows }) =
    Grid.view (viewCell windows) items
