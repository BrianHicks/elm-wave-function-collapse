module Wave exposing (Wave, getEntropies, init, step, view)

import Adjacency exposing (Rules)
import Array exposing (Array)
import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)
import Cell exposing (Cell)
import Css
import Grid exposing (Grid)
import Heap exposing (Heap)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Image exposing (Image)
import Random exposing (Generator, Seed)


type Wave
    = Wave
        { items : Grid (Cell Image)
        , width : Int
        , height : Int
        , probabilities : Dict Image Int
        , rules : Rules
        , entropies : Heap { row : Int, column : Int, entropy : Float }
        }



-- DEBUGGING


getEntropies : Wave -> Heap { row : Int, column : Int, entropy : Float }
getEntropies (Wave guts) =
    guts.entropies



-- END DEBUGGING


init : { width : Int, height : Int } -> List Image -> Wave
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
                    List.range 0 (height - 1)
                        |> List.concatMap
                            (\row ->
                                List.map
                                    (\col ->
                                        { row = row
                                        , column = col
                                        , entropy = startingEntropy
                                        }
                                    )
                                    (List.range 0 (width - 1))
                            )
                        |> Heap.fromList (Heap.smallest |> Heap.by .entropy)
                , rules =
                    List.foldl
                        (\window rules -> Adjacency.combine rules (Adjacency.fromImage window))
                        Adjacency.emptyRules
                        windows
                , width = width
                , height = height
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


view : (Set Image -> Html msg) -> Wave -> Html msg
view viewItems (Wave { items }) =
    Grid.view
        (\cell ->
            case Cell.state cell of
                Cell.Blocked ->
                    Html.td [] [ Html.text "x" ]

                Cell.Done item ->
                    viewItems (Set.singleton item)

                Cell.Remaining remaining ->
                    Html.td [] [ viewItems remaining ]
        )
        items


step : Seed -> Wave -> ( Wave, Seed )
step seed (Wave wave) =
    case Heap.pop wave.entropies of
        Just ( { row, column }, poppedEntropies ) ->
            case Grid.get { row = row, column = column } wave.items |> Maybe.map Cell.state of
                Just (Cell.Remaining remaining) ->
                    let
                        ( chosen, newSeed ) =
                            Random.step
                                (wave.probabilities
                                    |> Dict.filter (\k _ -> Set.member k remaining)
                                    |> toWeights
                                )
                                seed

                        chosenCell =
                            Maybe.map Cell.singleton chosen

                        maybeTopLeft =
                            Maybe.andThen Grid.topLeft chosen
                    in
                    case ( maybeTopLeft, chosenCell ) of
                        ( Just topLeft, Just newValue ) ->
                            let
                                ( newItems, newEntropies ) =
                                    List.foldl
                                        (\rule ( grid, entropies ) ->
                                            let
                                                coords =
                                                    { row = row + rule.offsetRows
                                                    , column = column + rule.offsetColumns
                                                    }
                                            in
                                            if coords.row < wave.height && coords.row >= 0 && coords.column < wave.width && coords.column >= 0 then
                                                ( Grid.update
                                                    (Cell.eliminateIf
                                                        (\image ->
                                                            case Grid.topLeft image of
                                                                Just color ->
                                                                    Set.member color rule.to

                                                                Nothing ->
                                                                    True
                                                        )
                                                    )
                                                    coords
                                                    grid
                                                , Heap.push
                                                    { row = coords.row
                                                    , column = coords.column
                                                    , entropy = entropy wave.probabilities (Cell.toSet newValue)
                                                    }
                                                    entropies
                                                )

                                            else
                                                ( grid, entropies )
                                        )
                                        ( Grid.set { row = row, column = column } newValue wave.items
                                        , poppedEntropies
                                        )
                                        (Adjacency.forColor topLeft wave.rules)
                            in
                            ( Wave { wave | entropies = newEntropies, items = newItems }
                            , newSeed
                            )

                        _ ->
                            -- TODO: this should probably return an error of
                            -- some sort. It probably indicates that the
                            -- remaining possibilities were not actually a
                            -- subset of the probabilities, or something along
                            -- those lines.
                            ( Wave wave, newSeed )

                Just Cell.Blocked ->
                    ( Wave wave, seed )

                -- it's fine if the cell was already done. We have duplicate
                -- coordinates in the heap (trading off memory to avoid having
                -- to recalculate every entropy every time.)
                Just (Cell.Done _) ->
                    step seed (Wave { wave | entropies = poppedEntropies })

                -- likewise, it's fine if the stack is empty. That just means
                -- we're done!
                Nothing ->
                    ( Wave wave, seed )

        Nothing ->
            ( Wave wave, seed )


toWeights : Dict a Int -> Generator (Maybe a)
toWeights dict =
    case Dict.foldl (\k weight soFar -> ( toFloat weight, k ) :: soFar) [] dict of
        [] ->
            Random.constant Nothing

        [ ( _, only ) ] ->
            Random.map Just (Random.constant only)

        first :: rest ->
            Random.map Just (Random.weighted first rest)
