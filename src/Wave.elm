module Wave exposing (Wave, init, step, view)

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
        , probabilities : Dict Image Int
        , rules : Rules
        , entropies : Heap { row : Int, column : Int, entropy : Float }
        }


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
                    Html.td [] [ Html.text "X" ]

                Cell.Done item ->
                    viewItems (Set.singleton item)

                Cell.Remaining remaining ->
                    Html.td [] [ viewItems remaining ]
        )
        items


step : Seed -> Wave -> ( Wave, Seed )
step seed (Wave wave) =
    case Heap.pop wave.entropies of
        Just ( { row, column }, newEntropies ) ->
            case Grid.get { row = row, column = column } wave.items |> Maybe.map Cell.state of
                Just (Cell.Remaining remaining) ->
                    let
                        ( chosen, newSeed ) =
                            Random.step
                                (wave.probabilities
                                    |> Dict.filter (\k _ -> Set.member k remaining)
                                    |> toWeights
                                    |> Random.map (Maybe.map Cell.singleton)
                                )
                                seed
                    in
                    case chosen of
                        Just newValue ->
                            ( Wave
                                { wave
                                    | entropies = newEntropies
                                    , items = Grid.set { row = row, column = column } newValue wave.items
                                }
                            , newSeed
                            )

                        Nothing ->
                            -- TODO: this should probably return an error of
                            -- some sort. It probably indicates that the
                            -- remaining possibilities were not actually a
                            -- subset of the probabilities, or something along
                            -- those lines.
                            ( Wave wave, newSeed )

                Just Cell.Blocked ->
                    ( Wave wave, seed )

                -- otherwise we just move on
                _ ->
                    ( Wave { wave | entropies = newEntropies }, seed )

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
