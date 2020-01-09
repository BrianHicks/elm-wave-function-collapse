module Adjacency exposing (Direction(..), DraftRules, Rule, Rules, finalize, fromIds)

import Array
import Dict exposing (Dict)
import Grid exposing (Grid)
import Set exposing (Set)


type Direction
    = Up
    | Down
    | Left
    | Right


directionToComparable : Direction -> Int
directionToComparable d =
    case d of
        Up ->
            0

        Down ->
            1

        Left ->
            2

        Right ->
            3


directionFromComparable : Int -> Direction
directionFromComparable c =
    case c of
        0 ->
            Up

        1 ->
            Down

        2 ->
            Left

        _ ->
            Right


type alias Rule =
    { direction : Direction
    , to : Set Int
    }


type alias Rules =
    Dict Int (List Rule)


finalize : DraftRules -> Rules
finalize (DraftRules draft) =
    Dict.foldl
        (\( id, direction ) values dict ->
            let
                rule =
                    { direction = directionFromComparable direction
                    , to = values
                    }
            in
            Dict.update id
                (\maybeRules ->
                    Just <|
                        case maybeRules of
                            Just rules ->
                                rule :: rules

                            Nothing ->
                                [ rule ]
                )
                dict
        )
        Dict.empty
        draft



-- Draft Rules (they should be combinable, eventually!)


type DraftRules
    = DraftRules (Dict ( Int, Int ) (Set Int))


fromIds : Grid Int -> DraftRules
fromIds grid =
    let
        rows =
            Grid.toArrays grid
    in
    rows
        |> Array.indexedMap
            (\rowNum row ->
                Array.indexedMap
                    (\colNum id ->
                        List.filterMap identity
                            [ Grid.get { row = rowNum, column = colNum - 1 } grid
                                |> Maybe.map (\dest -> ( id, directionToComparable Left, dest ))
                            , Grid.get { row = rowNum, column = colNum + 1 } grid
                                |> Maybe.map (\dest -> ( id, directionToComparable Right, dest ))
                            , Grid.get { row = rowNum - 1, column = colNum } grid
                                |> Maybe.map (\dest -> ( id, directionToComparable Up, dest ))
                            , Grid.get { row = rowNum + 1, column = colNum } grid
                                |> Maybe.map (\dest -> ( id, directionToComparable Down, dest ))
                            ]
                    )
                    row
            )
        |> Array.foldl
            (\row dictFromRow ->
                Array.foldl
                    (\rules dictFromRules ->
                        List.foldl
                            (\( from, direction, to ) dict ->
                                Dict.update ( from, direction )
                                    (\currentValue ->
                                        Just <|
                                            case currentValue of
                                                Just set ->
                                                    Set.insert to set

                                                Nothing ->
                                                    Set.singleton to
                                    )
                                    dict
                            )
                            dictFromRules
                            rules
                    )
                    dictFromRow
                    row
            )
            Dict.empty
        |> DraftRules
