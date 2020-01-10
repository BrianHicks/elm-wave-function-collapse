module Adjacency exposing (DraftRules, Rule, Rules, combineRules, finalize, fromIds)

{-| The draft/final rules distinction is no longer necessary and should really
be cleaned up.
-}

import Array
import Dict exposing (Dict)
import Direction exposing (Direction)
import Grid exposing (Grid)
import Set exposing (Set)


type alias Rule comparable =
    { direction : Direction
    , to : Set comparable
    }


type alias Rules comparable =
    Dict ( comparable, Direction ) (Set comparable)


finalize : DraftRules comparable -> Rules comparable
finalize (DraftRules draft) =
    draft


combineRules : List (Rule comparable) -> List (Rule comparable)
combineRules original =
    original
        |> List.foldl
            (\rule ->
                Dict.update rule.direction
                    (\maybeRule ->
                        case maybeRule of
                            Just existing ->
                                Just { existing | to = Set.union existing.to rule.to }

                            Nothing ->
                                Just rule
                    )
            )
            Dict.empty
        |> Dict.values



-- Draft Rules (they should be combinable, eventually!)


type DraftRules comparable
    = DraftRules (Dict ( comparable, Direction ) (Set comparable))


fromIds : Grid comparable -> DraftRules comparable
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
                            [ Grid.getWrapping { row = rowNum, column = colNum - 1 } grid
                                |> Maybe.map (\dest -> ( id, Direction.left, dest ))
                            , Grid.getWrapping { row = rowNum, column = colNum + 1 } grid
                                |> Maybe.map (\dest -> ( id, Direction.right, dest ))
                            , Grid.getWrapping { row = rowNum - 1, column = colNum } grid
                                |> Maybe.map (\dest -> ( id, Direction.up, dest ))
                            , Grid.getWrapping { row = rowNum + 1, column = colNum } grid
                                |> Maybe.map (\dest -> ( id, Direction.down, dest ))
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
