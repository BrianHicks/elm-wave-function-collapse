module Adjacency exposing (Rule, Rules, combine, emptyRules, fromImage)

import Array
import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)
import Color.Transparent exposing (Color)
import Grid exposing (Grid)
import Image exposing (Image)


type Rule
    = Rule
        { from : Color
        , to : Set Color
        , offsetRows : Int
        , offsetColumns : Int
        }


type Rules
    = Rules (Dict ( Color, Int, Int ) Rule)


emptyRules : Rules
emptyRules =
    Rules Dict.empty


fromImage : Image -> Rules
fromImage grid =
    let
        arrays =
            Grid.toArrays grid
    in
    case Grid.topLeft grid of
        Just topLeft ->
            grid
                |> Grid.indexedMap (\{ row, column } value -> ( row, column, value ))
                |> Grid.toArrays
                |> Array.foldl
                    (\row outerRules ->
                        Array.foldl
                            (\( rowNum, column, value ) rules ->
                                let
                                    rule =
                                        Rule
                                            { from = topLeft
                                            , to = Set.singleton value
                                            , offsetRows = rowNum
                                            , offsetColumns = column
                                            }

                                    revRule =
                                        Rule
                                            { from = value
                                            , to = Set.singleton topLeft
                                            , offsetRows = -rowNum
                                            , offsetColumns = -column
                                            }
                                in
                                rules
                                    |> Dict.update
                                        ( topLeft, rowNum, column )
                                        (\maybeExisting ->
                                            case maybeExisting of
                                                Nothing ->
                                                    Just rule

                                                Just existing ->
                                                    Just (combineRule rule existing)
                                        )
                                    |> Dict.update
                                        ( value, -rowNum, -column )
                                        (\maybeExisting ->
                                            case maybeExisting of
                                                Nothing ->
                                                    Just revRule

                                                Just existing ->
                                                    Just (combineRule revRule existing)
                                        )
                            )
                            outerRules
                            row
                    )
                    Dict.empty
                |> Rules

        Nothing ->
            Rules Dict.empty


{-| Combine two arbitrary rules. This only actually combines the `to` field.
Everything else will be taken from the left rule.
-}
combineRule : Rule -> Rule -> Rule
combineRule (Rule a) (Rule b) =
    Rule
        { from = a.from
        , to = Set.union a.to b.to
        , offsetColumns = a.offsetColumns
        , offsetRows = a.offsetRows
        }


combine : Rules -> Rules -> Rules
combine (Rules a) (Rules b) =
    Dict.merge
        (\leftKey leftValue result -> Dict.insert leftKey leftValue result)
        (\key (Rule leftValue) (Rule rightValue) result ->
            Dict.insert key
                (Rule
                    { from = leftValue.from
                    , to = Set.union leftValue.to rightValue.to
                    , offsetRows = leftValue.offsetRows
                    , offsetColumns = leftValue.offsetColumns
                    }
                )
                result
        )
        (\rightKey rightValue result -> Dict.insert rightKey rightValue result)
        a
        b
        Dict.empty
        |> Rules
