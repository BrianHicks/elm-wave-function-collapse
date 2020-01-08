module Adjacency exposing (Rule, Rules, combine, emptyRules, forColor, fromImage)

import Array
import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)
import Color.Transparent exposing (Color)
import Grid exposing (Grid)
import Image exposing (Image)


type alias Rule =
    { from : Color
    , to : Set Color
    , offsetRows : Int
    , offsetColumns : Int
    }


type Rules
    = Rules (Dict ( Color, Int, Int ) Rule)


{-| Warning: this might be in the hot path and very slow. Figure it out later
though!
-}
forColor : Color -> Rules -> List Rule
forColor color (Rules rules) =
    Dict.foldl
        (\_ rule soFar ->
            if rule.from == color then
                rule :: soFar

            else
                soFar
        )
        []
        rules


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
                                if rowNum == 0 && column == 0 then
                                    rules

                                else
                                    let
                                        rule =
                                            { from = topLeft
                                            , to = Set.singleton value
                                            , offsetRows = rowNum
                                            , offsetColumns = column
                                            }

                                        revRule =
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
combineRule a b =
    { from = a.from
    , to = Set.union a.to b.to
    , offsetColumns = a.offsetColumns
    , offsetRows = a.offsetRows
    }


combine : Rules -> Rules -> Rules
combine (Rules a) (Rules b) =
    Dict.merge
        (\leftKey leftValue result -> Dict.insert leftKey leftValue result)
        (\key leftValue rightValue result ->
            Dict.insert key
                { from = leftValue.from
                , to = Set.union leftValue.to rightValue.to
                , offsetRows = leftValue.offsetRows
                , offsetColumns = leftValue.offsetColumns
                }
                result
        )
        (\rightKey rightValue result -> Dict.insert rightKey rightValue result)
        a
        b
        Dict.empty
        |> Rules
