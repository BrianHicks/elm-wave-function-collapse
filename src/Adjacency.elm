module Adjacency exposing (Rule, Rules, combine, fromGrid)

import Array
import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)
import Grid exposing (Grid)


type Rule a
    = Rule
        { from : a
        , to : Set a
        , offsetRows : Int
        , offsetColumns : Int
        }


type Rules a
    = Rules (Dict ( a, Int, Int ) (Rule a))


fromGrid : Grid a -> Rules a
fromGrid grid =
    let
        arrays =
            Grid.toArrays grid
    in
    case Grid.get { row = 0, column = 0 } grid of
        Just topLeft ->
            grid
                |> Grid.indexedMap
                    (\{ row, column } value ->
                        ( row
                        , column
                        , Rule
                            { from = topLeft
                            , to = Set.singleton value
                            , offsetRows = row
                            , offsetColumns = column
                            }
                        )
                    )
                |> Grid.toArrays
                |> Array.foldl
                    (\row outerRules ->
                        Array.foldl
                            (\( rowNum, column, (Rule guts) as rule ) rules ->
                                if rowNum == 0 && column == 0 then
                                    rules

                                else
                                    Dict.insert
                                        ( guts.from, guts.offsetRows, guts.offsetColumns )
                                        rule
                                        rules
                            )
                            outerRules
                            row
                    )
                    Dict.empty
                |> Rules

        Nothing ->
            Rules Dict.empty


combine : Rules a -> Rules a -> Rules a
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
