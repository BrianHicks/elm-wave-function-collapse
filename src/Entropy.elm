module Entropy exposing (entropy)

import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)


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

                    out =
                        -probability * logBase 2 probability
                in
                if isNaN out then
                    0

                else
                    out
            )
        |> List.sum
