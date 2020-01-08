module Entropy exposing (entropy)

import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)


entropy : Dict a Int -> Set a -> Float
entropy probabilities possibilities =
    possibilities
        |> Set.toList
        |> List.map
            (\item ->
                probabilities
                    |> Dict.get item
                    |> Maybe.withDefault 0
                    |> toFloat
            )
        |> List.sum
