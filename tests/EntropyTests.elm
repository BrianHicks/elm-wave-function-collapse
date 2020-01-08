module EntropyTests exposing (..)

import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)
import Entropy exposing (entropy)
import Expect exposing (FloatingPointTolerance(..))
import Fuzz
import Test exposing (..)


entropyTest : Test
entropyTest =
    describe "entropy"
        [ test "should be 1 when all possibilities are possible" <|
            \_ ->
                entropy
                    (Dict.fromList [ ( "a", 1 ), ( "b", 1 ) ])
                    (Set.fromList [ "a", "b" ])
                    |> Expect.equal 1
        , test "should be 0 when no possibilities are possible" <|
            \_ ->
                entropy
                    (Dict.fromList [ ( "a", 1 ), ( "b", 1 ) ])
                    (Set.fromList [])
                    |> Expect.equal 0
        , test "should be 0.5 when half of the possibilities are possible" <|
            \_ ->
                entropy
                    (Dict.fromList [ ( "a", 1 ), ( "b", 1 ) ])
                    (Set.fromList [ "a" ])
                    |> Expect.within (Absolute 0.00001) 0.5
        , test "entropy should reflect higher/lower weights" <|
            \_ ->
                entropy
                    (Dict.fromList [ ( "a", 2 ), ( "b", 1 ) ])
                    (Set.fromList [ "a" ])
                    |> Expect.within (Absolute 0.00001) 0.389975
        ]
