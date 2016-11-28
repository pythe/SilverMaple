port module Main exposing (..)

import Json.Encode exposing (Value)
import Test.Runner.Node exposing (run)
import Test exposing (..)
import Expect
import SilverMaple
import Scores exposing (..)


main : Program Value
main =
    run emit suite



{-
   Ideas for property tests:

   Identical strings always return 1.0
   Totally dissimilar strings always return 0.0
   A query longer than the string returns 0.0


-}


suite : Test
suite =
    describe "SilverMaple"
        [ describe "SilverMaple.score"
            [ test "scores empty case like LiquidMetal" <|
                \() ->
                    SilverMaple.score "" ""
                        |> Expect.equal trailing
            , test "scores like empty text LiquidMetal" <|
                \() ->
                    SilverMaple.score "" "a"
                        |> Expect.equal noMatch
            , test "scores empty query like LiquidMetal" <|
                \() ->
                    SilverMaple.score "a" ""
                        |> Expect.equal trailing
            , test "scores miss like LiquidMetal" <|
                \() ->
                    SilverMaple.score "a" "tooLong"
                        |> Expect.equal noMatch
            , test "scores short exact match like LiquidMetal" <|
                \() ->
                    SilverMaple.score "a" "a"
                        |> Expect.equal match
            , test "scores short miss like LiquidMetal" <|
                \() ->
                    SilverMaple.score "a" "b"
                        |> Expect.equal noMatch
            , test "scores like LiquidMetal" <|
                \() ->
                    SilverMaple.score "abc" ""
                        |> Expect.equal
                            (expectedScore
                                [ trailing, trailing, trailing ]
                            )
            , test "scores case-insensitively like LiquidMetal" <|
                \() ->
                    SilverMaple.score "A" "a"
                        |> Expect.equal
                            (expectedScore
                                [ match ]
                            )
            , test "scores mid-word like LiquidMetal" <|
                \() ->
                    SilverMaple.score "Foo Bar" "ooar"
                        |> Expect.equal
                            (expectedScore
                                [ noMatch
                                , match
                                , match
                                , noMatch
                                , match
                                , match
                                ]
                            )
            , test "scores interesting case like LiquidMetal" <|
                \() ->
                    SilverMaple.score "gnu's Not Unix" "nu"
                        |> Expect.equal
                            (expectedScore
                                [ buffer
                                , buffer
                                , buffer
                                , buffer
                                , buffer
                                , match
                                , match
                                , buffer
                                , buffer
                                , match
                                , match
                                , trailing
                                , trailing
                                , trailing
                                ]
                            )
            ]
        ]


port emit : ( String, Value ) -> Cmd msg


expectedScore : List Float -> Float
expectedScore charScores =
    List.length charScores
        |> Basics.toFloat
        |> (/) (List.sum charScores)
        |> (*) 1000
        |> Basics.round
        |> Basics.toFloat
        |> (/) 1000
