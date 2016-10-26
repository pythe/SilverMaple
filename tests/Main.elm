port module Main exposing (..)

import Json.Encode exposing (Value)
import Test.Runner.Node exposing (run)
import Test exposing (..)
import Expect
import SilverMaple


main : Program Value
main =
    run emit suite


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
    --(Basics.toFloat <| Basics.round <| sum / (Basics.toFloat <| List.length charScores) * 1000) / 1000
    (List.length charScores
        |> Basics.toFloat
        |> (/) (List.sum charScores)
        |> (*) 1000
        |> Basics.round
        |> Basics.toFloat
    )
        / 1000


noMatch : Float
noMatch =
    0.0


match : Float
match =
    1.0


trailing : Float
trailing =
    0.8


specialTrailing : Float
specialTrailing =
    0.9


buffer : Float
buffer =
    0.85
