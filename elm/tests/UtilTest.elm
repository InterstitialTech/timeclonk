module UtilTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser as P
import Test exposing (..)
import Time
import Util


suite : Test
suite =
    describe "The Util module"
        [ describe "Util.parseTime"
            -- Nest as many descriptions as you like.
            [ test "parse int" <|
                \_ ->
                    let
                        mkay =
                            P.run Util.leadingZeroInt "5"
                    in
                    Expect.true "tried to parse date" (mkay |> Result.map (\_ -> True) |> Result.withDefault False)
            , test "parse int leading zero" <|
                \_ ->
                    let
                        mkay =
                            P.run Util.leadingZeroInt "005"
                    in
                    Expect.equal (Ok 5) mkay
            , test "regular datetime" <|
                \_ ->
                    let
                        dt =
                            Util.parseTime Time.utc "2021/12/5 7:37:12"
                    in
                    Expect.true "tried to parse date" (dt |> Result.map (\_ -> True) |> Result.withDefault False)
            , test "dashes" <|
                \_ ->
                    let
                        dt =
                            Util.parseTime Time.utc "2021-9-8 14:39:09"
                    in
                    Expect.true "tried to parse date" (dt |> Result.map (\_ -> True) |> Result.withDefault False)
            , test "timekeeper datetime w dashes" <|
                \_ ->
                    let
                        dt =
                            Util.parseTime Time.utc "2021-08-25 17:48:08"
                    in
                    Expect.true "tried to parse date" (dt |> Result.map (\_ -> True) |> Result.withDefault False)
            , fuzz int "date equal to itself" <|
                \rdt ->
                    let
                        dt =
                            abs rdt
                    in
                    Util.showTime Time.utc (Time.millisToPosix dt)
                        |> Util.parseTime Time.utc
                        |> (\r ->
                                Expect.equal
                                    (Ok (Just (dt // 1000)))
                                    (Result.map (Maybe.map (Time.posixToMillis >> (\x -> x // 1000))) r)
                           )
            ]
        ]
