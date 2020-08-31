module CommonTest exposing (suite)

import Expect
import Parser exposing (parse)
import Parser.Common exposing (int, number)
import Result
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Parser -- combinations"
        -- int
        [ describe "int"
            [ test "int match positive" <|
                \_ ->
                    int
                        |> parse "123abc"
                        |> Expect.equal (Ok 123)

            --
            , test "int match negative" <|
                \_ ->
                    int
                        |> parse "-123abc"
                        |> Expect.equal (Ok -123)

            --
            , test "int not match" <|
                \_ ->
                    int
                        |> parse "abc123"
                        |> Result.toMaybe
                        |> Expect.equal Nothing
            ]

        -- number
        , describe "number"
            [ test "number match positive" <|
                \_ ->
                    number
                        |> parse "12.34abc"
                        |> Expect.equal (Ok 12.34)

            --
            , test "number match negative" <|
                \_ ->
                    number
                        |> parse "-12.34abc"
                        |> Expect.equal (Ok -12.34)

            --
            , test "number match integer" <|
                \_ ->
                    number
                        |> parse "1234abc"
                        |> Expect.equal (Ok 1234)

            --
            , test "number not match" <|
                \_ ->
                    number
                        |> parse "abc12.34"
                        |> Result.toMaybe
                        |> Expect.equal Nothing
            ]
        ]
