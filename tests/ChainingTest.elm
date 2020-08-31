module ChainingTest exposing (suite)

import Expect
import Parser exposing (andThen, fail, orElse, parse, succeed, withError)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Parser -- chaining"
        -- andThen
        [ describe "andThen"
            [ test "andThen another parser" <|
                \_ ->
                    succeed 'a'
                        |> andThen (\c -> succeed (Char.toUpper c))
                        |> parse ""
                        |> Expect.equal (Ok 'A')
            ]

        -- orElse
        , describe "orElse"
            [ test "orElse after succeed" <|
                \_ ->
                    succeed 'a'
                        |> orElse (succeed '_')
                        |> parse ""
                        |> Expect.equal (Ok 'a')

            --
            , test "orElse after fail" <|
                \_ ->
                    fail "error"
                        |> orElse (succeed '_')
                        |> parse ""
                        |> Expect.equal (Ok '_')
            ]

        -- withError
        , describe "withError"
            [ test "withError after succeed" <|
                \_ ->
                    succeed 'a'
                        |> withError "new error"
                        |> parse ""
                        |> Expect.equal (Ok 'a')

            --
            , test "withError after fail" <|
                \_ ->
                    fail "first error"
                        |> withError "new error"
                        |> parse ""
                        |> Result.mapError .message
                        |> Expect.equal (Err "new error")
            ]
        ]
