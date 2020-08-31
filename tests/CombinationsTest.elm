module CombinationsTest exposing (suite)

import Expect
import Parser exposing (atLeast, atMost, between, char, maybe, oneOf, oneOrMore, parse, repeat, sequence, succeed, text, textNoCase, toString, until, while, zeroOrMore)
import Result
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Parser -- combinations"
        -- text
        [ describe "text"
            [ test "text match" <|
                \_ ->
                    text "ab"
                        |> parse "abc"
                        |> Expect.equal (Ok "ab")

            --
            , test "text not match" <|
                \_ ->
                    text "ab"
                        |> parse "a"
                        |> Result.toMaybe
                        |> Expect.equal Nothing
            ]

        -- textNoCase
        , describe "textNoCase"
            [ test "textNoCase match" <|
                \_ ->
                    textNoCase "ab"
                        |> parse "Abc"
                        |> Expect.equal (Ok "Ab")

            --
            , test "textNoCase not match" <|
                \_ ->
                    textNoCase "ab"
                        |> parse "a"
                        |> Result.toMaybe
                        |> Expect.equal Nothing
            ]

        -- sequence
        , describe "sequence"
            [ test "sequence empty" <|
                \_ ->
                    sequence []
                        |> parse ""
                        |> Expect.equal (Ok [])

            --
            , test "sequence match" <|
                \_ ->
                    sequence [ char 'a', char 'b', char 'c' ]
                        |> parse "abc"
                        |> Expect.equal (Ok [ 'a', 'b', 'c' ])

            --
            , test "sequence not match" <|
                \_ ->
                    sequence [ char 'a', char 'b', char 'c' ]
                        |> parse "a_c"
                        |> Result.toMaybe
                        |> Expect.equal Nothing
            ]

        -- toString
        , describe "toString"
            [ test "toString empty" <|
                \_ ->
                    toString (succeed [])
                        |> parse ""
                        |> Expect.equal (Ok "")

            --
            , test "toString match" <|
                \_ ->
                    toString (sequence [ char 'a', char 'b', char 'c' ])
                        |> parse "abc"
                        |> Expect.equal (Ok "abc")

            --
            , test "toString not match" <|
                \_ ->
                    toString (sequence [ char 'a', char 'b', char 'c' ])
                        |> parse "a_c"
                        |> Result.toMaybe
                        |> Expect.equal Nothing
            ]

        -- oneOf
        , describe "oneOf"
            [ test "oneOf match" <|
                \_ ->
                    oneOf [ char 'a', char 'b', char 'c' ]
                        |> parse "c"
                        |> Expect.equal (Ok 'c')

            --
            , test "oneOf not match" <|
                \_ ->
                    oneOf [ char 'a', char 'b', char 'c' ]
                        |> parse "_"
                        |> Result.toMaybe
                        |> Expect.equal Nothing
            ]

        -- maybe
        , describe "maybe"
            [ test "maybe match" <|
                \_ ->
                    maybe (char 'a')
                        |> parse "a"
                        |> Expect.equal (Ok (Just 'a'))

            --
            , test "maybe not match" <|
                \_ ->
                    maybe (char 'a')
                        |> parse ""
                        |> Expect.equal (Ok Nothing)
            ]

        -- zeroOrMore
        , describe "zeroOrMore"
            [ test "zeroOrMore match zero" <|
                \_ ->
                    zeroOrMore (char 'a')
                        |> parse ""
                        |> Expect.equal (Ok [])

            --
            , test "zeroOrMore match one" <|
                \_ ->
                    zeroOrMore (char 'a')
                        |> parse "a"
                        |> Expect.equal (Ok [ 'a' ])

            --
            , test "zeroOrMore match many" <|
                \_ ->
                    zeroOrMore (char 'a')
                        |> parse "aaa"
                        |> Expect.equal (Ok [ 'a', 'a', 'a' ])
            ]

        -- oneOrMore
        , describe "oneOrMore"
            [ test "oneOrMore match zero" <|
                \_ ->
                    oneOrMore (char 'a')
                        |> parse ""
                        |> Result.toMaybe
                        |> Expect.equal Nothing

            --
            , test "oneOrMore match one" <|
                \_ ->
                    oneOrMore (char 'a')
                        |> parse "a"
                        |> Expect.equal (Ok [ 'a' ])

            --
            , test "oneOrMore match many" <|
                \_ ->
                    oneOrMore (char 'a')
                        |> parse "aaa"
                        |> Expect.equal (Ok [ 'a', 'a', 'a' ])
            ]

        -- repeat
        , describe "repeat"
            [ test "repeat less" <|
                \_ ->
                    repeat 3 (char 'a')
                        |> parse ""
                        |> Result.toMaybe
                        |> Expect.equal Nothing

            --
            , test "repeat exactly" <|
                \_ ->
                    repeat 3 (char 'a')
                        |> parse "aaa"
                        |> Expect.equal (Ok [ 'a', 'a', 'a' ])

            --
            , test "repeat more" <|
                \_ ->
                    repeat 3 (char 'a')
                        |> parse "aaaaa"
                        |> Expect.equal (Ok [ 'a', 'a', 'a' ])
            ]

        -- atLeast
        , describe "atLeast"
            [ test "atLeast less" <|
                \_ ->
                    atLeast 3 (char 'a')
                        |> parse ""
                        |> Result.toMaybe
                        |> Expect.equal Nothing

            --
            , test "atLeast exactly" <|
                \_ ->
                    atLeast 3 (char 'a')
                        |> parse "aaa"
                        |> Expect.equal (Ok [ 'a', 'a', 'a' ])

            --
            , test "atLeast more" <|
                \_ ->
                    atLeast 3 (char 'a')
                        |> parse "aaaaa"
                        |> Expect.equal (Ok [ 'a', 'a', 'a', 'a', 'a' ])
            ]

        -- atMost
        , describe "atMost"
            [ test "atMost less" <|
                \_ ->
                    atMost 3 (char 'a')
                        |> parse ""
                        |> Expect.equal (Ok [])

            --
            , test "atMost exactly" <|
                \_ ->
                    atMost 3 (char 'a')
                        |> parse "aaa"
                        |> Expect.equal (Ok [ 'a', 'a', 'a' ])

            --
            , test "atMost more" <|
                \_ ->
                    atMost 3 (char 'a')
                        |> parse "aaaaa"
                        |> Expect.equal (Ok [ 'a', 'a', 'a' ])
            ]

        -- between
        , describe "between"
            [ test "between less" <|
                \_ ->
                    between 2 4 (char 'a')
                        |> parse ""
                        |> Result.toMaybe
                        |> Expect.equal Nothing

            --
            , test "between min" <|
                \_ ->
                    between 2 4 (char 'a')
                        |> parse "aa"
                        |> Expect.equal (Ok [ 'a', 'a' ])

            --
            , test "between max" <|
                \_ ->
                    between 2 4 (char 'a')
                        |> parse "aaaa"
                        |> Expect.equal (Ok [ 'a', 'a', 'a', 'a' ])

            --
            , test "between more" <|
                \_ ->
                    between 2 4 (char 'a')
                        |> parse "aaaaaa"
                        |> Expect.equal (Ok [ 'a', 'a', 'a', 'a' ])
            ]

        -- until
        , describe "until"
            [ test "until on empty" <|
                \_ ->
                    until (char '_') (char 'a')
                        |> parse ""
                        |> Expect.equal (Ok [])

            --
            , test "until match zero" <|
                \_ ->
                    until (char '_') (char 'a')
                        |> parse "_a"
                        |> Expect.equal (Ok [])

            --
            , test "until match one" <|
                \_ ->
                    until (char '_') (char 'a')
                        |> parse "a_a"
                        |> Expect.equal (Ok [ 'a' ])

            --
            , test "until match many" <|
                \_ ->
                    until (char '_') (char 'a')
                        |> parse "aaa_a"
                        |> Expect.equal (Ok [ 'a', 'a', 'a' ])
            ]

        -- while
        , describe "while"
            [ test "while on empty" <|
                \_ ->
                    while (\_ x -> x /= '_') (char 'a')
                        |> parse ""
                        |> Expect.equal (Ok [])

            --
            , test "while match zero" <|
                \_ ->
                    while (\_ x -> x /= '_') (char 'a')
                        |> parse "_a"
                        |> Expect.equal (Ok [])

            --
            , test "while match one" <|
                \_ ->
                    while (\_ x -> x /= '_') (char 'a')
                        |> parse "a_a"
                        |> Expect.equal (Ok [ 'a' ])

            --
            , test "while match many" <|
                \_ ->
                    while (\_ x -> x /= '_') (char 'a')
                        |> parse "aaa_a"
                        |> Expect.equal (Ok [ 'a', 'a', 'a' ])

            --
            , test "while match many checking previous matches" <|
                \_ ->
                    while (\xs _ -> List.length xs < 3) (char 'a')
                        |> parse "aaaaa"
                        |> Expect.equal (Ok [ 'a', 'a', 'a' ])
            ]
        ]
