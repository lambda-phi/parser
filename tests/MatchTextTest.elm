module MatchTextTest exposing (suite)

import Expect
import Parser exposing (alphanumeric, anyChar, blank, char, charNoCase, digit, end, letter, lowercase, parse, uppercase)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Parser -- match text"
        -- anyChar
        [ describe "anyChar"
            [ test "anyChar on empty text" <|
                \_ ->
                    anyChar
                        |> parse ""
                        |> Result.toMaybe
                        |> Expect.equal Nothing

            --
            , test "anyChar on text" <|
                \_ ->
                    anyChar
                        |> parse "abc"
                        |> Expect.equal (Ok 'a')
            ]

        -- char
        , describe "char"
            [ test "char match" <|
                \_ ->
                    char 'a'
                        |> parse "abc"
                        |> Expect.equal (Ok 'a')

            --
            , test "char not match" <|
                \_ ->
                    char 'a'
                        |> parse "_bc"
                        |> Result.toMaybe
                        |> Expect.equal Nothing
            ]

        -- charNoCase
        , describe "charNoCase"
            [ test "charNoCase match lower" <|
                \_ ->
                    charNoCase 'a'
                        |> parse "abc"
                        |> Expect.equal (Ok 'a')

            --
            , test "charNoCase match upper" <|
                \_ ->
                    charNoCase 'a'
                        |> parse "Abc"
                        |> Expect.equal (Ok 'A')

            --
            , test "charNoCase not match" <|
                \_ ->
                    char 'a'
                        |> parse "_bc"
                        |> Result.toMaybe
                        |> Expect.equal Nothing
            ]

        -- character classes
        , describe "character classes"
            -- digit
            [ test "digit match" <|
                \_ ->
                    digit
                        |> parse "2"
                        |> Expect.equal (Ok '2')

            --
            , test "digit not match" <|
                \_ ->
                    digit
                        |> parse "a"
                        |> Result.toMaybe
                        |> Expect.equal Nothing

            -- letter
            , test "letter match lower" <|
                \_ ->
                    letter
                        |> parse "a"
                        |> Expect.equal (Ok 'a')

            --
            , test "letter match upper" <|
                \_ ->
                    letter
                        |> parse "A"
                        |> Expect.equal (Ok 'A')

            --
            , test "letter not match" <|
                \_ ->
                    letter
                        |> parse "2"
                        |> Result.toMaybe
                        |> Expect.equal Nothing

            -- lowercase
            , test "lowercase match lower" <|
                \_ ->
                    lowercase
                        |> parse "a"
                        |> Expect.equal (Ok 'a')

            --
            , test "lowercase match upper" <|
                \_ ->
                    lowercase
                        |> parse "A"
                        |> Result.toMaybe
                        |> Expect.equal Nothing

            -- uppercase
            , test "uppercase match lower" <|
                \_ ->
                    uppercase
                        |> parse "a"
                        |> Result.toMaybe
                        |> Expect.equal Nothing

            --
            , test "uppercase match upper" <|
                \_ ->
                    uppercase
                        |> parse "A"
                        |> Expect.equal (Ok 'A')

            -- alphanumeric
            , test "alphanumeric match lower" <|
                \_ ->
                    alphanumeric
                        |> parse "a"
                        |> Expect.equal (Ok 'a')

            --
            , test "alphanumeric match upper" <|
                \_ ->
                    alphanumeric
                        |> parse "A"
                        |> Expect.equal (Ok 'A')

            --
            , test "alphanumeric match digit" <|
                \_ ->
                    alphanumeric
                        |> parse "2"
                        |> Expect.equal (Ok '2')

            --
            , test "alphanumeric not match" <|
                \_ ->
                    alphanumeric
                        |> parse "_"
                        |> Result.toMaybe
                        |> Expect.equal Nothing

            -- blank
            , test "blank match space" <|
                \_ ->
                    blank
                        |> parse " "
                        |> Expect.equal (Ok ' ')

            --
            , test "blank match tab" <|
                \_ ->
                    blank
                        |> parse "\t"
                        |> Expect.equal (Ok '\t')

            --
            , test "blank match newline" <|
                \_ ->
                    blank
                        |> parse "\n"
                        |> Expect.equal (Ok '\n')

            --
            , test "blank match return carriage" <|
                \_ ->
                    blank
                        |> parse "\u{000D}"
                        |> Expect.equal (Ok '\u{000D}')

            --
            , test "blank match form feed" <|
                \_ ->
                    blank
                        |> parse "\u{000C}"
                        |> Expect.equal (Ok '\u{000C}')

            --
            , test "blank match vertical tab" <|
                \_ ->
                    blank
                        |> parse "\u{000B}"
                        |> Expect.equal (Ok '\u{000B}')

            --
            , test "blank not match" <|
                \_ ->
                    blank
                        |> parse "a"
                        |> Result.toMaybe
                        |> Expect.equal Nothing
            ]

        -- end
        , describe "end"
            [ test "end on empty text" <|
                \_ ->
                    end
                        |> parse ""
                        |> Expect.equal (Ok ())

            --
            , test "end on non-empty text" <|
                \_ ->
                    end
                        |> parse "abc"
                        |> Result.toMaybe
                        |> Expect.equal Nothing
            ]
        ]
