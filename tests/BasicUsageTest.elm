module BasicUsageTest exposing (suite)

import Expect
import Parser exposing (anyChar, drop, into, parse, take)
import Test exposing (Test, describe, test)


type alias T =
    { a : Char
    , b : Char
    , c : Char
    }


suite : Test
suite =
    describe "Parser -- basic usage"
        [ describe "into, take and drop"
            [ test "take one" <|
                \_ ->
                    into "T" (\x -> x)
                        |> take anyChar
                        |> parse "abc"
                        |> Expect.equal (Ok 'a')

            --
            , test "take many" <|
                \_ ->
                    into "T" T
                        |> take anyChar
                        |> take anyChar
                        |> take anyChar
                        |> parse "abc"
                        |> Expect.equal (Ok { a = 'a', b = 'b', c = 'c' })

            --
            , test "take and drop" <|
                \_ ->
                    into "T" T
                        |> drop anyChar
                        |> take anyChar
                        |> drop anyChar
                        |> take anyChar
                        |> drop anyChar
                        |> drop anyChar
                        |> take anyChar
                        |> drop anyChar
                        |> parse "abcdefgh"
                        |> Expect.equal (Ok { a = 'b', b = 'd', c = 'g' })
            ]
        ]
