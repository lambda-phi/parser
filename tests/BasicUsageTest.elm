module BasicUsageTest exposing (suite)

import Expect
import Parser exposing (anyChar, grab, ignore, into, parse)
import Test exposing (Test, describe, test)


type alias T =
    { a : Char
    , b : Char
    , c : Char
    }


suite : Test
suite =
    describe "Parser -- basic usage"
        [ describe "into, grab and ignore"
            [ test "grab one" <|
                \_ ->
                    into "T" (\x -> x)
                        |> grab anyChar
                        |> parse "abc"
                        |> Expect.equal (Ok 'a')

            --
            , test "grab many" <|
                \_ ->
                    into "T" T
                        |> grab anyChar
                        |> grab anyChar
                        |> grab anyChar
                        |> parse "abc"
                        |> Expect.equal (Ok { a = 'a', b = 'b', c = 'c' })

            --
            , test "grab and ignore" <|
                \_ ->
                    into "T" T
                        |> ignore anyChar
                        |> grab anyChar
                        |> ignore anyChar
                        |> grab anyChar
                        |> ignore anyChar
                        |> ignore anyChar
                        |> grab anyChar
                        |> ignore anyChar
                        |> parse "abcdefgh"
                        |> Expect.equal (Ok { a = 'b', b = 'd', c = 'g' })
            ]
        ]
