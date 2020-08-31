module ErrorReportingTest exposing (suite)

import Expect
import Parser exposing (anyChar, drop, into, parse, take)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Parser -- error reporting"
        [ describe "positions"
            [ test "advance columns" <|
                \_ ->
                    into "T" (\x -> x)
                        |> take anyChar
                        |> parse "a"
                        |> Expect.equal (Ok 'a')
            ]

        -- context stack
        ]
