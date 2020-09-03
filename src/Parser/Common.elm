module Parser.Common exposing
    ( int
    , number
    )

import Parser exposing (Parser, andThen, char, concat, digit, fail, oneOf, oneOrMore, sequence, succeed, textOf, zeroOrMore, zeroOrOne)



---=== Common ===---


{-| Gets an integer value as an `Int`.

    import Parser exposing (parse)

    parse "123" int --> Ok 123

    parse "-123" int --> Ok -123

    parse "abc" int
        |> Result.mapError .message
    --> Err "Expected a digit [0-9], but got 'a' instead"

-}
int : Parser Int
int =
    concat [ zeroOrOne (char '-'), oneOrMore digit ]
        |> textOf
        |> andThen
            (\str ->
                case String.toInt str of
                    Just n ->
                        succeed n

                    Nothing ->
                        -- Unreachable
                        fail ("Failed to parse integer from '" ++ str ++ "'")
            )


{-| Gets a decimal value as a `Float`.

    import Parser exposing (parse)

    parse "12" number --> Ok 12.0

    parse "12." number --> Ok 12.0

    parse "12.34" number --> Ok 12.34

    parse "-12.34" number --> Ok -12.34

    parse ".12" number --> Ok 0.12

    parse "-.12" number --> Ok -0.12

    parse "." number
        |> Result.mapError .message
    --> Err "Expected a digit [0-9], but got '.' instead"

    parse "abc" number
        |> Result.mapError .message
    --> Err "Expected a digit [0-9], but got 'a' instead"

-}
number : Parser Float
number =
    concat
        [ zeroOrOne (oneOf [ char '-', char '+' ])
        , oneOf
            -- .12
            [ concat
                [ sequence [ char '.' ]
                , oneOrMore digit
                ]

            -- 12.34
            , concat
                [ oneOrMore digit
                , zeroOrOne (char '.')
                , zeroOrMore digit
                ]
            ]
        ]
        |> textOf
        |> andThen
            (\str ->
                case String.toFloat str of
                    Just n ->
                        succeed n

                    Nothing ->
                        -- Unreachable
                        fail ("Failed to parse number from '" ++ str ++ "'")
            )



-- TODO: date
-- TODO: time
-- TODO: datetime
---=== Programming languages ===---
-- TODO: identifier
-- TODO: intBin
-- TODO: intOct
-- TODO: intHex
-- TODO: intExp
-- TODO: numberExp
-- TODO: quotedText
-- TODO: collection
