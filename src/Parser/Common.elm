module Parser.Common exposing (int, number)

{-| Common and other useful parsers.


# Common

@docs int, number

-}

import Parser exposing (Parser, andThen, char, concat, digit, expected, oneOf, oneOrMore, sequence, succeed, textOf, zeroOrMore, zeroOrOne)



-- COMMON


{-| Gets an integer value as an `Int`.

    import Parser exposing (parse)
    import Parser.Error

    -- You can parse integers as `Int` instead of `String`.
    parse "123" int --> Ok 123

    -- It also works with negative numbers.
    parse "-123" int --> Ok -123

    -- But not with invalid numbers.
    parse "abc" int
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting a digit [0-9].\nI got stuck when I got the character 'a'."

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
                        expected ("an integer from '" ++ str ++ "'")
            )


{-| Gets a decimal value as a `Float`.

    import Parser exposing (parse)
    import Parser.Error

    -- You can parse numbers as `Float` instead of `String`.
    parse "12" number --> Ok 12.0
    parse "12." number --> Ok 12.0
    parse "12.34" number --> Ok 12.34
    parse ".12" number --> Ok 0.12

    -- It also works with negative numbers.
    parse "-12.34" number --> Ok -12.34
    parse "-.12" number --> Ok -0.12

    -- But not with invalid numbers.
    parse "." number
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting a digit [0-9].\nI got stuck when I got the character '.'."

    parse "abc" number
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting a digit [0-9].\nI got stuck when I got the character 'a'."

-}
number : Parser Float
number =
    concat
        [ zeroOrOne (oneOf [ char '-', char '+' ])
        , oneOf
            [ concat
                [ sequence [ char '.' ]
                , oneOrMore digit
                ]
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
                        expected ("Failed to parse number from '" ++ str ++ "'")
            )



-- TODO: date
-- TODO: time
-- TODO: datetime
-- TODO: email
-- TODO: unixPath
-- TODO: windowsPath
-- TODO: uri
-- TODO: IPv4
-- TODO: IPv6
-- PROGRAMMING LANGUAGES
-- TODO: identifier
-- TODO: intBin
-- TODO: intOct
-- TODO: intHex
-- TODO: intExp
-- TODO: numberExp
-- TODO: quotedText
-- TODO: collection
