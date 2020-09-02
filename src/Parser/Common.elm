module Parser.Common exposing
    ( int
    , number
    , word
    )

import Parser exposing (Parser, andThen, char, concat, digit, fail, letter, oneOf, oneOrMore, succeed, textOf, zeroOrMore, zeroOrOne)



---=== Common ===---


{-| Gets an integer value.

    import Parser exposing (parse)

    parse "123" int --> Ok 123

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
                        fail ("failed to parse integer from '" ++ str ++ "'")
            )



-- number


number : Parser Float
number =
    concat
        [ zeroOrOne (oneOf [ char '-', char '+' ])
        , oneOrMore digit
        , zeroOrOne (char '.')
        , zeroOrMore digit
        ]
        |> textOf
        |> andThen
            (\str ->
                case String.toFloat str of
                    Just n ->
                        succeed n

                    Nothing ->
                        fail ("failed to parse number from '" ++ str ++ "'")
            )



-- word


word : Parser String
word =
    concat
        [ oneOrMore letter
        ]
        |> textOf



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
