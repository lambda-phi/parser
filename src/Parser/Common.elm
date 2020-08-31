module Parser.Common exposing
    ( int
    , number
    )

import Parser exposing (Parser, andThen, digit, fail, map2, mapList, oneOf, oneOrMore, succeed, text, toString, zeroOrMore)



-- INT


int : Parser Int
int =
    map2 (++)
        (oneOf [ text "-", text "" ])
        (toString (oneOrMore digit))
        |> andThen
            (\str ->
                case String.toInt str of
                    Just n ->
                        succeed n

                    Nothing ->
                        fail ("failed to parse integer from '" ++ str ++ "'")
            )



-- INT BIN
-- INT OCT
-- INT HEX
-- INT EXP
-- NUMBER


number : Parser Float
number =
    mapList (String.join "")
        [ oneOf [ text "-", text "" ]
        , toString (oneOrMore digit)
        , text "."
        , toString (zeroOrMore digit)
        ]
        |> andThen
            (\str ->
                case String.toFloat str of
                    Just n ->
                        succeed n

                    Nothing ->
                        fail ("failed to parse number from '" ++ str ++ "'")
            )



-- NUMBER EXP
-- DATE
-- TIME
-- DATETIME
