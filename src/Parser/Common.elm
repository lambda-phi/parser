module Parser.Common exposing (int, number, text, textNoCase, digits, letters, spaces, line, token)

{-| Common and other useful parsers.

@docs int, number, text, textNoCase, digits, letters, spaces, line, token

-}

import Parser exposing (Parser, andThen, andThenIgnore, drop, expected, expecting, map, oneOf, succeed, take, textOf)
import Parser.Char exposing (anyChar, char, charNoCase, digit, letter, space)
import Parser.Check exposing (endOfLine, notFollowedBy)
import Parser.Sequence exposing (concat, exactly, oneOrMore, sequence, until, zeroOrMore, zeroOrOne)


{-| Matches an integer value as an `Int`.

    import Parser exposing (parse)
    import Parser.Error

    -- You can parse integers as `Int` instead of `String`.
    parse "123" int --> Ok 123

    -- It also works with negative numbers.
    parse "-123" int --> Ok -123

    -- A decimal number is _not_ an integer :)
    parse "3.14" int
        |> Result.mapError Parser.Error.message
    --> Err "1:2: I was expecting an integer value. I got stuck when I got the character '.'."

    -- But not with invalid numbers.
    parse "abc" int
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting an integer value. I got stuck when I got the character 'a'."

-}
int : Parser Int
int =
    concat [ zeroOrOne (char '-'), oneOrMore digit ]
        |> notFollowedBy (char '.')
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
        |> expecting "an integer value"


{-| Matches a decimal value as a `Float`.

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
    --> Err "1:1: I was expecting a digit [0-9]. I got stuck when I got the character '.'."

    parse "abc" number
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting a digit [0-9]. I got stuck when I got the character 'a'."

-}
number : Parser Float
number =
    concat
        [ zeroOrOne (oneOf [ char '-', char '+' ])
        , oneOf
            [ concat
                [ exactly 1 (char '.')
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


{-| Matches a specific text string.
This is case sensitive.

    import Parser exposing (parse)

    -- Match an exact text, case sensitive.
    parse "abcdef" (text "abc") --> Ok "abc"

    -- But anything else makes it fail.
    import Parser.Error

    text "abc"
        |> parse "abCDEF"
        |> Result.mapError Parser.Error.message
    --> Err "1:3: I was expecting the text 'abc'. I got stuck when I got the character 'C'."

-}
text : String -> Parser String
text str =
    textOf (sequence (List.map char (String.toList str)))
        |> expecting ("the text '" ++ str ++ "'")


{-| Matches a specific text string.
This is case insensitive.

    import Parser exposing (parse)

    -- Match an exact text, case insensitive.
    parse "abcdef" (textNoCase "abc") --> Ok "abc"
    parse "ABCDEF" (textNoCase "abc") --> Ok "ABC"

    -- But anything else makes it fail.
    import Parser.Error

    textNoCase "abc"
        |> parse "ab@"
        |> Result.mapError Parser.Error.message
    --> Err "1:3: I was expecting the text 'abc' (case insensitive). I got stuck when I got the character '@'."

-}
textNoCase : String -> Parser String
textNoCase str =
    textOf (sequence (List.map charNoCase (String.toList str)))
        |> expecting ("the text '" ++ str ++ "' (case insensitive)")


{-| Matches one or more digit.

> ℹ️ Equivalent regular expression: `[0-9]+` or `\d+`

    import Parser exposing (parse)

    -- Match many digits.
    parse "123abc" digits --> Ok "123"

    -- But anything else makes it fail.
    import Parser.Error

    digits
        |> parse "abc123"
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting one or more digits [0-9]+. I got stuck when I got the character 'a'."

-}
digits : Parser String
digits =
    textOf (oneOrMore digit)
        |> expecting "one or more digits [0-9]+"


{-| Matches one or more letter.
This is case insensitive.

> ℹ️ Equivalent regular expression: `[a-zA-Z]+`

    import Parser exposing (parse)

    -- Match many letters, case insensitive.
    parse "abc123" letters --> Ok "abc"
    parse "ABC123" letters --> Ok "ABC"

    -- But anything else makes it fail.
    import Parser.Error

    letters
        |> parse "123abc"
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting one or more letters [a-zA-Z]+. I got stuck when I got the character '1'."

-}
letters : Parser String
letters =
    textOf (oneOrMore letter)
        |> expecting "one or more letters [a-zA-Z]+"


{-| Matches zero or more Unicode blank spaces, including new lines.

> ℹ️ Equivalent regular expression: `[ \t\n\r\f]*` or `\s*`

    import Parser exposing (parse)

    -- Match many spaces.
    parse "    abc" spaces --> Ok "    "

    parse "\n\t abc" spaces --> Ok "\n\t "

    -- Including zero spaces :)
    parse "abc" spaces --> Ok ""

-}
spaces : Parser String
spaces =
    textOf (zeroOrMore space)


{-| Matches a line from the input text, delimited by '\\n'.

    import Parser exposing (parse)
    import Parser.Error
    import Parser.Sequence exposing (zeroOrMore)

    -- A line could be delimited by the newline character '\n'.
    parse "abc\ndef" line --> Ok "abc"

    -- Or this could also be the last line.
    parse "abc" line --> Ok "abc"

    -- An empty line still counts.
    parse "\n" line --> Ok ""

    -- But not an empty file.
    line
        |> parse ""
        |> Result.mapError Parser.Error.message
    --> Err "1:0: I was expecting a line. I reached the end of the input text."

    -- So we can parse multiple lines.
    zeroOrMore line
        |> parse "abc\ndef\nghi"
    --> Ok [ "abc", "def", "ghi"]

-}
line : Parser String
line =
    \state ->
        if String.isEmpty state.remaining then
            expected "a line" state

        else
            textOf (anyChar |> until endOfLine |> map Tuple.first) state


{-| Matches a parser as a `token` that can be preceded and followed by
zero or more spaces.

    import Parser exposing (map3, parse)
    import Parser.Char exposing (char)

    parse "1+2" (map3 (\x _ y -> x + y) int (token (char '+')) int) --> Ok 3
    parse "1 + 2" (map3 (\x _ y -> x + y) int (token (char '+')) int) --> Ok 3
    parse "1  +  2" (map3 (\x _ y -> x + y) int (token (char '+')) int) --> Ok 3

-}
token : Parser a -> Parser a
token parser =
    succeed Basics.identity
        |> drop spaces
        |> take parser
        |> drop spaces



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
