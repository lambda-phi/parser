module Parser.Check exposing
    ( followedBy, notFollowedBy
    , precededBy, notPrecededBy
    , wordBoundary, beginningOfLine, endOfLine, beginning, end
    )

{-| Parsers to check for conditions, but not consume any inputs.


# Lookahead

@docs followedBy, notFollowedBy


# Lookbehind

@docs precededBy, notPrecededBy


# Locations

@docs wordBoundary, beginningOfLine, endOfLine, beginning, end

-}

import Parser exposing (Parser, andThen, andThenKeep, expected, expecting, map, oneOf, orElse, succeed)
import Parser.Char exposing (alphaNum, anyChar, except)



-- Lookahead


{-| Succeeds only if the input text is followed by a _lookahead_ parser.
This does not consume any inputs.

If you want to consume the inputs or use the matched value in any way,
consider using [`andThen`](#andThen).

> ℹ️ Equivalent regular expression: `(?=...)` _(positive lookahead)_

    import Parser exposing (parse, succeed)
    import Parser.Char exposing (digit)
    import Parser.Common exposing (letters)
    import Parser.Error

    -- Succeed only if it's `followedBy` a digit.
    succeed ":)"
        |> followedBy digit
        |> parse "123"
    --> Ok ":)"

    -- Match letters only if it's `followedBy` a digit.
    letters
        |> followedBy digit
        |> parse "abc123"
    --> Ok "abc"

    -- Even if we match the letters, fail if the next character is not a digit.
    letters
        |> followedBy digit
        |> parse "abc@def"
        |> Result.mapError Parser.Error.message
    --> Err "1:4: I was expecting a digit [0-9]. I got stuck when I got the character '@'."

-}
followedBy : Parser lookahead -> Parser a -> Parser a
followedBy lookahead parser =
    \state ->
        parser state
            |> Result.andThen
                (\( value, nextState ) ->
                    lookahead nextState
                        |> Result.map (\_ -> ( value, nextState ))
                )


{-| Succeeds only if the input text is _not_ followed by a _lookahead_ parser.
This does not consume any inputs.

> ℹ️ It's a good idea to use [`expecting`](#expecting) alongside this function
> to improve the error messages.

> ℹ️ Equivalent regular expression: `(?!...)` _(negative lookahead)_

    import Parser exposing (expecting, parse, succeed)
    import Parser.Char exposing (char, digit)
    import Parser.Common exposing (letters)
    import Parser.Error

    -- Succeed only if it's `notFollowedBy` a digit.
    succeed ":)"
        |> notFollowedBy digit
        |> parse "abc"
    --> Ok ":)"

    -- Match letters only if it's `notFollowedBy` a digit.
    letters
        |> notFollowedBy digit
        |> expecting "letters not followed by a number"
        |> parse "abc@def"
    --> Ok "abc"

    -- Even if we match the letters, fail if the next character is a digit.
    -- This is the default error message, but you can use `expecting` to improve it.
    letters
        |> notFollowedBy digit
        |> parse "abc123"
        |> Result.mapError Parser.Error.message
    --> Err "1:4: I was expecting to not match a pattern, but I did. I got stuck when I got the character '1'."

-}
notFollowedBy : Parser lookahead -> Parser a -> Parser a
notFollowedBy lookahead parser =
    \state ->
        parser state
            |> Result.andThen
                (\( value, nextState ) ->
                    case lookahead nextState of
                        Ok ( _, lookaheadState ) ->
                            expected "to not match a pattern, but I did" lookaheadState

                        Err _ ->
                            Ok ( value, nextState )
                )



-- Lookbehind


{-| Succeeds only if the last character matches the parser provided.
This does not consume any inputs.

    import Parser exposing (andThenKeep, parse)
    import Parser.Char exposing (anyChar, char)
    import Parser.Common exposing (letters)
    import Parser.Error

    -- Make sure some letters were preceded by a '_'.
    anyChar
        |> andThenKeep (letters |> precededBy (char '_'))
        |> parse "_abc"
    --> Ok "abc"

    -- If it was something different, it fails.
    anyChar
        |> andThenKeep (letters |> precededBy (char '_'))
        |> parse "@abc"
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting the character '_'. I got stuck when I got the character '@'."

-}
precededBy : Parser Char -> Parser a -> Parser a
precededBy lastChar parser =
    \state ->
        parser state
            |> Result.andThen
                (\( value, nextState ) ->
                    lastChar
                        { state
                            | remaining =
                                state.lastChar
                                    |> Maybe.map String.fromChar
                                    |> Maybe.withDefault ""
                            , col = state.col - 1
                        }
                        |> Result.map (\_ -> ( value, nextState ))
                )


{-| Succeeds only if the last character does _not_ match the parser provided.
This does not consume any inputs.

    import Parser exposing (andThenKeep, parse)
    import Parser.Char exposing (anyChar, char)
    import Parser.Common exposing (letters)
    import Parser.Error

    -- Make sure some letters were not preceded by a '_'.
    anyChar
        |> andThenKeep (letters |> notPrecededBy (char '_'))
        |> parse "@abc"
    --> Ok "abc"

    -- If it was preceded by '_', it fails.
    anyChar
        |> andThenKeep (letters |> notPrecededBy (char '_'))
        |> parse "_abc"
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting a different character. I got stuck when I got the character '_'."

-}
notPrecededBy : Parser Char -> Parser a -> Parser a
notPrecededBy lastChar =
    precededBy (except lastChar)



-- Locations


{-| Succeeds only if the current position is the beginning or end of a word.
This does not consume any inputs.

    import Parser exposing (andThenKeep, parse, textOf)
    import Parser.Char exposing (anyChar)
    import Parser.Error
    import Parser.Sequence exposing (zeroOrMore)

    --- Beginning of a word ---

    -- It can start at the beginning of the input text, followed by an alphanumeric.
    wordBoundary
        |> andThenKeep (textOf (zeroOrMore anyChar))
        |> parse "abc"
    --> Ok "abc"

    -- But not followed by anything else.
    wordBoundary
        |> andThenKeep (textOf (zeroOrMore anyChar))
        |> parse "@abc"
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting a word boundary. I got stuck when I got the character '@'."

    -- It can also start with an alphanumeric, preceded by a non-alphanumeric.
    anyChar
        |> followedBy wordBoundary
        |> andThenKeep (textOf (zeroOrMore anyChar))
        |> parse "@abc"
    --> Ok "abc"

    -- But not if preceded by anything else
    anyChar
        |> followedBy wordBoundary
        |> andThenKeep (textOf (zeroOrMore anyChar))
        |> parse "abc"
        |> Result.mapError Parser.Error.message
    --> Err "1:2: I was expecting a word boundary. I got stuck when I got the character 'b'."


    --- End of a word ---

    -- It can end at the end of the input text, preceded by an alphanumeric.
    textOf (zeroOrMore anyChar)
        |> followedBy wordBoundary
        |> parse "abc"
    --> Ok "abc"

    -- But not preceded by anything else.
    textOf (zeroOrMore anyChar)
        |> followedBy wordBoundary
        |> parse "abc@"
        |> Result.mapError Parser.Error.message
    --> Err "1:4: I was expecting a word boundary. I got stuck when I got the character '@'."

    -- It can also end with a non-alphanumeric, preceded by an alphanumeric.
    anyChar
        |> followedBy wordBoundary
        |> parse "a@"
    --> Ok 'a'

    -- But not if preceded by anything else
    anyChar
        |> followedBy wordBoundary
        |> parse "ab"
        |> Result.mapError Parser.Error.message
    --> Err "1:2: I was expecting a word boundary. I got stuck when I got the character 'b'."

-}
wordBoundary : Parser ()
wordBoundary =
    oneOf
        [ beginning |> followedBy alphaNum
        , succeed () |> precededBy (except alphaNum) |> followedBy alphaNum
        , end |> precededBy alphaNum
        , succeed () |> precededBy alphaNum |> followedBy (except alphaNum)
        ]
        |> orElse (anyChar |> andThenKeep (expected ""))
        |> expecting "a word boundary"


{-| Succeeds only the parser is at the beginning of a new line or
at the beginning of the input text.
This does not consume any inputs.

> ℹ️ Equivalent regular expression: `^`

    import Parser exposing (andThenKeep, parse)
    import Parser.Char exposing (anyChar)
    import Parser.Common exposing (line)
    import Parser.Error

    -- Succeed at the beginning of the file.
    beginningOfLine
        |> andThenKeep line
        |> parse "abc\n123"
    --> Ok "abc"

    -- The end of file also counts.
    line
        |> followedBy beginningOfLine
        |> andThenKeep line
        |> parse "abc\n123"
    --> Ok "123"

    -- But fail otherwise.
    anyChar
        |> followedBy beginningOfLine
        |> parse "abc"
        |> Result.mapError Parser.Error.message
    --> Err "1:2: I was expecting the beginning of a line. I got stuck when I got the character 'b'."

-}
beginningOfLine : Parser ()
beginningOfLine =
    \state ->
        case state.lastChar of
            Nothing ->
                succeed () state

            Just '\n' ->
                succeed () state

            -- Carriage return '\r'
            Just '\u{000D}' ->
                succeed () state

            _ ->
                (map (\_ -> ()) anyChar |> andThenKeep (expected "the beginning of a line")) state


{-| Succeeds only the parser is at the end of the current line or there are
no more remaining characters in the input text.
This does not consume any inputs.

> ℹ️ Equivalent regular expression: `$`

    import Parser exposing (parse)
    import Parser.Common exposing (letters, line)
    import Parser.Error

    -- Succeed if we've reached the end of line.
    letters
        |> followedBy endOfLine
        |> parse "abc\n123"
    --> Ok "abc"

    -- A carriage return also counts.
    letters
        |> followedBy endOfLine
        |> parse "abc\r123"
    --> Ok "abc"

    -- The end of file also counts.
    letters
        |> followedBy endOfLine
        |> parse "abc"
    --> Ok "abc"

    -- But fail otherwise.
    letters
        |> followedBy endOfLine
        |> parse "abc123"
        |> Result.mapError Parser.Error.message
    --> Err "1:4: I was expecting the end of the current line. I got stuck when I got the character '1'."

-}
endOfLine : Parser ()
endOfLine =
    \state ->
        case anyChar state of
            Err _ ->
                succeed () state

            Ok ( '\n', nextState ) ->
                succeed () nextState

            -- Carriage return '\r'
            Ok ( '\u{000D}', nextState ) ->
                succeed () nextState

            Ok ( _, nextState ) ->
                expected "the end of the current line" nextState


{-| Succeeds only if it's the beginning of the input text.
This does not consume any inputs.

    import Parser exposing (andThenKeep, parse)
    import Parser.Common exposing (letters)
    import Parser.Error

    -- Succeed if we are at the beginning of file.
    beginning
        |> andThenKeep letters
        |> parse "abc"
    --> Ok "abc"

    -- Or fail otherwise.
    letters
        |> followedBy beginning
        |> parse "abc123"
        |> Result.mapError Parser.Error.message
    --> Err "1:3: I was expecting the beginning of the input text. I got stuck when I got the character 'c'."

-}
beginning : Parser ()
beginning =
    \state ->
        case state.lastChar of
            Nothing ->
                succeed () state

            Just _ ->
                expected "the beginning of the input text" state


{-| Succeeds only if there are no more remaining characters in the input text.
This does not consume any inputs.

    import Parser exposing (parse)
    import Parser.Common exposing (letters)
    import Parser.Error

    -- Succeed if we've reached the end of file.
    letters
        |> followedBy end
        |> parse "abc"
    --> Ok "abc"

    -- Or fail otherwise.
    letters
        |> followedBy end
        |> parse "abc123"
        |> Result.mapError Parser.Error.message
    --> Err "1:4: I was expecting the end of the input text, but 3 characters are still remaining. I got stuck when I got the character '1'."

-}
end : Parser ()
end =
    \state ->
        case anyChar state of
            Err _ ->
                succeed () state

            Ok ( _, nextState ) ->
                expected
                    ("the end of the input text, but "
                        ++ String.fromInt (String.length state.remaining)
                        ++ " characters are still remaining"
                    )
                    nextState
