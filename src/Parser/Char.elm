module Parser.Char exposing (anyChar, char, charNoCase, digit, letter, lowercase, uppercase, alphaNum, space, punctuation, except)

{-| Parsers for characters.

@docs anyChar, char, charNoCase, digit, letter, lowercase, uppercase, alphaNum, space, punctuation, except

-}

import Parser exposing (Parser, andThen, expected, expecting, oneOf, succeed)


{-| Matches any single character.

> ℹ️ Equivalent regular expression: `.`

    import Parser exposing (parse)

    -- We can match any character.
    parse "abc" anyChar --> Ok 'a'
    parse "#hashtag" anyChar --> Ok '#'

    -- This can only fail if we run out of inputs.
    import Parser.Error

    anyChar
        |> parse ""
        |> Result.mapError Parser.Error.message
    --> Err "1:0: I was expecting a character. I reached the end of the input text."

-}
anyChar : Parser Char
anyChar =
    \state ->
        case String.uncons state.remaining of
            Just ( ch, tail ) ->
                succeed ch
                    (if ch == '\n' then
                        { state
                            | remaining = tail
                            , lastChar = Just ch
                            , row = state.row + 1
                            , col = 1
                        }

                     else
                        { state
                            | remaining = tail
                            , lastChar = Just ch
                            , col = state.col + 1
                        }
                    )

            Nothing ->
                expected "a character" state


{-| Matches a specific single character.
This is case sensitive.

    import Parser exposing (parse)

    -- Match a specific character, case sensitive.
    parse "abc" (char 'a') --> Ok 'a'

    -- It fails if it's not _exactly_ the same.
    import Parser.Error

    char 'a'
        |> parse "ABC"
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting the character 'a'. I got stuck when I got the character 'A'."

-}
char : Char -> Parser Char
char ch =
    andThen
        (\c ->
            if c == ch then
                succeed c

            else
                expected ""
        )
        anyChar
        |> expecting ("the character '" ++ String.fromChar ch ++ "'")


{-| Matches a specific single character.
This is case insensitive.

    import Parser exposing (parse)

    -- Match a specific character, case insensitive.
    parse "abc" (charNoCase 'a') --> Ok 'a'
    parse "ABC" (charNoCase 'a') --> Ok 'A'

    -- But anything else makes it fail.
    import Parser.Error

    charNoCase 'a'
        |> parse "123"
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting the character 'a' (case insensitive). I got stuck when I got the character '1'."

-}
charNoCase : Char -> Parser Char
charNoCase ch =
    andThen
        (\c ->
            if Char.toLower c == Char.toLower ch then
                succeed c

            else
                expected ""
        )
        anyChar
        |> expecting ("the character '" ++ String.fromChar ch ++ "' (case insensitive)")


{-| Matches exactly one digit character.

> ℹ️ Equivalent regular expression: `[0-9]` or `\d`

    import Parser exposing (parse)

    -- Match a digit.
    parse "123" digit --> Ok '1'
    parse "3.14" digit --> Ok '3'

    -- But anything else makes it fail.
    import Parser.Error

    digit
        |> parse "abc"
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting a digit [0-9]. I got stuck when I got the character 'a'."

-}
digit : Parser Char
digit =
    andThen
        (\c ->
            if Char.isDigit c then
                succeed c

            else
                expected ""
        )
        anyChar
        |> expecting "a digit [0-9]"


{-| Matches exactly one letter character.
This is case insensitive.

> ℹ️ Equivalent regular expression: `[a-zA-Z]`

    import Parser exposing (parse)

    -- Match any letter, case insensitive.
    parse "abc" letter --> Ok 'a'
    parse "ABC" letter --> Ok 'A'

    -- But anything else makes it fail.
    import Parser.Error

    letter
        |> parse "123"
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '1'."

-}
letter : Parser Char
letter =
    andThen
        (\c ->
            if Char.isAlpha c then
                succeed c

            else
                expected ""
        )
        anyChar
        |> expecting "a letter [a-zA-Z]"


{-| Matches exactly one lowercase letter character.
This is case sensitive.

> ℹ️ Equivalent regular expression: `[a-z]`

    import Parser exposing (parse)

    -- Match a lowercase letter.
    parse "abc" lowercase --> Ok 'a'

    -- But anything else makes it fail.
    import Parser.Error

    lowercase
        |> parse "ABC"
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting a lowercase letter [a-z]. I got stuck when I got the character 'A'."

-}
lowercase : Parser Char
lowercase =
    andThen
        (\c ->
            if Char.isLower c then
                succeed c

            else
                expected ""
        )
        anyChar
        |> expecting "a lowercase letter [a-z]"


{-| Matches exactly one uppercase letter character.
This is case sensitive.

> ℹ️ Equivalent regular expression: `[A-Z]`

    import Parser exposing (parse)

    -- Match an uppercase letter.
    parse "ABC" uppercase --> Ok 'A'

    -- But anything else makes it fail.
    import Parser.Error

    uppercase
        |> parse "abc"
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting an uppercase letter [A-Z]. I got stuck when I got the character 'a'."

-}
uppercase : Parser Char
uppercase =
    andThen
        (\c ->
            if Char.isUpper c then
                succeed c

            else
                expected ""
        )
        anyChar
        |> expecting "an uppercase letter [A-Z]"


{-| Matches exactly one letter or digit character.
This is case insensitive.

> ℹ️ Equivalent regular expression: `[a-zA-Z0-9]`

    import Parser exposing (parse)

    -- Match a letter or number.
    parse "abc" alphaNum --> Ok 'a'
    parse "ABC" alphaNum --> Ok 'A'
    parse "123" alphaNum --> Ok '1'

    -- But anything else makes it fail.
    import Parser.Error

    alphaNum
        |> parse "_abc"
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting a letter or a digit [a-zA-Z0-9]. I got stuck when I got the character '_'."

-}
alphaNum : Parser Char
alphaNum =
    andThen
        (\c ->
            if Char.isAlphaNum c then
                succeed c

            else
                expected ""
        )
        anyChar
        |> expecting "a letter or a digit [a-zA-Z0-9]"


{-| Matches a Unicode blank space character, including new lines.
A blank space can be a
regular space `' '`,
tab `'\t'`,
new line `'\n'`,
carriage return `'\r'`,
or form feed `'\f'`.

> ℹ️ Equivalent regular expression: `[ \t\n\r\f]` or `\s`

    import Parser exposing (parse)

    -- Match a space space.
    parse "    abc" space --> Ok ' '
    parse "\n\t abc" space --> Ok '\n'

    -- But anything else makes it fail.
    import Parser.Error

    parse "abc" space
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting a blank space or new line. I got stuck when I got the character 'a'."

-}
space : Parser Char
space =
    oneOf
        [ char ' ' -- space
        , char '\t' -- tab
        , char '\n' -- new line
        , char '\u{000D}' -- \r -- carriage return
        , char '\u{000C}' -- \f -- form feed
        ]
        |> expecting "a blank space or new line"


{-| Matches an ASCII punctuation character.
A punctuation character can be any of
`!`, `"`, `#`, `$`, `%`, `&`, `'`, `(`, `)`, `*`, `+`, `,`, `-`, `.`,
`/`, `:`, `;`, `<`, `=`, `>`, `?`, `@`, `[`, `]`, `^`, `_`, `\``,`{`,`}`, or`~\`.

> ℹ️ Equivalent regular expression: `[!"#$%&'()*+,-./:;<=>?@[\]^_\\{}~]`

    import Parser exposing (parse)

    -- Match a punctuation character.
    parse "#hashtag" punctuation --> Ok '#'
    parse "=123" punctuation --> Ok '='

    -- But anything else makes it fail.
    import Parser.Error

    punctuation
        |> parse "abc"
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting a punctuation character. I got stuck when I got the character 'a'."

-}
punctuation : Parser Char
punctuation =
    List.map char
        [ '!', '"', '#', '$', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/', ':', ';', '<', '=', '>', '?', '@', '[', ']', '^', '_', '\\', '{', '}', '~' ]
        |> oneOf
        |> expecting "a punctuation character"


{-| Matches any character `except` the parser provided.

> ℹ️ It's a good idea to use [`expecting`](#expecting) alongside this function
> to improve the error messages.

    import Parser exposing (parse)
    import Parser.Error

    -- Anything except a letter is okay.
    parse "123" (except letter) --> Ok '1'
    parse "-123" (except letter) --> Ok '-'

    -- But a letter is not.
    except letter
        |> parse "abc"
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting a different character. I got stuck when I got the character 'a'."

-}
except : Parser Char -> Parser Char
except parser =
    \state ->
        case parser state of
            Ok ( _, nextState ) ->
                expected "a different character" nextState

            Err _ ->
                anyChar state
