module Parser exposing
    ( Parser, Error, parse, into, take, drop
    , anyChar, char, charNoCase, digit, digits, letter, letters, lowercase, uppercase, alphanumeric, space, spaces
    , text, textNoCase, textOf, line
    , sequence, concat, oneOf, maybe, zeroOrOne, zeroOrMore, oneOrMore, repeat, atLeast, atMost, between, until, while
    , succeed, fail, end, andThen, andThen2, orElse, withError
    , map, map2, map3, map4, map5, mapList
    )

{-| Intuitive and easy to use parser library.


# Basic usage

@docs Parser, Error, parse, into, take, drop


# Matching characters

@docs anyChar, char, charNoCase, digit, digits, letter, letters, lowercase, uppercase, alphanumeric, space, spaces


# Text operations

@docs text, textNoCase, textOf, line


# List operations

@docs sequence, concat, oneOf, maybe, zeroOrOne, zeroOrMore, oneOrMore, repeat, atLeast, atMost, between, until, while


# Chaining

@docs succeed, fail, end, andThen, andThen2, orElse, withError


# Mapping

@docs map, map2, map3, map4, map5, mapList


# Error reporting

@docs Error, errorDump, errorDumpSnippet, errorDumpContext

-}

import Result
import String



---=== TYPE DEFINITIONS ===---


{-| A `Parser` is defined as a function that takes an input state,
including an input text, and returns either an [`Error`](#Error),
or a value with the next state.
-}
type alias Parser a =
    State -> Result Error ( a, State )


{-| Contains the description of an error.
This includes an error message, the position, and the context stack.
-}
type alias Error =
    { message : String
    , input : String
    , row : Int
    , col : Int
    , context : List Context
    }


type alias State =
    { input : String
    , remaining : String
    , row : Int
    , col : Int
    , context : List Context
    }


type alias Context =
    { name : String
    , row : Int
    , col : Int
    }



---=== BASIC USAGE ===---


{-| Parse an input text, and get either an [`Error`](#Error)
or the parsed value as a result.

    parse "abc" letter --> Ok 'a'

    letter
        |> parse "123"
        |> Result.mapError .message
    --> Err "expected a letter [a-zA-Z], but got '1' instead"

-}
parse : String -> Parser a -> Result Error a
parse input parser =
    parser
        { input = input
        , remaining = input
        , row = 1
        , col = 1
        , context = []
        }
        |> Result.map Tuple.first


{-| Starts a parsing pipeline to parse into a data type.

You should also specify a name for the context,
this helps to give better error messages.

    import Parser.Common exposing (number)

    type alias Point =
        { x : Float
        , y : Float
        }

    point : Parser Point
    point =
        into "Point"
            (succeed Point
                |> drop (char '(')
                |> take number
                |> drop (char ',')
                |> take number
                |> drop (char ')')
            )

    parse "(12,34)" point --> Ok {x = 12, y = 34}

-}
into : String -> Parser a -> Parser a
into context parser state =
    let
        ctx =
            { name = context
            , row = state.row
            , col = state.col
            }
    in
    parser { state | context = ctx :: state.context }
        |> Result.andThen
            (\( value, newState ) ->
                succeed value { newState | context = state.context }
            )


{-| Takes a parsed value and feeds it to the return value of the parser.
-}
take : Parser a -> Parser (a -> b) -> Parser b
take next parser =
    andThen2 (\f x -> succeed (f x))
        parser
        next


{-| Ignores a parsed value, but it still must match to continue.
-}
drop : Parser a -> Parser b -> Parser b
drop next parser =
    andThen2 (\x _ -> succeed x)
        parser
        next



---=== MATCHING TEXT ===---


{-| Matches any single character.

> ℹ️ Equivalent regular expression: `.`

    parse "abc" anyChar --> Ok 'a'

    anyChar
        |> parse ""
        |> Result.mapError .message
    --> Err "expected a character, but reached the end of the input text"

-}
anyChar : Parser Char
anyChar state =
    case String.uncons state.remaining of
        Just ( ch, tail ) ->
            succeed ch
                (if ch == '\n' then
                    { state
                        | remaining = tail
                        , row = state.row + 1
                        , col = 1
                    }

                 else
                    { state
                        | remaining = tail
                        , col = state.col + 1
                    }
                )

        Nothing ->
            fail "expected a character, but reached the end of the input text" state


{-| Matches a specific single character.
This is case sensitive.

    parse "abc" (char 'a') --> Ok 'a'

    char 'a'
        |> parse "ABC"
        |> Result.mapError .message
    --> Err "expected the character 'a', but got 'A' instead"

-}
char : Char -> Parser Char
char ch =
    andThen
        (\c ->
            if c == ch then
                succeed c

            else
                fail
                    ("expected the character '"
                        ++ String.fromChar ch
                        ++ "', but got '"
                        ++ String.fromChar c
                        ++ "' instead"
                    )
        )
        anyChar


{-| Matches a specific single character.
This is case insensitive.

    parse "abc" (charNoCase 'a') --> Ok 'a'

    parse "ABC" (charNoCase 'a') --> Ok 'A'

    charNoCase 'a'
        |> parse "123"
        |> Result.mapError .message
    --> Err "expected the character 'a' or 'A', but got '1' instead"

-}
charNoCase : Char -> Parser Char
charNoCase ch =
    andThen
        (\c ->
            if Char.toLower c == Char.toLower ch then
                succeed c

            else
                fail
                    ("expected the character '"
                        ++ String.fromChar (Char.toLower ch)
                        ++ "' or '"
                        ++ String.fromChar (Char.toUpper ch)
                        ++ "', but got '"
                        ++ String.fromChar c
                        ++ "' instead"
                    )
        )
        anyChar


{-| Matches a digit character.

> ℹ️ Equivalent regular expression: `[0-9]` or `\d`

    parse "123" digit --> Ok '1'

    digit
        |> parse "abc"
        |> Result.mapError .message
    --> Err "expected a digit [0-9], but got 'a' instead"

-}
digit : Parser Char
digit =
    andThen
        (\c ->
            if Char.isDigit c then
                succeed c

            else
                fail
                    ("expected a digit [0-9], but got '"
                        ++ String.fromChar c
                        ++ "' instead"
                    )
        )
        anyChar


{-| Matches one or more digit.

> ℹ️ Equivalent regular expression: `[0-9]+` or `\d+`

    parse "123abc" digits --> Ok "123"

    digits
        |> parse "abc123"
        |> Result.mapError .message
    --> Err "expected a digit [0-9], but got 'a' instead"

-}
digits : Parser String
digits =
    textOf (oneOrMore digit)


{-| Matches a letter character.
This is case insensitive.

> ℹ️ Equivalent regular expression: `[a-zA-Z]`

    parse "abc" letter --> Ok 'a'

    parse "ABC" letter --> Ok 'A'

    letter
        |> parse "123"
        |> Result.mapError .message
    --> Err "expected a letter [a-zA-Z], but got '1' instead"

-}
letter : Parser Char
letter =
    andThen
        (\c ->
            if Char.isAlpha c then
                succeed c

            else
                fail
                    ("expected a letter [a-zA-Z], but got '"
                        ++ String.fromChar c
                        ++ "' instead"
                    )
        )
        anyChar


{-| Matches one or more letter.
This is case insensitive.

> ℹ️ Equivalent regular expression: `[a-zA-Z]+`

    parse "abc123" letters --> Ok "abc"

    parse "ABC123" letters --> Ok "ABC"

    letters
        |> parse "123abc"
        |> Result.mapError .message
    --> Err "expected a letter [a-zA-Z], but got '1' instead"

-}
letters : Parser String
letters =
    textOf (oneOrMore letter)


{-| Matches a lowercase letter character.
This is case sensitive.

> ℹ️ Equivalent regular expression: `[a-z]`

    parse "abc" lowercase --> Ok 'a'

    lowercase
        |> parse "ABC"
        |> Result.mapError .message
    --> Err "expected a lowercase letter [a-z], but got 'A' instead"

-}
lowercase : Parser Char
lowercase =
    andThen
        (\c ->
            if Char.isLower c then
                succeed c

            else
                fail
                    ("expected a lowercase letter [a-z], but got '"
                        ++ String.fromChar c
                        ++ "' instead"
                    )
        )
        anyChar


{-| Matches an uppercase letter character.
This is case sensitive.

> ℹ️ Equivalent regular expression: `[A-Z]`

    parse "ABC" uppercase --> Ok 'A'

    uppercase
        |> parse "abc"
        |> Result.mapError .message
    --> Err "expected an uppercase letter [A-Z], but got 'a' instead"

-}
uppercase : Parser Char
uppercase =
    andThen
        (\c ->
            if Char.isUpper c then
                succeed c

            else
                fail
                    ("expected an uppercase letter [A-Z], but got '"
                        ++ String.fromChar c
                        ++ "' instead"
                    )
        )
        anyChar


{-| Matches a letter or digit character.
This is case insensitive.

> ℹ️ Equivalent regular expression: `[a-zA-Z0-9]`

    parse "abc" alphanumeric --> Ok 'a'

    parse "ABC" alphanumeric --> Ok 'A'

    parse "123" alphanumeric --> Ok '1'

    alphanumeric
        |> parse "_abc"
        |> Result.mapError .message
    --> Err "expected a letter or digit [a-zA-Z0-9], but got '_' instead"

-}
alphanumeric : Parser Char
alphanumeric =
    andThen
        (\c ->
            if Char.isAlphaNum c then
                succeed c

            else
                fail
                    ("expected a letter or digit [a-zA-Z0-9], but got '"
                        ++ String.fromChar c
                        ++ "' instead"
                    )
        )
        anyChar


{-| Matches a blank space character.
A blank space can be a
regular space `' '`,
tab `'\t'`,
new line `'\n'`,
carriage return `'\r'`,
form feed `'\f'`,
or vertical tab `'\v'`.

> ℹ️ Equivalent regular expression: `[ \t\n\r\f\v]` or `\s`

    parse "    abc" space --> Ok ' '

    parse "\tabc" space --> Ok '\t'

    space
        |> parse "abc"
        |> Result.mapError .message
    --> Err "expected a blank space character, but got 'a' instead"

-}
space : Parser Char
space =
    oneOf
        [ char ' ' -- space
        , char '\t' -- tab
        , char '\n' -- new line
        , char '\u{000D}' -- \r -- carriage return
        , char '\u{000C}' -- \f -- form feed
        , char '\u{000B}' -- \v -- vertical tab
        ]
        |> orElse
            (anyChar
                |> andThen
                    (\c ->
                        fail
                            ("expected a blank space character, but got '"
                                ++ String.fromChar c
                                ++ "' instead"
                            )
                    )
            )


{-| Matches zero or more spaces.

> ℹ️ Equivalent regular expression: `[ \t\n\r\f\v]*` or `\s*`

    parse "    abc" spaces --> Ok "    "

    parse "\tabc" spaces --> Ok "\t"

    parse "abc" spaces --> Ok ""

-}
spaces : Parser String
spaces =
    textOf (zeroOrMore space)



---=== TEXT OPERATIONS ===---


{-| Matches a specific text string.
This is case sensitive.

    parse "abcdef" (text "abc") --> Ok "abc"

    text "abc"
        |> parse "ABCDEF"
        |> Result.mapError .message
    --> Err "expected the character 'a', but got 'A' instead"

-}
text : String -> Parser String
text str =
    textOf (sequence (List.map char (String.toList str)))


{-| Matches a specific text string.
This is case insensitive.

    parse "abcdef" (textNoCase "abc") --> Ok "abc"

    parse "ABCDEF" (textNoCase "abc") --> Ok "ABC"

    textNoCase "abc"
        |> parse "123"
        |> Result.mapError .message
    --> Err "expected the character 'a' or 'A', but got '1' instead"

-}
textNoCase : String -> Parser String
textNoCase str =
    textOf (sequence (List.map charNoCase (String.toList str)))


{-| Gets a string from a list of characters.

    textOf (oneOrMore letter) -- letters
        |> parse "abc123" --> Ok "abc"

-}
textOf : Parser (List Char) -> Parser String
textOf =
    map String.fromList


{-| Gets a line from the input text, delimited by '\\n'.

    parse "abc\ndef" line --> Ok "abc"

-}
line : Parser String
line =
    textOf (until (char '\n') anyChar)



---=== LIST OPERATIONS ===---


{-| Matches a sequence of parsers in order, and gets the result as a `List`.

    sequence [ char '_', letter, digit ]
        |> parse "_A5" --> Ok [ '_', 'A', '5' ]

-}
sequence : List (Parser a) -> Parser (List a)
sequence parsers initialState =
    List.foldl
        (\parser ->
            Result.andThen
                (\( results, state ) ->
                    parser state
                        |> Result.map
                            (Tuple.mapFirst (\result -> results ++ [ result ]))
                )
        )
        (Ok ( [], initialState ))
        parsers


{-| Concatenates the parsed values from all the parsers into a single list with
all the values.

    concat
        [ oneOrMore digit       -- [ '3' ]
        , zeroOrOne (char '.')  -- [ '.' ]
        , zeroOrMore digit      -- [ '1', '4' ]
        ]
        |> parse "3.14"
    --> Ok [ '3', '.', '1', '4' ]

-}
concat : List (Parser (List a)) -> Parser (List a)
concat parsers =
    mapList List.concat parsers


{-| Returns the value of the first parser that matches.
It tries to match the parsers in order.

If none of the parsers match, it keeps the error message from the last parser.
To give a more descriptive error message, you can use [`withError`](#withError).

> ℹ️ Equivalent regular expression: `|`

    oneOf [ char '_', letter ]
        |> parse "_abc"
    --> Ok '_'

    oneOf [ char '_', letter ]
        |> parse "abc"
    --> Ok 'a'

    oneOf [ char '_', letter ]
        |> parse "123"
        |> Result.mapError .message
    --> Err "expected a letter [a-zA-Z], but got '1' instead"

-}
oneOf : List (Parser a) -> Parser a
oneOf parsers =
    List.foldl orElse (fail "") parsers


{-| Parses an optional value and returns it as a `Maybe`.

> ℹ️ Equivalent regular expression: `?`

    parse "abc" (maybe letter) --> Ok (Just 'a')

    parse "_abc" (maybe letter) --> Ok Nothing

-}
maybe : Parser a -> Parser (Maybe a)
maybe parser =
    map Just parser
        |> orElse (succeed Nothing)


{-| Parses an optional value and returns it as a `List`.

> ℹ️ Equivalent regular expression: `?`

    parse "abc" (zeroOrOne letter) --> Ok [ 'a' ]

    parse "_abc" (zeroOrOne letter) --> Ok []

-}
zeroOrOne : Parser a -> Parser (List a)
zeroOrOne parser =
    map (\x -> [ x ]) parser
        |> orElse (succeed [])


{-| Parses zero or more values and returns them as a `List`.

> ℹ️ Equivalent regular expression: `*`

    parse "abc" (zeroOrMore letter) --> Ok [ 'a', 'b', 'c' ]

    parse "_abc" (zeroOrMore letter) --> Ok []

-}
zeroOrMore : Parser a -> Parser (List a)
zeroOrMore parser =
    while (\_ _ -> True) parser


{-| Parses one or more values and returns them as a `List`.

> ℹ️ Equivalent regular expression: `+`

    parse "abc" (oneOrMore letter) --> Ok [ 'a', 'b', 'c' ]

    oneOrMore letter
        |> parse "_abc"
        |> Result.mapError .message
    --> Err "expected a letter [a-zA-Z], but got '_' instead"

-}
oneOrMore : Parser a -> Parser (List a)
oneOrMore parser =
    map2 (::) parser (zeroOrMore parser)


{-| Parses a value exactly a number of times and returns them as a `List`.

> ℹ️ Equivalent regular expression: `{n}`

    parse "abcdef" (repeat 3 letter) --> Ok [ 'a', 'b', 'c' ]

    repeat 3 letter
        |> parse "ab_def"
        |> Result.mapError .message
    --> Err "expected a letter [a-zA-Z], but got '_' instead"

-}
repeat : Int -> Parser a -> Parser (List a)
repeat n parser =
    sequence (List.repeat n parser)


{-| Parses a value at least a number of times and returns them as a `List`.

> ℹ️ Equivalent regular expression: `{min,}`

    parse "abcdef" (atLeast 3 letter) --> Ok [ 'a', 'b', 'c', 'd', 'e', 'f' ]

    atLeast 3 letter
        |> parse "ab_def"
        |> Result.mapError .message
    --> Err "expected a letter [a-zA-Z], but got '_' instead"

-}
atLeast : Int -> Parser a -> Parser (List a)
atLeast min parser =
    map2 (++)
        (repeat min parser)
        (zeroOrMore parser)


{-| Parses a value at most a number of times and returns them as a `List`.

> ℹ️ Equivalent regular expression: `{0,max}`

    parse "abcdef" (atMost 3 letter) --> Ok [ 'a', 'b', 'c' ]

    parse "ab_def" (atMost 3 letter) --> Ok [ 'a', 'b' ]

    parse "_bcdef" (atMost 3 letter) --> Ok []

-}
atMost : Int -> Parser a -> Parser (List a)
atMost max parser =
    while (\xs _ -> List.length xs < max) parser


{-| Parses a value between a range of times and returns them as a `List`.

> ℹ️ Equivalent regular expression: `{min,max}`

    parse "abcdef" (between 2 3 letter) --> Ok [ 'a', 'b', 'c' ]

    parse "ab_def" (between 2 3 letter) --> Ok [ 'a', 'b' ]

    between 2 3 letter
        |> parse "a_cdef"
        |> Result.mapError .message
    --> Err "expected a letter [a-zA-Z], but got '_' instead"

-}
between : Int -> Int -> Parser a -> Parser (List a)
between min max parser =
    map2 (++)
        (repeat min parser)
        (atMost (max - min) parser)


{-| Parses a values repeatedly until a delimiter parser matches.
The delimiter marks the end of the sequence, but is not consumed.

If the delimiter is not found, it parses until it stops matching
or until the end of the input text.

This could be used as a lookahead.

    until (char 'd') letter
        |> parse "abcdef"
    --> Ok [ 'a', 'b', 'c' ]

    until (char 'd') letter
        |> parse "abc123"
    --> Ok [ 'a', 'b', 'c' ]

    until (char 'd') letter
        |> parse "abc"
    --> Ok [ 'a', 'b', 'c' ]

-}
until : Parser stop -> Parser a -> Parser (List a)
until delimiter parser =
    let
        until_ values =
            andThen (\_ -> succeed values)
                delimiter
                |> orElse
                    (andThen (\value -> until_ (values ++ [ value ]))
                        parser
                        |> orElse (succeed values)
                    )
    in
    until_ []


{-| Parses values while a condition is still `True`.
Once the condition is `False`, the sequence stops and the current value is not consumed.

If the condition never gets to `False`, it parses until it stops matching
or until the end of the input text.

The condition function takes two inputs:
a list of all the previously matched values,
and the current value matched.

    while (\_ value -> value /= 'd') letter
        |> parse "abcdef"
    --> Ok [ 'a', 'b', 'c' ]

    while (\values _ -> List.length values < 3) letter
        |> parse "abcdef"
    --> Ok [ 'a', 'b', 'c' ]

    while (\_ value -> value /= 'd') letter
        |> parse "abc123"
    --> Ok [ 'a', 'b', 'c' ]

    while (\_ value -> value /= 'd') letter
        |> parse "abc"
    --> Ok [ 'a', 'b', 'c' ]

-}
while : (List a -> a -> Bool) -> Parser a -> Parser (List a)
while condition parser =
    let
        while_ values =
            andThen
                (\value ->
                    if condition values value then
                        while_ (values ++ [ value ])

                    else
                        succeed values
                )
                parser
                |> orElse (succeed values)
    in
    while_ []



---=== CHAINING ===---


{-| A parser that always succeeds with the given value.

    parse "" (succeed "abc") --> Ok "abc"

-}
succeed : a -> Parser a
succeed value state =
    Ok ( value, state )


{-| A parser that always fails with the given error message.

    fail "something went wrong :("
        |> parse ""
        |> Result.mapError .message
    --> Err "something went wrong :("

-}
fail : String -> Parser a
fail message state =
    Err
        { message = message
        , input = state.input
        , row = state.row
        , col = state.col - 1
        , context = state.context
        }


{-| Succeeds only if there are no more remaining characters in the input text.

> ℹ️ Equivalent regular expression: `$`

    letters
        |> end
        |> parse "abc"
    --> Ok "abc"

    letters
        |> end
        |> parse "abc123"
        |> Result.mapError .message
    --> Err "expected the end of the input text, but 3 characters are still remaining"

-}
end : Parser a -> Parser a
end parser initialState =
    parser initialState
        |> Result.andThen
            (\( value, state ) ->
                if String.isEmpty state.remaining then
                    succeed value state

                else
                    fail
                        ("expected the end of the input text, but "
                            ++ String.fromInt (String.length state.remaining)
                            ++ " characters are still remaining"
                        )
                        state
            )


{-| Parse one value `andThen` do something with that value,
which results in another parser.

This can be used to validate the last thing we parsed,
or transform the last value,
or to use the last value for the next parser like a backreference.

    textOf (oneOrMore anyChar)
        |> andThen
            (\chars ->
                case String.toInt chars of
                    Just n -> succeed n
                    Nothing -> fail "better use Parser.Common.int"
            )
        |> parse "123"
    --> Ok 123

    textOf (oneOrMore anyChar)
        |> andThen
            (\chars ->
                case String.toInt chars of
                    Just n -> succeed n
                    Nothing -> fail "better use Parser.Common.int"
            )
        |> parse "abc"
        |> Result.mapError .message
    --> Err "better use Parser.Common.int"

-}
andThen : (a -> Parser b) -> Parser a -> Parser b
andThen f parser state =
    parser state
        |> Result.andThen (\( value, newState ) -> f value newState)


{-| Parse two values `andThen2` do something with those values,
which results in another parser.

This can be used to validate the last things we parsed,
or transform the last values,
or to ignore eiher of the values,
or to use the last values for the next parser like a backreference.

    andThen2
        (\_ chars ->
            case String.toInt chars of
                Just n -> succeed n
                Nothing -> fail "failed to parse a number"
        )
        (char '=')
        digits
        |> parse "=123"
    --> Ok 123

    andThen2
        (\_ chars ->
            case String.toInt chars of
                Just n -> succeed n
                Nothing -> fail "failed to parse a number"
        )
        (char '=')
        digits
        |> parse "_123"
        |> Result.mapError .message
    --> Err "expected the character '=', but got '_' instead"

-}
andThen2 : (a -> b -> Parser c) -> Parser a -> Parser b -> Parser c
andThen2 f parserA parserB =
    andThen
        (\a ->
            andThen (\b -> f a b)
                parserB
        )
        parserA


{-| If the previous parser failed, try a fallback parser.

    letters
        |> orElse digits
        |> parse "abc"
    --> Ok "abc"

    letters
        |> orElse digits
        |> parse "123"
    --> Ok "123"

    letters
        |> orElse digits
        |> parse "_"
        |> Result.mapError .message
    --> Err "expected a digit [0-9], but got '_' instead"

-}
orElse : Parser a -> Parser a -> Parser a
orElse fallback parser state =
    case parser state of
        Ok result ->
            Ok result

        Err _ ->
            fallback state


{-| If there is an error, this replaces the error message.
This helps create more descriptive error messages instead of the more generic
ones.

If you want to use the value that failed some validation in the error message,
consider using a more relaxed parser and using [`andThen`](#andThen) to do the
validation.
It's a little longer, but that way you get access to the potentially invalid
parsed value.

    letters
        |> withError "a name can only consist of letters"
        |> parse "123"
        |> Result.mapError .message
    --> Err "a name can only consist of letters"

    -- Alternatively, if you want even better error messages.
    textOf (oneOrMore anyChar)
        |> andThen
            (\txt ->
                if String.all Char.isAlpha txt then
                    succeed txt
                else
                    fail
                        ( "a name can only consist of letters, got '"
                        ++ txt
                        ++ "'"
                        )
            )
        |> parse "123"
        |> Result.mapError .message
    --> Err "a name can only consist of letters, got '123'"

-}
withError : String -> Parser a -> Parser a
withError message parser state =
    parser state
        |> Result.mapError
            (\e -> { e | message = message })



---=== MAPPING ===---


{-| Transform the result of a parser.

    letters
        |> map String.toLower
        |> parse "ABC"
    --> Ok "abc"

-}
map : (a -> b) -> Parser a -> Parser b
map f =
    andThen (\a -> succeed (f a))


{-| Transform the result of two parsers.

    import Parser.Common exposing (int)

    map2
        (\op x ->
            if op == '-' then
                -x
            else
                x
        )
        (oneOf [char '+', char '-'])
        int
        |> parse "-123"
    --> Ok -123

-}
map2 : (a -> b -> c) -> Parser a -> Parser b -> Parser c
map2 f parserA parserB =
    andThen
        (\a ->
            map (\b -> f a b)
                parserB
        )
        parserA


{-| Transform the result of three parsers.

    import Parser.Common exposing (int)

    map3
        (\x op y ->
            if op == '+' then
                x + y
            else
                x - y
        )
        int
        (oneOf [char '+', char '-'])
        int
        |> parse "5+2"
    --> Ok 7

-}
map3 : (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
map3 f parserA parserB parserC =
    andThen
        (\a ->
            map2 (\b c -> f a b c)
                parserB
                parserC
        )
        parserA


{-| Transform the result of four parsers.
-}
map4 : (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
map4 f parserA parserB parserC parserD =
    andThen
        (\a ->
            map3 (\b c d -> f a b c d)
                parserB
                parserC
                parserD
        )
        parserA


{-| Transform the result of five parsers.
-}
map5 : (a -> b -> c -> d -> e -> f) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f
map5 f parserA parserB parserC parserD parserE =
    andThen
        (\a ->
            map4 (\b c d e -> f a b c d e)
                parserB
                parserC
                parserD
                parserE
        )
        parserA


{-| Transform the result of a list of parsers.
Note that all parsers must be of the same type, unlike `map2`..`map5`.

    mapList (String.join ",")
        [ text "ab"
        , text "cd"
        , letters
        ]
        |> parse "abcdefg"
    --> Ok "ab,cd,efg"

-}
mapList : (List a -> b) -> List (Parser a) -> Parser b
mapList f parsers =
    map f (sequence parsers)
