module Parser exposing
    ( Parser, Error, parse, into, take, drop
    , anyChar, char, charNoCase, digit, digits, letter, letters, lowercase, uppercase, alphanumeric, space, spaces
    , text, textNoCase, textOf, line
    , sequence, concat, oneOf, maybe, zeroOrOne, zeroOrMore, oneOrMore, exactly, atLeast, atMost, between, until, untilIncluding, while
    , succeed, fail, andThen, andThen2, andThenIgnore, orElse, orEnd, withError, end, followedBy, notFollowedBy
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

@docs sequence, concat, oneOf, maybe, zeroOrOne, zeroOrMore, oneOrMore, exactly, atLeast, atMost, between, until, untilIncluding, while


# Chaining

@docs succeed, fail, andThen, andThen2, andThenIgnore, orElse, orEnd, withError, end, followedBy, notFollowedBy


# Mapping

@docs map, map2, map3, map4, map5, mapList

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

    -- Consumes a single letter, then "bc" are still remaining.
    parse "abc" letter --> Ok 'a'

    -- We get an error message if the parser doesn't match.
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

    -- We can use `into` to have more context when an error happens.
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

    -- We can get the error context stack as well as where they started matching.
    parse "(a,b)" point
        |> Result.mapError .context
    --> Err [{ name = "Point", row = 1, col = 1 }]

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

    -- We can match any character.
    parse "abc" anyChar --> Ok 'a'
    parse "#hashtag" anyChar --> Ok '#'

    -- This can only fail if we run out of inputs.
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

    -- Match a specific character, case sensitive.
    parse "abc" (char 'a') --> Ok 'a'

    -- It fails if it's not _exactly_ the same.
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

    -- Match a specific character, case insensitive.
    parse "abc" (charNoCase 'a') --> Ok 'a'
    parse "ABC" (charNoCase 'a') --> Ok 'A'

    -- But anything else makes it fail.
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


{-| Matches exactly one digit character.

> ℹ️ Equivalent regular expression: `[0-9]` or `\d`

    -- Match a digit.
    parse "123" digit --> Ok '1'
    parse "3.14" digit --> Ok '3'

    -- But anything else makes it fail.
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

    -- Match many digits.
    parse "123abc" digits --> Ok "123"

    -- But anything else makes it fail.
    digits
        |> parse "abc123"
        |> Result.mapError .message
    --> Err "expected a digit [0-9], but got 'a' instead"

-}
digits : Parser String
digits =
    textOf (oneOrMore digit)


{-| Matches exactly one letter character.
This is case insensitive.

> ℹ️ Equivalent regular expression: `[a-zA-Z]`

    -- Match any letter, case insensitive.
    parse "abc" letter --> Ok 'a'
    parse "ABC" letter --> Ok 'A'

    -- But anything else makes it fail.
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

    -- Match many letters, case insensitive.
    parse "abc123" letters --> Ok "abc"
    parse "ABC123" letters --> Ok "ABC"

    -- But anything else makes it fail.
    letters
        |> parse "123abc"
        |> Result.mapError .message
    --> Err "expected a letter [a-zA-Z], but got '1' instead"

-}
letters : Parser String
letters =
    textOf (oneOrMore letter)


{-| Matches exactly one lowercase letter character.
This is case sensitive.

> ℹ️ Equivalent regular expression: `[a-z]`

    -- Match a lowercase letter.
    parse "abc" lowercase --> Ok 'a'

    -- But anything else makes it fail.
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


{-| Matches exactly one uppercase letter character.
This is case sensitive.

> ℹ️ Equivalent regular expression: `[A-Z]`

    -- Match an uppercase letter.
    parse "ABC" uppercase --> Ok 'A'

    -- But anything else makes it fail.
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


{-| Matches exactly one letter or digit character.
This is case insensitive.

> ℹ️ Equivalent regular expression: `[a-zA-Z0-9]`

    -- Match a letter or number.
    parse "abc" alphanumeric --> Ok 'a'
    parse "ABC" alphanumeric --> Ok 'A'
    parse "123" alphanumeric --> Ok '1'

    -- But anything else makes it fail.
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

    -- Match a blank space.
    parse "    abc" space --> Ok ' '
    parse "\t abc" space --> Ok '\t'

    -- But anything else makes it fail.
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

    -- Match many spaces.
    parse "    abc" spaces --> Ok "    "

    parse "\t abc" spaces --> Ok "\t "

    -- Including zero spaces :)
    parse "abc" spaces --> Ok ""

-}
spaces : Parser String
spaces =
    textOf (zeroOrMore space)



---=== TEXT OPERATIONS ===---


{-| Matches a specific text string.
This is case sensitive.

    -- Match an exact text, case sensitive.
    parse "abcdef" (text "abc") --> Ok "abc"

    -- But anything else makes it fail.
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

    -- Match an exact text, case insensitive.
    parse "abcdef" (textNoCase "abc") --> Ok "abc"
    parse "ABCDEF" (textNoCase "abc") --> Ok "ABC"

    -- But anything else makes it fail.
    textNoCase "abc"
        |> parse "123"
        |> Result.mapError .message
    --> Err "expected the character 'a' or 'A', but got '1' instead"

-}
textNoCase : String -> Parser String
textNoCase str =
    textOf (sequence (List.map charNoCase (String.toList str)))


{-| Gets a string from a list of characters.

    -- Get a `String` out of a `List Char`, we could have also used `letters` :)
    textOf (oneOrMore letter)
        |> parse "abc123" --> Ok "abc"

-}
textOf : Parser (List Char) -> Parser String
textOf =
    map String.fromList


{-| Gets a line from the input text, delimited by '\\n'.

    -- A line could be delimited by the newline character '\n'.
    parse "abc\ndef" line --> Ok "abc"

    -- Or this could also be the last line.
    parse "abc" line --> Ok "abc"

    -- An empty line still counts.
    parse "" line --> Ok ""

-}
line : Parser String
line =
    textOf (until (char '\n' |> orEnd) anyChar)



---=== LIST OPERATIONS ===---


{-| Matches a sequence of parsers in order, and gets the result as a `List`.

    -- Note that all the parsers must be of the same type, like `Parser Char`.
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

    -- We get all these parsers as a single sequence of values.
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

    -- Try the first parser.
    oneOf [ char '_', letter ]
        |> parse "_abc"
    --> Ok '_'

    -- If it doesn't work, try the next.
    oneOf [ char '_', letter ]
        |> parse "abc"
    --> Ok 'a'

    -- If none of them work, we get the error from the last parser.
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

    -- Maybe we get `Just` a letter.
    parse "abc" (maybe letter) --> Ok (Just 'a')

    -- Or maybe we get `Nothing`.
    parse "_abc" (maybe letter) --> Ok Nothing

-}
maybe : Parser a -> Parser (Maybe a)
maybe parser =
    map Just parser
        |> orElse (succeed Nothing)


{-| Parses an optional value and returns it as a `List`.

> ℹ️ Equivalent regular expression: `?`

    -- We want one letter, optionally.
    parse "abc" (zeroOrOne letter) --> Ok [ 'a' ]

    -- If we don't get any, that's still okay.
    parse "_abc" (zeroOrOne letter) --> Ok []

-}
zeroOrOne : Parser a -> Parser (List a)
zeroOrOne parser =
    map (\x -> [ x ]) parser
        |> orElse (succeed [])


{-| Parses zero or more values and returns them as a `List`.

> ℹ️ Equivalent regular expression: `*`

    -- We want as many letters as there are.
    parse "abc" (zeroOrMore letter) --> Ok [ 'a', 'b', 'c' ]

    -- Even zero letters is okay.
    parse "_abc" (zeroOrMore letter) --> Ok []

-}
zeroOrMore : Parser a -> Parser (List a)
zeroOrMore parser =
    while (\_ _ -> True) parser


{-| Parses one or more values and returns them as a `List`.

> ℹ️ Equivalent regular expression: `+`

    -- We want as many letters as there are.
    parse "abc" (oneOrMore letter) --> Ok [ 'a', 'b', 'c' ]

    -- But we want at least one.
    oneOrMore letter
        |> parse "_abc"
        |> Result.mapError .message
    --> Err "expected a letter [a-zA-Z], but got '_' instead"

-}
oneOrMore : Parser a -> Parser (List a)
oneOrMore parser =
    atLeast 1 parser


{-| Parses a value exactly a number of times and returns them as a `List`.

> ℹ️ Equivalent regular expression: `{n}`

    -- We want `exactly` three letters.
    parse "abcdef" (exactly 3 letter) --> Ok [ 'a', 'b', 'c' ]

    -- Not two or four, we want three.
    exactly 3 letter
        |> parse "ab_def"
        |> Result.mapError .message
    --> Err "expected a letter [a-zA-Z], but got '_' instead"

-}
exactly : Int -> Parser a -> Parser (List a)
exactly n parser =
    sequence (List.repeat n parser)


{-| Parses a value at least a number of times and returns them as a `List`.

> ℹ️ Equivalent regular expression: `{min,}`

    -- We want at least three letters, we are okay with more than three.
    parse "abcdef" (atLeast 3 letter) --> Ok [ 'a', 'b', 'c', 'd', 'e', 'f' ]

    -- But not two, that's sacrilegious.
    atLeast 3 letter
        |> parse "ab_def"
        |> Result.mapError .message
    --> Err "expected a letter [a-zA-Z], but got '_' instead"

-}
atLeast : Int -> Parser a -> Parser (List a)
atLeast min parser =
    map2 (++)
        (exactly min parser)
        (zeroOrMore parser)


{-| Parses a value at most a number of times and returns them as a `List`.

> ℹ️ Equivalent regular expression: `{0,max}`

    -- We want a maximum of three letters.
    parse "abcdef" (atMost 3 letter) --> Ok [ 'a', 'b', 'c' ]

    -- Less than that is also okay.
    parse "ab_def" (atMost 3 letter) --> Ok [ 'a', 'b' ]

    -- Even zero letters are fine.
    parse "_bcdef" (atMost 3 letter) --> Ok []

-}
atMost : Int -> Parser a -> Parser (List a)
atMost max parser =
    while (\xs _ -> List.length xs < max) parser


{-| Parses a value between a range of times and returns them as a `List`.

> ℹ️ Equivalent regular expression: `{min,max}`

    -- We want between two and four letters.
    parse "abcdef" (between 2 4 letter) --> Ok [ 'a', 'b', 'c', 'd' ]
    parse "abc_ef" (between 2 4 letter) --> Ok [ 'a', 'b', 'c' ]
    parse "ab_def" (between 2 4 letter) --> Ok [ 'a', 'b' ]

    -- But less than that is not cool.
    between 2 3 letter
        |> parse "a_cdef"
        |> Result.mapError .message
    --> Err "expected a letter [a-zA-Z], but got '_' instead"

-}
between : Int -> Int -> Parser a -> Parser (List a)
between min max parser =
    map2 (++)
        (exactly min parser)
        (atMost (max - min) parser)


{-| Parses a values repeatedly until a delimiter parser matches.
The delimiter marks the end of the sequence, and it is _not_ consumed.

    -- Get all the letters until we find 'd'.
    letter
        |> until (char 'd')
        |> parse "abcdef"
    --> Ok [ 'a', 'b', 'c' ]

    -- If the delimiter is not found, we get an error.
    letter
        |> until (char 'd')
        |> parse "abc123"
        |> Result.mapError .message
    --> Err "expected a letter [a-zA-Z], but got '1' instead"

    -- The delimiter is _not_ consumed.
    succeed (\str -> str)
        |> drop (char '<')
        |> take (textOf (letter |> until (char '>')))
        |> drop (char '>')
        |> end
        |> parse "<abc>"
    --> Ok "abc"

-}
until : Parser stop -> Parser a -> Parser (List a)
until delimiter parser =
    let
        until_ : List a -> Parser (List a)
        until_ values =
            succeed values
                |> followedBy delimiter
                |> orElse
                    (parser
                        |> andThen (\value -> until_ (values ++ [ value ]))
                    )
    in
    until_ []


{-| Parses a values repeatedly until a delimiter parser matches.
The delimiter marks the end of the sequence, and it is consumed.

    -- Gets all the letters until we find 'd'.
    letter
        |> untilIncluding (char 'd')
        |> parse "abcdef"
    --> Ok ([ 'a', 'b', 'c' ], 'd')

    -- If we don't care about the delimiter, we can extract only the values.
    letter
        |> untilIncluding (char 'd')
        |> map Tuple.first
        |> parse "abcdef"
    --> Ok [ 'a', 'b', 'c' ]

    -- If the delimiter is not found, we get an error.
    letter
        |> untilIncluding (char 'd')
        |> parse "abc123"
        |> Result.mapError .message
    --> Err "expected a letter [a-zA-Z], but got '1' instead"

    -- The delimiter is consumed.
    succeed (\str -> str)
        |> drop (char '<')
        |> take (textOf (letter |> untilIncluding (char '>') |> map Tuple.first))
        |> end
        |> parse "<abc>"
    --> Ok "abc"

-}
untilIncluding : Parser stop -> Parser a -> Parser ( List a, stop )
untilIncluding delimiter parser =
    let
        until_ : List a -> Parser ( List a, stop )
        until_ values =
            delimiter
                |> map (\delim -> ( values, delim ))
                |> orElse
                    (parser
                        |> andThen (\value -> until_ (values ++ [ value ]))
                    )
    in
    until_ []


{-| Parses values while a condition is still `True`.
Once the condition is `False`,
the sequence stops and the current value is not consumed.

If the condition never gets to `False`, it parses until it stops matching
or until the end of the input text.

The condition function takes two inputs:
a list of all the previously matched values,
and the current value matched.

> ℹ️ This is a low level function, for most cases you can probably find a higher
> level function that is more readable and easier to use.

    -- Get letters `while` the current letter is not 'd'.
    -- In other words, stop until we find a 'd'.
    letter
        |> while (\_ value -> value /= 'd')
        |> parse "abcdef"
    --> Ok [ 'a', 'b', 'c' ]

    -- Get letters `while` we've got less than 3 letters.
    -- In other words, stop until we have three letters.
    letter
        |> while (\values _ -> List.length values < 3)
        |> parse "abcdef"
    --> Ok [ 'a', 'b', 'c' ]

    -- If we stop matching, we still succeed with whatever we could get.
    letter
        |> while (\_ value -> value /= 'd')
        |> parse "abc123"
    --> Ok [ 'a', 'b', 'c' ]

    -- Once the condition fails, that input is _not_ consumed.
    succeed (\str -> str)
        |> drop (char '<')
        |> take (textOf (letter |> while (\_ value -> value /= '>')))
        |> drop (char '>')
        |> parse "<abc>"
    --> Ok "abc"

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

    -- Always succeed with "abc" no matter the input text.
    parse "" (succeed "abc") --> Ok "abc"

    -- This is usually used to start parser pipelines.
    import Parser.Common exposing (int)

    type alias Pair =
        { x : Int
        , y : Int
        }

    pair : Parser Pair
    pair =
        succeed Pair
            |> take int
            |> drop (char ',')
            |> take int

    parse "12,34" pair --> Ok { x = 12, y = 34 }

-}
succeed : a -> Parser a
succeed value state =
    Ok ( value, state )


{-| A parser that always fails with the given error message.

    -- Always fail with an error message.
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


{-| Parse one value `andThen` do something with that value,
which results in another parser.

This can be used to validate the last thing we parsed,
or transform the last value,
or to use the last value for the next parser like a backreference.

    -- Get some characters `andThen` interpret them as an `Int`.
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

    -- Get something like "=123" `andThen` interpret it as an `Int`.
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


{-| Parse and consume the next parser, but ignore its value.

This is useful when you want to match and advance the parser,
but keep the previous value.

    -- Let's parse a simple email, but we're only interested in the username.
    letters
        |> andThenIgnore (char '@')
        |> andThenIgnore letters
        |> andThenIgnore (text ".com")
        |> parse "user@example.com"
    --> Ok "user"

    -- To ignore previous values, you can use a regular andThen
    letters
        |> andThen (\_ -> char '@')
        |> andThen (\_ -> letters)
        |> andThenIgnore (text ".com")
        |> parse "user@example.com"
    --> Ok "example"

-}
andThenIgnore : Parser ignore -> Parser a -> Parser a
andThenIgnore ignore parser =
    andThen2 (\value _ -> succeed value) parser ignore


{-| If the previous parser failed, try a fallback parser.

    -- Try letters, `orElse` give me some digits.
    letters
        |> orElse digits
        |> parse "abc"
    --> Ok "abc"

    -- We didn't get letters, but we still got digits.
    letters
        |> orElse digits
        |> parse "123"
    --> Ok "123"

    -- But if we still fail, give the error message of the fallback parser.
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


{-| Succeeds either if the parser succeeds,
or if there are no more input characters.

    -- Get some letters.
    letters
        |> orEnd
        |> parse "abc123"
    --> Ok (Just "abc")

    -- If we get to the end of inputs, that's still okay.
    letters
        |> orEnd
        |> parse ""
    --> Ok Nothing

    -- But if we match something different, that's _not_ okay.
    letters
        |> orEnd
        |> parse "123abc"
        |> Result.mapError .message
    --> Err "expected a letter [a-zA-Z], but got '1' instead"

-}
orEnd : Parser a -> Parser (Maybe a)
orEnd parser =
    end (succeed Nothing)
        |> orElse (parser |> map Just)


{-| If there is an error, this replaces the error message.
This helps create more descriptive error messages instead of the more generic
ones.

If you want to use the value that failed some validation in the error message,
consider using a more relaxed parser and using [`andThen`](#andThen) to do the
validation.
It's a little longer, but that way you get access to the potentially invalid
parsed value.

    -- We can redefine an error message if something goes wrong.
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
                    fail ( "a name can only consist of letters, got '" ++ txt ++ "'" )
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


{-| Succeeds only if there are no more remaining characters in the input text.
This does not consume any inputs.

> ℹ️ Equivalent regular expression: `$`

    -- Get some letters, and there are better no input characters left.
    letters
        |> end
        |> parse "abc"
    --> Ok "abc"

    -- Or fail otherwise.
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


{-| Succeeds only if the input text is followed by a _lookahead_ parser.
This does not consume any inputs.

> ℹ️ Equivalent regular expression: `(?=...)` _(positive lookahead)_

    -- Match letters only if it's `followedBy` a digit.
    letters
        |> followedBy digit
        |> parse "abc123"
    --> Ok "abc"

    -- Even if we match the letters, fail if the next character is not a digit.
    letters
        |> followedBy digit
        |> parse "abc@def"
        |> Result.mapError .message
    --> Err "expected a digit [0-9], but got '@' instead"

-}
followedBy : Parser lookahead -> Parser a -> Parser a
followedBy lookahead parser initialState =
    parser initialState
        |> Result.andThen
            (\( value, state ) ->
                lookahead state
                    |> Result.map (\_ -> ( value, state ))
            )


{-| Succeeds only if the input text is _not_ followed by a _lookahead_ parser.
This does not consume any inputs.

> ℹ️ Equivalent regular expression: `(?!...)` _(negative lookahead)_

    -- Match letters only if it's `notFollowedBy` a digit.
    letters
        |> notFollowedBy digit
        |> parse "abc@def"
    --> Ok "abc"

    -- Even if we match the letters, fail if the next character is a digit.
    letters
        |> notFollowedBy digit
        |> parse "abc123"
        |> Result.mapError .message
    --> Err "the input text was followed by an unexpected pattern"

-}
notFollowedBy : Parser lookahead -> Parser a -> Parser a
notFollowedBy lookahead parser initialState =
    parser initialState
        |> Result.andThen
            (\( value, state ) ->
                case lookahead state of
                    Ok _ ->
                        fail "the input text was followed by an unexpected pattern" state

                    Err _ ->
                        Ok ( value, state )
            )



---=== MAPPING ===---


{-| Transform the result of a parser.

    -- Get some letters, and make them lowercase.
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

    -- Get an `Int` and apply an unary operator.
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

    -- Get an `Int` and apply a binary operator.
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

    -- Match a list of parsers, and then `map` a function to the results.
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
