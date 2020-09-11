module Parser exposing
    ( Parser, Error, parse, into, take, drop
    , anyChar, char, charNoCase, digit, digits, letter, letters, lowercase, uppercase, alphaNum, space, spaces
    , text, textNoCase, textOf, line
    , sequence, concat, oneOf, maybe, zeroOrOne, zeroOrMore, oneOrMore, exactly, atLeast, atMost, between, until, untilIncluding, while
    , succeed, expected, andThen, andThen2, andThenIgnore, orElse, orEnd, expecting, end, followedBy, notFollowedBy
    , map, map2, map3, map4, map5, mapList
    )

{-| Intuitive and easy to use parser library.


# Basic usage

@docs Parser, Error, parse, into, take, drop


# Matching characters

@docs anyChar, char, charNoCase, digit, digits, letter, letters, lowercase, uppercase, alphaNum, space, spaces


# Text operations

@docs text, textNoCase, textOf, line


# List operations

@docs sequence, concat, oneOf, maybe, zeroOrOne, zeroOrMore, oneOrMore, exactly, atLeast, atMost, between, until, untilIncluding, while


# Chaining

@docs succeed, expected, andThen, andThen2, andThenIgnore, orElse, orEnd, expecting, end, followedBy, notFollowedBy


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
    { expected : String
    , lastChar : Maybe Char
    , input : String
    , row : Int
    , col : Int
    , context : List Context
    }


type alias State =
    { input : String
    , remaining : String
    , lastChar : Maybe Char
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
    import Parser.Error

    letter
        |> parse "123"
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting a letter [a-zA-Z].\nI got stuck when I got the character '1'."

-}
parse : String -> Parser a -> Result Error a
parse input parser =
    parser
        { input = input
        , remaining = input
        , lastChar = Nothing
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
    import Parser.Error

    anyChar
        |> parse ""
        |> Result.mapError Parser.Error.message
    --> Err "1:0: I was expecting a character.\nI reached the end of the input text."

-}
anyChar : Parser Char
anyChar state =
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

    -- Match a specific character, case sensitive.
    parse "abc" (char 'a') --> Ok 'a'

    -- It fails if it's not _exactly_ the same.
    import Parser.Error

    char 'a'
        |> parse "ABC"
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting the character 'a'.\nI got stuck when I got the character 'A'."

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

    -- Match a specific character, case insensitive.
    parse "abc" (charNoCase 'a') --> Ok 'a'
    parse "ABC" (charNoCase 'a') --> Ok 'A'

    -- But anything else makes it fail.
    import Parser.Error

    charNoCase 'a'
        |> parse "123"
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting the character 'a' (case insensitive).\nI got stuck when I got the character '1'."

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

    -- Match a digit.
    parse "123" digit --> Ok '1'
    parse "3.14" digit --> Ok '3'

    -- But anything else makes it fail.
    import Parser.Error

    digit
        |> parse "abc"
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting a digit [0-9].\nI got stuck when I got the character 'a'."

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


{-| Matches one or more digit.

> ℹ️ Equivalent regular expression: `[0-9]+` or `\d+`

    -- Match many digits.
    parse "123abc" digits --> Ok "123"

    -- But anything else makes it fail.
    import Parser.Error

    digits
        |> parse "abc123"
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting one or more digits [0-9]+.\nI got stuck when I got the character 'a'."

-}
digits : Parser String
digits =
    textOf (oneOrMore digit)
        |> expecting "one or more digits [0-9]+"


{-| Matches exactly one letter character.
This is case insensitive.

> ℹ️ Equivalent regular expression: `[a-zA-Z]`

    -- Match any letter, case insensitive.
    parse "abc" letter --> Ok 'a'
    parse "ABC" letter --> Ok 'A'

    -- But anything else makes it fail.
    import Parser.Error

    letter
        |> parse "123"
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting a letter [a-zA-Z].\nI got stuck when I got the character '1'."

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


{-| Matches one or more letter.
This is case insensitive.

> ℹ️ Equivalent regular expression: `[a-zA-Z]+`

    -- Match many letters, case insensitive.
    parse "abc123" letters --> Ok "abc"
    parse "ABC123" letters --> Ok "ABC"

    -- But anything else makes it fail.
    import Parser.Error

    letters
        |> parse "123abc"
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting one or more letters [a-zA-Z]+.\nI got stuck when I got the character '1'."

-}
letters : Parser String
letters =
    textOf (oneOrMore letter)
        |> expecting "one or more letters [a-zA-Z]+"


{-| Matches exactly one lowercase letter character.
This is case sensitive.

> ℹ️ Equivalent regular expression: `[a-z]`

    -- Match a lowercase letter.
    parse "abc" lowercase --> Ok 'a'

    -- But anything else makes it fail.
    import Parser.Error

    lowercase
        |> parse "ABC"
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting a lowercase letter [a-z].\nI got stuck when I got the character 'A'."

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

    -- Match an uppercase letter.
    parse "ABC" uppercase --> Ok 'A'

    -- But anything else makes it fail.
    import Parser.Error

    uppercase
        |> parse "abc"
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting an uppercase letter [A-Z].\nI got stuck when I got the character 'a'."

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

    -- Match a letter or number.
    parse "abc" alphaNum --> Ok 'a'
    parse "ABC" alphaNum --> Ok 'A'
    parse "123" alphaNum --> Ok '1'

    -- But anything else makes it fail.
    import Parser.Error

    alphaNum
        |> parse "_abc"
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting a letter or a digit [a-zA-Z0-9].\nI got stuck when I got the character '_'."

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
    import Parser.Error

    space
        |> parse "abc"
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting a blank space.\nI got stuck when I got the character 'a'."

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
        |> expecting "a blank space"


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
    import Parser.Error

    text "abc"
        |> parse "abCDEF"
        |> Result.mapError Parser.Error.message
    --> Err "1:3: I was expecting the text 'abc'.\nI got stuck when I got the character 'C'."

-}
text : String -> Parser String
text str =
    textOf (sequence (List.map char (String.toList str)))
        |> expecting ("the text '" ++ str ++ "'")


{-| Matches a specific text string.
This is case insensitive.

    -- Match an exact text, case insensitive.
    parse "abcdef" (textNoCase "abc") --> Ok "abc"
    parse "ABCDEF" (textNoCase "abc") --> Ok "ABC"

    -- But anything else makes it fail.
    import Parser.Error

    textNoCase "abc"
        |> parse "ab@"
        |> Result.mapError Parser.Error.message
    --> Err "1:3: I was expecting the text 'abc' (case insensitive).\nI got stuck when I got the character '@'."

-}
textNoCase : String -> Parser String
textNoCase str =
    textOf (sequence (List.map charNoCase (String.toList str)))
        |> expecting ("the text '" ++ str ++ "' (case insensitive)")


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
To give a more descriptive error message, you can use [`expecting`](#expecting).

> ℹ️ It's a good idea to use [`expecting`](#expecting) alongside this function
> to improve the error messages.

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
    import Parser.Error

    oneOf [ char '_', letter ]
        |> parse "123"
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting a letter [a-zA-Z].\nI got stuck when I got the character '1'."

-}
oneOf : List (Parser a) -> Parser a
oneOf parsers =
    List.foldl orElse (expected "") parsers


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
    import Parser.Error

    oneOrMore letter
        |> parse "_abc"
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting a letter [a-zA-Z].\nI got stuck when I got the character '_'."

-}
oneOrMore : Parser a -> Parser (List a)
oneOrMore parser =
    atLeast 1 parser


{-| Parses a value exactly a number of times and returns them as a `List`.

> ℹ️ Equivalent regular expression: `{n}`

    -- We want `exactly` three letters.
    parse "abcdef" (exactly 3 letter) --> Ok [ 'a', 'b', 'c' ]

    -- Not two or four, we want three.
    import Parser.Error

    exactly 3 letter
        |> parse "ab_def"
        |> Result.mapError Parser.Error.message
    --> Err "1:3: I was expecting a letter [a-zA-Z].\nI got stuck when I got the character '_'."

-}
exactly : Int -> Parser a -> Parser (List a)
exactly n parser =
    sequence (List.repeat n parser)


{-| Parses a value at least a number of times and returns them as a `List`.

> ℹ️ Equivalent regular expression: `{min,}`

    -- We want at least three letters, we are okay with more than three.
    parse "abcdef" (atLeast 3 letter) --> Ok [ 'a', 'b', 'c', 'd', 'e', 'f' ]

    -- But not two, that's sacrilegious.
    import Parser.Error

    atLeast 3 letter
        |> parse "ab_def"
        |> Result.mapError Parser.Error.message
    --> Err "1:3: I was expecting a letter [a-zA-Z].\nI got stuck when I got the character '_'."

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
    import Parser.Error

    between 2 3 letter
        |> parse "a_cdef"
        |> Result.mapError Parser.Error.message
    --> Err "1:2: I was expecting a letter [a-zA-Z].\nI got stuck when I got the character '_'."

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

    -- The delimiter is _not_ consumed.
    succeed (\str -> str)
        |> drop (char '<')
        |> take (textOf (letter |> until (char '>')))
        |> drop (char '>')
        |> end
        |> parse "<abc>"
    --> Ok "abc"

    -- If the delimiter is not found, we get an error.
    import Parser.Error

    letter
        |> until (char 'd')
        |> parse "abc123"
        |> Result.mapError Parser.Error.message
    --> Err "1:4: I was expecting a letter [a-zA-Z].\nI got stuck when I got the character '1'."

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

    -- The delimiter is consumed.
    succeed (\str -> str)
        |> drop (char '<')
        |> take (textOf (letter |> untilIncluding (char '>') |> map Tuple.first))
        |> end
        |> parse "<abc>"
    --> Ok "abc"

    -- If the delimiter is not found, we get an error.
    import Parser.Error

    letter
        |> untilIncluding (char 'd')
        |> parse "abc123"
        |> Result.mapError Parser.Error.message
    --> Err "1:4: I was expecting a letter [a-zA-Z].\nI got stuck when I got the character '1'."

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

    -- Always fails with an error message.
    import Parser.Error

    expected "nothing, this always fails"
        |> parse ""
        |> Result.mapError Parser.Error.message
    --> Err "1:0: I was expecting nothing, this always fails.\nI reached the end of the input text."

-}
expected : String -> Parser a
expected description state =
    Err
        { expected = description
        , lastChar = state.lastChar
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
    import Parser.Error

    textOf (oneOrMore anyChar)
        |> andThen
            (\chars ->
                case String.toInt chars of
                    Just n -> succeed n
                    Nothing -> expected "an integer, better use Parser.Common.int"
            )
        |> parse "123"
    --> Ok 123

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
                Nothing -> expected "an integer, better use Parser.Common.int"
        )
        (char '=')
        digits
        |> parse "=123"
    --> Ok 123

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
    import Parser.Error

    letters
        |> orElse digits
        |> parse "_"
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting one or more digits [0-9]+.\nI got stuck when I got the character '_'."

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
    import Parser.Error

    letters
        |> orEnd
        |> parse "123abc"
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting one or more letters [a-zA-Z]+.\nI got stuck when I got the character '1'."

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
    import Parser.Error

    letters
        |> expecting "a name consisting of letters"
        |> parse "123"
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting a name consisting of letters.\nI got stuck when I got the character '1'."

-}
expecting : String -> Parser a -> Parser a
expecting description parser state =
    parser state
        |> Result.mapError
            (\e -> { e | expected = description })


{-| Succeeds only if there are no more remaining characters in the input text.
This does not consume any inputs.

> ℹ️ Equivalent regular expression: `$`

    -- Get some letters, and there are better no input characters left.
    letters
        |> end
        |> parse "abc"
    --> Ok "abc"

    -- Or fail otherwise.
    import Parser.Error

    letters
        |> end
        |> parse "abc123"
        |> Result.mapError Parser.Error.message
    --> Err "1:4: I was expecting the end of the input text, but 3 characters are still remaining.\nI got stuck when I got the character '1'."

-}
end : Parser a -> Parser a
end parser initialState =
    parser initialState
        |> Result.andThen
            (\( value, state ) ->
                if String.isEmpty state.remaining then
                    succeed value state

                else
                    anyChar state
                        |> Result.andThen
                            (\( _, nextState ) ->
                                expected
                                    ("the end of the input text, but "
                                        ++ String.fromInt (String.length state.remaining)
                                        ++ " characters are still remaining"
                                    )
                                    nextState
                            )
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
    import Parser.Error

    letters
        |> followedBy digit
        |> parse "abc@def"
        |> Result.mapError Parser.Error.message
    --> Err "1:4: I was expecting a digit [0-9].\nI got stuck when I got the character '@'."

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

> ℹ️ It's a good idea to use [`expecting`](#expecting) alongside this function
> to improve the error messages.

> ℹ️ Equivalent regular expression: `(?!...)` _(negative lookahead)_

    -- Match letters only if it's `notFollowedBy` a digit.
    letters
        |> notFollowedBy digit
        |> expecting "letters not followed by a number"
        |> parse "abc@def"
    --> Ok "abc"

    -- Even if we match the letters, fail if the next character is a digit.
    import Parser.Error

    -- This is the default error message, but you can use `expecting` to improve it.
    letters
        |> notFollowedBy digit
        |> parse "abc123"
        |> Result.mapError Parser.Error.message
    --> Err "1:4: I was expecting to not match a pattern, but I did.\nI got stuck when I got the character '1'."

-}
notFollowedBy : Parser lookahead -> Parser a -> Parser a
notFollowedBy lookahead parser initialState =
    parser initialState
        |> Result.andThen
            (\( value, state ) ->
                case lookahead state of
                    Ok ( _, lookaheadState ) ->
                        expected "to not match a pattern, but I did" lookaheadState

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
