module Parser.Sequence exposing (sequence, concat, maybe, zeroOrOne, zeroOrMore, oneOrMore, exactly, atLeast, atMost, between, until, fold, split, splitIncluding)

{-| Parsers involving sequences of matches.


# Sequences

@docs sequence, concat, maybe, zeroOrOne, zeroOrMore, oneOrMore, exactly, atLeast, atMost, between, until, fold, split, splitIncluding

-}

import Parser exposing (Parser, andThen, andThen2, andThenIgnore, andThenKeep, map, map2, orElse, succeed, textOf)
import Parser.Char exposing (anyChar)
import Parser.Check exposing (followedBy, notFollowedBy)


{-| Matches a sequence of parsers in order, and gets the result as a `List`.

    import Parser exposing (parse)
    import Parser.Char exposing (char, digit, letter)

    -- Note that all the parsers must be of the same type, like `Parser Char`.
    sequence [ char '_', letter, digit ]
        |> parse "_A5" --> Ok [ '_', 'A', '5' ]

-}
sequence : List (Parser a) -> Parser (List a)
sequence parsers =
    \state ->
        List.foldl
            (\parser ->
                Result.andThen
                    (\( results, nextState ) ->
                        parser nextState
                            |> Result.map
                                (Tuple.mapFirst (\result -> results ++ [ result ]))
                    )
            )
            (Ok ( [], state ))
            parsers


{-| Concatenates the parsed values from all the parsers into a single list with
all the values.

    import Parser exposing (parse)
    import Parser.Char exposing (char, digit)

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
    map List.concat (sequence parsers)


{-| Matches an optional value and returns it as a `Maybe`.

> ℹ️ Equivalent regular expression: `?`

    import Parser exposing (parse)
    import Parser.Char exposing (letter)

    -- Maybe we get `Just` a letter.
    parse "abc" (maybe letter) --> Ok (Just 'a')

    -- Or maybe we get `Nothing`.
    parse "123abc" (maybe letter) --> Ok Nothing

-}
maybe : Parser a -> Parser (Maybe a)
maybe parser =
    map Just parser
        |> orElse (succeed Nothing)


{-| Matches an optional value and returns it as a `List`.

> ℹ️ Equivalent regular expression: `?`

    import Parser exposing (parse)
    import Parser.Char exposing (letter)

    -- We want one letter, optionally.
    parse "abc" (zeroOrOne letter) --> Ok [ 'a' ]

    -- If we don't get any, that's still okay.
    parse "123abc" (zeroOrOne letter) --> Ok []

-}
zeroOrOne : Parser a -> Parser (List a)
zeroOrOne parser =
    map (\x -> [ x ]) parser
        |> orElse (succeed [])


{-| Matches zero or more values and returns them as a `List`.

> ℹ️ Equivalent regular expression: `*`

    import Parser exposing (parse)
    import Parser.Char exposing (letter)

    -- We want as many letters as there are.
    parse "abc" (zeroOrMore letter) --> Ok [ 'a', 'b', 'c' ]
    parse "abc123" (zeroOrMore letter) --> Ok [ 'a', 'b', 'c' ]

    -- Even zero letters is okay.
    parse "123abc" (zeroOrMore letter) --> Ok []

-}
zeroOrMore : Parser a -> Parser (List a)
zeroOrMore =
    fold (\xs x -> xs ++ [ x ]) []


{-| Matches one or more values and returns them as a `List`.

> ℹ️ Equivalent regular expression: `+`

    import Parser exposing (parse)
    import Parser.Char exposing (letter)
    import Parser.Error

    -- We want as many letters as there are.
    parse "abc" (oneOrMore letter) --> Ok [ 'a', 'b', 'c' ]
    parse "abc123" (oneOrMore letter) --> Ok [ 'a', 'b', 'c' ]

    -- But we want at least one.
    oneOrMore letter
        |> parse "123abc"
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '1'."

-}
oneOrMore : Parser a -> Parser (List a)
oneOrMore parser =
    atLeast 1 parser


{-| Matches a value `exactly` a number of times and returns them as a `List`.

> ℹ️ Equivalent regular expression: `{n}`

    import Parser exposing (parse)
    import Parser.Char exposing (letter)
    import Parser.Error

    -- We want `exactly` three letters.
    parse "abcdef" (exactly 3 letter) --> Ok [ 'a', 'b', 'c' ]

    -- Not two or four, we want three.
    exactly 3 letter
        |> parse "ab_def"
        |> Result.mapError Parser.Error.message
    --> Err "1:3: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '_'."

-}
exactly : Int -> Parser a -> Parser (List a)
exactly n parser =
    sequence (List.repeat n parser)


{-| Matches a value at least a number of times and returns them as a `List`.

> ℹ️ Equivalent regular expression: `{min,}`

    import Parser exposing (parse)
    import Parser.Char exposing (letter)
    import Parser.Error

    -- We want at least three letters, we are okay with more than three.
    parse "abcdef" (atLeast 3 letter) --> Ok [ 'a', 'b', 'c', 'd', 'e', 'f' ]

    -- But not two, that's sacrilegious.
    atLeast 3 letter
        |> parse "ab_def"
        |> Result.mapError Parser.Error.message
    --> Err "1:3: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '_'."

-}
atLeast : Int -> Parser a -> Parser (List a)
atLeast min parser =
    map2 (++)
        (exactly min parser)
        (zeroOrMore parser)


{-| Matches a value at most a number of times and returns them as a `List`.

> ℹ️ Equivalent regular expression: `{0,max}`

    import Parser exposing (parse)
    import Parser.Char exposing (letter)

    -- We want a maximum of three letters.
    parse "abcdef" (atMost 3 letter) --> Ok [ 'a', 'b', 'c' ]

    -- Less than that is also okay.
    parse "ab_def" (atMost 3 letter) --> Ok [ 'a', 'b' ]

    -- Even zero letters are fine.
    parse "_bcdef" (atMost 3 letter) --> Ok []

-}
atMost : Int -> Parser a -> Parser (List a)
atMost max =
    fold
        (\xs x ->
            if List.length xs < max then
                xs ++ [ x ]

            else
                xs
        )
        []


{-| Matches a value between a range of times and returns them as a `List`.

> ℹ️ Equivalent regular expression: `{min,max}`

    import Parser exposing (parse)
    import Parser.Char exposing (letter)
    import Parser.Error

    -- We want between two and four letters.
    parse "abcdef" (between 2 4 letter) --> Ok [ 'a', 'b', 'c', 'd' ]
    parse "abc_ef" (between 2 4 letter) --> Ok [ 'a', 'b', 'c' ]
    parse "ab_def" (between 2 4 letter) --> Ok [ 'a', 'b' ]

    -- But less than that is not cool.
    between 2 3 letter
        |> parse "a_cdef"
        |> Result.mapError Parser.Error.message
    --> Err "1:2: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '_'."

-}
between : Int -> Int -> Parser a -> Parser (List a)
between min max parser =
    map2 (++)
        (exactly min parser)
        (atMost (max - min) parser)


{-| Matches a values repeatedly until a delimiter parser matches.
The delimiter marks the end of the sequence, and it is _not_ consumed.

    import Parser exposing (drop, parse, succeed, take, textOf)
    import Parser.Char exposing (char, letter)
    import Parser.Check exposing (end)
    import Parser.Error

    -- Get all the letters until we find 'd'.
    letter
        |> until (char 'd')
        |> parse "abcdef"
    --> Ok [ 'a', 'b', 'c' ]

    -- Or until the parser stops matching
    letter
        |> until (char 'd')
        |> parse "abc123"
    --> Ok [ 'a', 'b', 'c' ]

    -- The delimiter is _not_ consumed.
    succeed identity
        |> drop (char '<')
        |> take (textOf (letter |> until (char '>')))
        |> drop (char '>')
        |> drop end
        |> parse "<abc>"
    --> Ok "abc"

-}
until : Parser delimiter -> Parser a -> Parser (List a)
until delimiter parser =
    succeed ()
        |> notFollowedBy delimiter
        |> andThenKeep parser
        |> fold (\xs x -> xs ++ [ x ]) []


{-| Reduces matches of a parser.

    import Parser exposing (parse)
    import Parser.Common exposing (letters, number, token)

    parse "2 3 4" (fold (+) 1 (token number)) --> Ok 10
    parse "b c d" (fold (++) "a" (token letters)) --> Ok "abcd"

-}
fold : (b -> a -> b) -> b -> Parser a -> Parser b
fold f first parser =
    parser
        |> andThen (\x -> fold f (f first x) parser)
        |> orElse (succeed first)


{-| Splits the input text by a _separator_ parser into a `List` of `String`s.
The separators cannot overlap, and are discarded after being matched.

    import Parser exposing (parse)
    import Parser.Char exposing (char)

    -- Split Comma-Separated-Values (CSV) into a `List` of `String`s.
    split (char ',')
        |> parse "a,bc,def"
    --> Ok [ "a", "bc", "def" ]

    -- Leading/trailing separators are valid and give empty values.
    split (char ',')
        |> parse ",a,,"
    --> Ok [ "", "a", "", "" ]

    -- An empty input text gives a single empty string element.
    split (char ',')
        |> parse ""
    --> Ok [ "" ]

-}
split : Parser separator -> Parser (List String)
split separator =
    map2 (++)
        -- Zero or more values pairs delimited by the separator.
        (textOf (anyChar |> until separator)
            |> andThenIgnore separator
            |> zeroOrMore
        )
        -- Last value with whatever is left.
        (textOf (zeroOrMore anyChar)
            |> map (\x -> [ x ])
        )


{-| Splits the input text by a _separator_ parser into a `List` of `String`s.
The separators cannot overlap,
and are interleaved alongside the values in the order found.

    import Parser exposing (map, parse)
    import Parser.Common exposing (text)

    type Token
        = Separator
        | Value String

    -- Note that both values and separators must be of the same type.
    splitIncluding (text "," |> map (\_ -> Separator)) Value
        |> parse "a,bc,def"
    --> Ok [ Value "a", Separator, Value "bc", Separator, Value "def" ]

    -- Leading/trailing separators are valid and give empty values.
    splitIncluding (text "," |> map (\_ -> Separator)) Value
        |> parse ",a,,"
    --> Ok [ Value "", Separator, Value "a", Separator, Value "", Separator, Value "" ]

    -- An empty input text gives a single element from an empty string.
    splitIncluding (text "," |> map (\_ -> Separator)) Value
        |> parse ""
    --> Ok [ Value "" ]

-}
splitIncluding : Parser a -> (String -> a) -> Parser (List a)
splitIncluding separator f =
    map2 (++)
        -- Zero or more value-separator pairs.
        (textOf (anyChar |> until separator)
            |> andThen (\value -> map (\sep -> [ f value, sep ]) separator)
            |> zeroOrMore
            |> map List.concat
        )
        -- Last value with whatever is left.
        (textOf (zeroOrMore anyChar)
            |> map (\lastValue -> [ f lastValue ])
        )
