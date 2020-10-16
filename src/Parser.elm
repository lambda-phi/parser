module Parser exposing
    ( Parser, Error, parse, into, take, drop, textOf, lazy
    , succeed, expected, oneOf, andThenKeep, andThenIgnore, andThen, andThen2, orElse, expecting
    , map, map2, map3, map4, map5
    )

{-| Intuitive and easy to use parser library.


# Basic usage

@docs Parser, Error, parse, into, take, drop, textOf, lazy


# Chaining

@docs succeed, expected, oneOf, andThenKeep, andThenIgnore, andThen, andThen2, orElse, expecting


# Mapping

@docs map, map2, map3, map4, map5

-}

-- TYPE DEFINITIONS


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



-- BASIC USAGE


{-| Parse an input text, and get either an [`Error`](#Error)
or the parsed value as a result.

    import Parser.Char exposing (letter)
    import Parser.Common exposing (number)
    import Parser.Error

    -- Consumes a single letter, then "bc" are still remaining.
    parse "abc" letter --> Ok 'a'

    -- We can also parse text into other data types like numbers.
    parse "3.14" number --> Ok 3.14

    -- We get an error message if the parser doesn't match.
    letter
        |> parse "123"
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '1'."

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

    import Parser.Char exposing (char)
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
into context parser =
    \state ->
        let
            ctx =
                { name = context
                , row = state.row
                , col = state.col
                }
        in
        parser { state | context = ctx :: state.context }
            |> Result.andThen
                (\( value, nextState ) ->
                    succeed value { nextState | context = state.context }
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


{-| Matches a string from a list of characters.

    import Parser.Char exposing (letter)
    import Parser.Sequence exposing (oneOrMore)

    -- Get a `String` out of a `List Char`, we could have also used `letters` :)
    textOf (oneOrMore letter)
        |> parse "abc123" --> Ok "abc"

-}
textOf : Parser (List Char) -> Parser String
textOf =
    map String.fromList


{-| Matches a parser _lazily_.
This allows to create self-referential parsers for recursive definitions.

    import Parser.Char exposing (anyChar)
    import Parser.Common exposing (text, token)

    type LazyList
        = End
        | Next Char LazyList

    lazyList : Parser LazyList
    lazyList =
        oneOf [ lazyListEnd, lazyListNext ]

    lazyListEnd : Parser LazyList
    lazyListEnd =
        succeed End
            |> drop (text "[]")

    lazyListNext : Parser LazyList
    lazyListNext =
        succeed Next
            |> take anyChar
            |> drop (token (text "::"))
            |> take (lazy (\_ -> lazyList))

    parse "[]" lazyList --> Ok End
    parse "a :: []" lazyList --> Ok (Next 'a' End)
    parse "a :: b :: []" lazyList --> Ok (Next 'a' (Next 'b' End))

> ℹ️ Without `lazy`, you would get an error like:
>
>     The `lazyList` definition is causing a very tricky infinite loop.
>
>     The `lazyList` value depends on itself through the following chain of
>     definitions:
>
>           ┌─────┐
>           │    lazyList
>           │     ↓
>           │    lazyListNext
>           └─────┘

-}
lazy : (() -> Parser a) -> Parser a
lazy f =
    \state -> f () state



-- CHAINING


{-| A parser that always succeeds with the given value.

    import Parser.Char exposing (char)
    import Parser.Common exposing (int)

    -- Always succeed with "abc" no matter the input text.
    parse "" (succeed "abc") --> Ok "abc"

    -- This is usually used to start parser pipelines.
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
succeed value =
    \state -> Ok ( value, state )


{-| A parser that always fails with the given error message.

    -- Always fails with an error message.
    import Parser.Error

    expected "nothing, this always fails"
        |> parse ""
        |> Result.mapError Parser.Error.message
    --> Err "1:0: I was expecting nothing, this always fails. I reached the end of the input text."

-}
expected : String -> Parser a
expected description =
    \state ->
        Err
            { expected = description
            , lastChar = state.lastChar
            , input = state.input
            , row = state.row
            , col = state.col - 1
            , context = state.context
            }


{-| Returns the value of the first parser that matches.
It tries to match the parsers in order.

If none of the parsers match, it keeps the error message from the last parser.

> ℹ️ It's a good idea to use [`expecting`](#expecting) alongside this function
> to improve the error messages.

> ℹ️ Equivalent regular expression: `|`

    import Parser.Char exposing (char, letter)
    import Parser.Error

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
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '1'."

-}
oneOf : List (Parser a) -> Parser a
oneOf parsers =
    List.foldl orElse (expected "a parser to match") parsers


{-| Parse and consume the next parser, and keep _only_ the previous value.

This is useful when you want to match and advance the parser,
but keep the previous value and ignore the new value.

    import Parser.Common exposing (letters, text)
    import Parser.Char exposing (char)

    -- Let's parse a simple email, but we're only interested in the username.
    letters
        |> andThenIgnore (char '@')
        |> andThenIgnore letters
        |> andThenIgnore (text ".com")
        |> parse "user@example.com"
    --> Ok "user"

-}
andThenIgnore : Parser ignore -> Parser a -> Parser a
andThenIgnore ignore parser =
    andThen2 (\value _ -> succeed value) parser ignore


{-| Parse and consume the next parser, and keep _only_ the new value.

This is useful when you want to match and advance the parser,
but ignore the previous value and keep the new one.

    import Parser.Common exposing (letters, text)
    import Parser.Char exposing (char)

    -- Let's parse a simple email, but we're only interested in the domain.
    letters
        |> andThenIgnore (char '@')
        |> andThenKeep letters
        |> andThenIgnore (text ".com")
        |> parse "user@example.com"
    --> Ok "example"

-}
andThenKeep : Parser keep -> Parser a -> Parser keep
andThenKeep keep parser =
    andThen (\_ -> keep) parser


{-| Parse one value `andThen` do something with that value,
which results in another parser.

This can be used to validate the last thing we parsed,
or transform the last value,
or to use the last value for the next parser like a backreference.

    import Parser.Char exposing (anyChar)
    import Parser.Error
    import Parser.Sequence exposing (oneOrMore)

    -- Get some characters `andThen` interpret them as an `Int`.
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
andThen f parser =
    \state ->
        parser state
            |> Result.andThen (\( value, nextState ) -> f value nextState)


{-| Parse two values `andThen2` do something with those values,
which results in another parser.

This can be used to validate the last things we parsed,
or transform the last values,
or to ignore eiher of the values,
or to use the last values for the next parser like a backreference.

    import Parser.Char exposing (char)
    import Parser.Common exposing (digits)

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


{-| If the previous parser failed, try a fallback parser.

    import Parser.Common exposing (digits, letters)
    import Parser.Error

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
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting one or more digits [0-9]+. I got stuck when I got the character '_'."

-}
orElse : Parser a -> Parser a -> Parser a
orElse fallback parser =
    \state ->
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

    import Parser.Common exposing (letters)
    import Parser.Error

    -- We can redefine an error message if something goes wrong.
    letters
        |> expecting "a name consisting of letters"
        |> parse "123"
        |> Result.mapError Parser.Error.message
    --> Err "1:1: I was expecting a name consisting of letters. I got stuck when I got the character '1'."

-}
expecting : String -> Parser a -> Parser a
expecting description parser =
    \state ->
        parser state
            |> Result.mapError
                (\e -> { e | expected = description })



-- MAPPING


{-| Transform the result of a parser.

    import Parser.Common exposing (letters)

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
    import Parser.Char exposing (char)

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

    import Parser.Char exposing (char)
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
