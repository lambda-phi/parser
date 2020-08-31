module Parser exposing
    ( Error
    , Parser
    , alphanumeric
    , andThen
    , anyChar
    , atLeast
    , atMost
    , between
    , char
    , charNoCase
    , digit
    , drop
    , end
    , fail
    , into
    , letter
    , lowercase
    , map
    , map2
    , map3
    , map4
    , map5
    , mapList
    , maybe
    , oneOf
    , oneOrMore
    , orElse
    , parse
    , repeat
    , sequence
    , space
    , spaces
    , stringOf
    , succeed
    , take
    , text
    , textNoCase
    , until
    , uppercase
    , while
    , withError
    , zeroOrMore
    )

import Result
import String



---=== TYPE DEFINITIONS ===---


type alias Parser a =
    State -> Result Error ( a, State )


type alias Error =
    { message : String
    , row : Int
    , col : Int
    , contextStack : List Context
    }


type alias State =
    { input : String
    , remaining : String
    , row : Int
    , col : Int
    , contextStack : List Context
    }


type alias Context =
    { context : String
    , row : Int
    , col : Int
    }



---=== BASIC USAGE ===---
--
-- PARSE


parse : String -> Parser a -> Result Error a
parse input parser =
    parser
        { input = input
        , remaining = input
        , row = 1
        , col = 1
        , contextStack = []
        }
        |> Result.map Tuple.first



-- INTO


into : String -> (a -> b) -> Parser (a -> b)
into context dataType =
    succeed dataType



-- GRAB


take : Parser a -> Parser (a -> b) -> Parser b
take next parser =
    andThen2 (\f x -> succeed (f x))
        parser
        next



-- IGNORE


drop : Parser a -> Parser b -> Parser b
drop next parser =
    andThen2 (\x _ -> succeed x)
        parser
        next



---=== MATCH TEXT ===---
--
-- ANY CHAR
-- regex: .


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



-- CHAR
-- regex: a


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



-- CHAR
-- regex: [aA]


charNoCase : Char -> Parser Char
charNoCase ch =
    andThen
        (\c ->
            if Char.toLower c == Char.toLower ch then
                succeed c

            else
                fail
                    ("expected the character '"
                        ++ String.fromChar ch
                        ++ "' (case insensitive), but got '"
                        ++ String.fromChar c
                        ++ "' instead"
                    )
        )
        anyChar



-- DIGIT
-- regex: [0-9]


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



-- LETTER
-- regex: [a-zA-Z]


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



-- LOWERCASE
-- regex: [a-z]


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



-- UPPERCASE
-- regex: [A-Z]


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



-- ALPHANUMERIC
-- regex: [a-zA-Z0-9]


alphanumeric : Parser Char
alphanumeric =
    andThen
        (\c ->
            if Char.isAlphaNum c then
                succeed c

            else
                fail
                    ("expected an uppercase letter [A-Z], but got '"
                        ++ String.fromChar c
                        ++ "' instead"
                    )
        )
        anyChar



-- space
-- regex: [ \t\n\r\f\v]


space : Parser Char
space =
    oneOf
        [ char ' '
        , char '\t'
        , char '\n'
        , char '\u{000D}' -- \r
        , char '\u{000C}' -- \f
        , char '\u{000B}' -- \v
        ]



-- spaces
-- regex: [ \t\n\r\f\v]*


spaces : Parser String
spaces =
    stringOf (zeroOrMore space)



-- END
-- regex: $


end : Parser ()
end state =
    if String.isEmpty state.remaining then
        succeed () state

    else
        fail "expected the end of the input text" state



---=== COMBINATIONS ===---
-- TEXT


text : String -> Parser String
text str =
    stringOf (sequence (List.map char (String.toList str)))



-- TEXT NO CASE


textNoCase : String -> Parser String
textNoCase str =
    stringOf (sequence (List.map charNoCase (String.toList str)))



-- SEQUENCE
-- regex: a[bc]d


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



-- TO STRING


stringOf : Parser (List Char) -> Parser String
stringOf =
    map String.fromList



-- ONE OF
-- a|b|c


oneOf : List (Parser a) -> Parser a
oneOf parsers =
    List.foldl orElse (fail "") parsers
        |> withError "expected one of the parsers to match"



-- MAYBE
-- regex: .?


maybe : Parser a -> Parser (Maybe a)
maybe parser =
    map Just parser
        |> orElse (succeed Nothing)



-- ZERO OR MORE
-- regex: .*


zeroOrMore : Parser a -> Parser (List a)
zeroOrMore parser =
    while (\_ _ -> True) parser



-- ONE OR MORE
-- regex: .+


oneOrMore : Parser a -> Parser (List a)
oneOrMore parser =
    map2 (::) parser (zeroOrMore parser)



-- REPEAT
-- regex: .{n}


repeat : Int -> Parser a -> Parser (List a)
repeat n parser =
    sequence (List.repeat n parser)



-- AT LEAST
-- regex: .{min,}


atLeast : Int -> Parser a -> Parser (List a)
atLeast min parser =
    map2 (++)
        (repeat min parser)
        (zeroOrMore parser)



-- AT MOST
-- regex: .{,max}


atMost : Int -> Parser a -> Parser (List a)
atMost max parser =
    while (\xs _ -> List.length xs < max) parser



-- BETWEEN
-- regex: .{min,max}


between : Int -> Int -> Parser a -> Parser (List a)
between min max parser =
    map2 (++)
        (repeat min parser)
        (atMost (max - min) parser)



-- UNTIL
-- regex: .* until lookahead


until : Parser end -> Parser a -> Parser (List a)
until endParser parser =
    let
        until_ values =
            andThen (\_ -> succeed values)
                endParser
                |> orElse
                    (andThen (\value -> until_ (values ++ [ value ]))
                        parser
                        |> orElse (succeed values)
                    )
    in
    until_ []



-- WHILE


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
--
-- SUCCEED


succeed : a -> Parser a
succeed value state =
    Ok ( value, state )



-- FAIL


fail : String -> Parser a
fail message state =
    Err
        { message = message
        , row = state.row
        , col = state.col
        , contextStack = state.contextStack
        }



-- AND THEN


andThen : (a -> Parser b) -> Parser a -> Parser b
andThen f parser state =
    parser state
        |> Result.andThen (\( value, newState ) -> f value newState)



-- AND THEN 2


andThen2 : (a -> b -> Parser c) -> Parser a -> Parser b -> Parser c
andThen2 f parserA parserB =
    andThen
        (\a ->
            andThen (\b -> f a b)
                parserB
        )
        parserA



-- OR ELSE


orElse : Parser a -> Parser a -> Parser a
orElse fallback parser state =
    case parser state of
        Ok result ->
            Ok result

        Err _ ->
            fallback state



-- WITH ERROR


withError : String -> Parser a -> Parser a
withError message parser state =
    parser state
        |> Result.mapError
            (\e -> { e | message = message })



---=== MAPPING ===---
--
-- MAP


map : (a -> b) -> Parser a -> Parser b
map f =
    andThen (\a -> succeed (f a))



-- MAP 2


map2 : (a -> b -> c) -> Parser a -> Parser b -> Parser c
map2 f parserA parserB =
    andThen
        (\a ->
            map (\b -> f a b)
                parserB
        )
        parserA



-- MAP 3


map3 : (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
map3 f parserA parserB parserC =
    andThen
        (\a ->
            map2 (\b c -> f a b c)
                parserB
                parserC
        )
        parserA



-- MAP 4


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



-- MAP 5


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



-- mapList


mapList : (List a -> b) -> List (Parser a) -> Parser b
mapList f parsers =
    map f (sequence parsers)
