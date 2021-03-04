module Parser.Expression exposing
    ( expression
    , term, prefix, suffix, inbetween, fromLeft, fromRight
    , Operator(..)
    )

{-| Parsers for expressions with operator precedence.


# Basic usage

@docs expression


# Operators

@docs term, prefix, suffix, inbetween, fromLeft, fromRight


# Custom operators

@docs Operator

-}

-- Based on Pratt parsers for operator precedence, but adapted on a functional style.
-- https://eli.thegreenplace.net/2010/01/02/top-down-operator-precedence-parsing

import Parser exposing (Parser, andThen, drop, expected, lazy, map, oneOf, succeed, take)
import Parser.Char exposing (anyChar)
import Parser.Common exposing (spaces)
import Parser.Sequence exposing (exactly, fold)



-- Basic usage


{-| Parses an expression using the provided operators with operator precedence.
There can be multiple operators sharing the same precedence like `+` and `-`,
or `*` and `/`.

    import Parser exposing (Parser, andThen, drop, parse, succeed, take)
    import Parser.Char exposing (char)
    import Parser.Common exposing (number)

    factorial : Float -> Float
    factorial n =
        List.product (List.range 1 (floor n) |> List.map toFloat)

    calculate : Parser Float
    calculate =
        expression
            [ [ prefix identity (char '+')
              , prefix (\x -> -x) (char '-')
              , suffix factorial (char '!')
              ]
            , [ fromRight (^) (char '^')
              ]
            , [ fromLeft (*) (char '*')
              , fromLeft (/) (char '/')
              ]
            , [ fromLeft (+) (char '+')
              , fromLeft (-) (char '-')
              ]
            , [ inbetween identity (char '(') (char ')')
              , term identity number
              ]
            ]

    -- Terms and parentheses.
    parse "1" calculate   --> Ok 1
    parse "(1)" calculate --> Ok 1

    -- Prefix operators.
    parse "-1" calculate  --> Ok -1  --   -1
    parse "+-1" calculate --> Ok -1  -- +(-1)
    parse "-+1" calculate --> Ok -1  -- -(+1)
    parse "--1" calculate --> Ok 1   -- -(-1)

    -- Suffix operators.
    parse "5!" calculate  --> Ok 120  -- 5!
    parse "3!!" calculate --> Ok 720  -- (3!)!

    -- Left-associative binary operators.
    parse "1+2+3" calculate   --> Ok 6    -- (1 + 2) + 3
    parse "1-2-3" calculate   --> Ok -4   -- (1 - 2) - 3
    parse "1-(2-3)" calculate --> Ok 2    -- 1 - (2 - 3)
    parse "2*3*4" calculate   --> Ok 24   -- (2 * 3) * 4
    parse "2/4/5" calculate   --> Ok 0.1  -- (2 / 4) / 5
    parse "2/(4/5)" calculate --> Ok 2.5  -- 2 / (4 / 5)

    -- Right-associative binary operators.
    parse "2^3^2" calculate   --> Ok 512  -- 2 ^ (3 ^ 2)
    parse "(2^3)^2" calculate --> Ok 64   -- (2 ^ 3) ^ 2)

    -- Operator precedence.
    parse "1+-2" calculate  --> Ok -1  -- 1 + (-2)
    parse "1+2*3" calculate --> Ok 7   -- 1 + (2 * 3)
    parse "1*2+3" calculate --> Ok 5   -- (1 * 2) + 3
    parse "1*2^3" calculate --> Ok 8   -- 1 * (2 ^ 3)
    parse "1^2*3" calculate --> Ok 3   -- (1 ^ 2) * 3

-}
expression : List (List (Operator a)) -> Parser a
expression operators =
    let
        opsWithPrecedence : List ( Int, Operator a )
        opsWithPrecedence =
            operators
                |> List.indexedMap (\i ops -> ( List.length operators - i, ops ))
                |> List.concatMap (\( prec, ops ) -> List.map (\op -> ( prec, op )) ops)

        unaryOps : List (UnaryOperator a)
        unaryOps =
            List.filterMap
                (\( prec, operator ) ->
                    case operator of
                        Prefix parse ->
                            Just (UnaryOperator parse prec)

                        InfixFromLeft _ ->
                            Nothing

                        InfixFromRight _ ->
                            Nothing
                )
                opsWithPrecedence

        binaryOps : List (BinaryOperator a)
        binaryOps =
            List.filterMap
                (\( opPrec, operator ) ->
                    case operator of
                        Prefix _ ->
                            Nothing

                        InfixFromLeft parse ->
                            Just (BinaryOperator parse (\prec -> prec < opPrec) opPrec)

                        InfixFromRight parse ->
                            Just (BinaryOperator parse (\prec -> prec <= opPrec) opPrec)
                )
                opsWithPrecedence
    in
    evalWithOps unaryOps binaryOps 0


{-| Defines an individual term.
This is usually a number, or a variable name,
or anything not containing an operator.

    import Parser exposing (Parser, parse)
    import Parser.Common exposing (number)

    expr : Parser Float
    expr =
        expression [ [ term identity number ] ]

    parse "5" expr --> Ok 5

    -- It does not trim spaces beforehand.
    parse " 5" expr |> Result.toMaybe --> Nothing

-}
term : (a -> b) -> Parser a -> Operator b
term eval x =
    Prefix (\_ -> map eval x)


{-| Defines a unary prefix operator.

    import Parser exposing (Parser, parse)
    import Parser.Char exposing (char)
    import Parser.Common exposing (number)

    expr : Parser Float
    expr =
        expression
            [ [ prefix (\x -> -x) (char '-') ]
            , [ term identity number ]
            ]

    parse "-5" expr --> Ok -5

    -- It does not trim spaces beforehand.
    parse " -5" expr |> Result.toMaybe --> Nothing

    -- But it does afterwards.
    parse "- 5" expr --> Ok -5

-}
prefix : (a -> a) -> Parser op -> Operator a
prefix eval op =
    Prefix
        (\expr ->
            succeed eval
                |> drop op
                |> drop spaces
                |> take expr
        )


{-| Defines a unary suffix operator.

    import Parser exposing (Parser, parse)
    import Parser.Char exposing (char)
    import Parser.Common exposing (number)

    factorial : Float -> Float
    factorial n =
        List.product (List.range 1 (floor n) |> List.map toFloat)

    expr : Parser Float
    expr =
        expression
            [ [ suffix factorial (char '!') ]
            , [ term identity number ]
            ]

    parse "5!" expr --> Ok 120

    -- It trims spaces beforehand.
    parse "5 !" expr --> Ok 120

-}
suffix : (a -> a) -> Parser op -> Operator a
suffix eval op =
    InfixFromLeft
        (\_ ->
            succeed eval
                |> drop spaces
                |> drop op
        )


{-| Defines an expression surrounded by an `open` and `close` operator.

    import Parser exposing (Parser, parse)
    import Parser.Char exposing (char)
    import Parser.Common exposing (number)

    expr : Parser Float
    expr =
        expression
            [ [ inbetween identity (char '(') (char ')') ]
            , [ term identity number ]
            ]

    parse "(5)" expr --> Ok 5
    parse "(5" expr |> Result.toMaybe --> Nothing
    parse "()" expr |> Result.toMaybe --> Nothing

    -- It does not trim spaces beforehand.
    parse " (5)" expr |> Result.toMaybe --> Nothing

    -- But it does inbetween.
    parse "( 5 )" expr --> Ok 5

-}
inbetween : (a -> a) -> Parser open -> Parser close -> Operator a
inbetween eval open close =
    Prefix
        (\expr ->
            succeed eval
                |> drop open
                |> drop spaces
                |> take expr
                |> drop spaces
                |> drop close
        )


{-| Defines a binary left-associative operator.

    import Parser exposing (Parser, parse)
    import Parser.Char exposing (char)
    import Parser.Common exposing (number)

    expr : Parser Float
    expr =
        expression
            [ [ fromLeft (+) (char '+')
              , fromLeft (-) (char '-')
              ]
            , [ term identity number ]
            ]

    parse "1+2" expr   --> Ok 3
    parse "1+2+3" expr --> Ok 6
    parse "1-2-3" expr --> Ok -4

    -- It does not trim spaces beforehand.
    parse " 1+2" expr |> Result.toMaybe --> Nothing

    -- But it does inbetween.
    parse "1 + 2" expr --> Ok 3

-}
fromLeft : (a -> a -> a) -> Parser op -> Operator a
fromLeft eval op =
    InfixFromLeft
        (\expr ->
            succeed (\right left -> eval left right)
                |> drop spaces
                |> drop op
                |> drop spaces
                |> take expr
        )


{-| Defines a binary right-associative operator.

    import Parser exposing (Parser, parse)
    import Parser.Char exposing (char)
    import Parser.Common exposing (number)

    expr : Parser Float
    expr =
        expression
            [ [ fromRight (^) (char '^') ]
            , [ term identity number ]
            ]

    parse "2^3" expr   --> Ok 8
    parse "2^3^2" expr --> Ok 512 -- 2 ^ (3 ^ 2)

    -- It does not trim spaces beforehand.
    parse " 2^3" expr |> Result.toMaybe --> Nothing

    -- But it does inbetween.
    parse "2 ^ 3" expr --> Ok 8

-}
fromRight : (a -> a -> a) -> Parser op -> Operator a
fromRight eval op =
    InfixFromRight
        (\expr ->
            succeed (\right left -> eval left right)
                |> drop spaces
                |> drop op
                |> drop spaces
                |> take expr
        )


evalWithOps : List (UnaryOperator a) -> List (BinaryOperator a) -> Int -> Parser a
evalWithOps unaryOps binaryOps precedence =
    let
        evalUnary : UnaryOperator a -> Parser a
        evalUnary op =
            op.parse (lazy (\_ -> evalWithOps unaryOps binaryOps op.precedence))

        evalBinary : BinaryOperator a -> Parser (a -> a)
        evalBinary op =
            if op.check precedence then
                op.parse (lazy (\_ -> evalWithOps unaryOps binaryOps op.precedence))

            else
                expected ""
    in
    oneOf (List.map evalUnary unaryOps)
        |> andThen
            (\firstTerm ->
                fold (\left f -> f left)
                    firstTerm
                    (oneOf (List.map evalBinary binaryOps))
            )



-- Custom operators


{-| Describes an operator as a parser.
They take a `Parser a` as an input, which is used to parse subexpressions
recursively.

> ℹ️ Both `Prefix` and `Infix*` operators _must_ start by parsing something
> other than a subexpression, otherwise there will be a stack overflow.

    import Parser exposing (Parser, drop, parse, succeed, take)
    import Parser.Char exposing (char)
    import Parser.Common exposing (number)

    -- Prefix operators simply parse and apply the operation.
    neg : Operator Float
    neg =
        Prefix
            (\expr ->
                succeed (\x -> -x)
                    |> drop (char '-')
                    |> take expr
            )

    -- Infix operators parse only the right side of the subexpression.
    -- The Parser returns a function that takes the left side of the
    -- subexpression as an input, and applies the operation.
    -- This is for a left-associative infix operator.
    add : Operator Float
    add =
        InfixFromLeft
            (\expr ->
                succeed (\right left -> left + right)
                    |> drop (char '+')
                    |> take expr
            )

    -- You can also define a right-associative infix operator.
    pow : Operator Float
    pow =
        InfixFromLeft
            (\expr ->
                succeed (\right left -> left ^ right)
                    |> drop (char '^')
                    |> take expr
            )

    -- A term can be achieved by simply parsing a token but not recursing.
    num : Operator Float
    num =
        Prefix (\_ -> number)

    calculate : Parser Float
    calculate =
        expression
            [ [ neg ] -- -1
            , [ pow ] -- 1 ^ 2
            , [ add ] -- 1 + 2
            , [ num ] -- 1
            ]

    parse "1+-2^3" calculate --> Ok -7 -- 1 + ((-2) ^ 3)

-}
type Operator a
    = Prefix (Parser a -> Parser a)
    | InfixFromLeft (Parser a -> Parser (a -> a))
    | InfixFromRight (Parser a -> Parser (a -> a))


type alias UnaryOperator a =
    { parse : Parser a -> Parser a
    , precedence : Int
    }


type alias BinaryOperator a =
    { parse : Parser a -> Parser (a -> a)
    , check : Int -> Bool
    , precedence : Int
    }
