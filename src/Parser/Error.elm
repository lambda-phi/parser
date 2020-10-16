module Parser.Error exposing (message, dump, dumpCodeSnippet)

{-| Error reporting utilities.


# Error reporting

@docs message, dump, dumpCodeSnippet

-}

import Parser exposing (Error)



-- ERROR REPORTING


{-| Creates an error message from an `Error` data.

    import Parser exposing (parse)
    import Parser.Char exposing (letter)

    -- Getting a digit instead of a letter.
    parse "123" letter
        |> Result.mapError message
    --> Err "1:1: I was expecting a letter [a-zA-Z]. I got stuck when I got the character '1'."

    -- Running out of input characters.
    parse "" letter
        |> Result.mapError message
    --> Err "1:0: I was expecting a letter [a-zA-Z]. I reached the end of the input text."

-}
message : Error -> String
message err =
    (String.fromInt err.row ++ ":" ++ String.fromInt err.col)
        ++ (": I was expecting " ++ err.expected ++ ". ")
        ++ (case err.lastChar of
                Just ch ->
                    "I got stuck when I got the character '" ++ String.fromChar ch ++ "'."

                Nothing ->
                    "I reached the end of the input text."
           )


{-| Dumps the error into a human-readable format.

    import Parser exposing (Parser, andThenKeep, drop, into, parse, succeed, take)
    import Parser.Char exposing (char)
    import Parser.Common exposing (number, spaces)

    spaces
        |> andThenKeep number
        |> parse "  abc  "
        |> Result.mapError (dump "filename.txt")
    --> Err
    -->     [ "[ERROR] filename.txt:1:3: I was expecting a digit [0-9]. I got stuck when I got the character 'a'."
    -->     , ""
    -->     , "1|  abc  "
    -->     , "    ^"
    -->     ]


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

    spaces
        |> andThenKeep point
        |> parse "  (12,)  "
        |> Result.mapError (dump "filename.txt")
    --> Err
    -->     [ "[ERROR] filename.txt:1:7: I was expecting a digit [0-9]. I got stuck when I got the character ')'."
    -->     , "  in Point at line 1:3"
    -->     , ""
    -->     , "1|  (12,)  "
    -->     , "    ~~~~^"
    -->     ]

    type alias Line =
        { p1 : Point
        , p2 : Point
        }

    line : Parser Line
    line =
        into "Line"
            (succeed Line
                |> drop (char '[')
                |> take point
                |> drop (char ',')
                |> take point
                |> drop (char ']')
            )

    spaces
        |> andThenKeep line
        |> parse "  [(12,34),(56,)]  "
        |> Result.mapError (dump "filename.txt")
    --> Err
    -->     [ "[ERROR] filename.txt:1:16: I was expecting a digit [0-9]. I got stuck when I got the character ')'."
    -->     , "  in Point at line 1:12"
    -->     , "  in Line at line 1:3"
    -->     , ""
    -->     , "1|  [(12,34),(56,)]  "
    -->     , "             ~~~~^"
    -->     ]

-}
dump : String -> Error -> List String
dump source err =
    ("[ERROR] " ++ source ++ ":" ++ message err)
        :: List.map
            (\ctx ->
                ("  in " ++ ctx.name ++ " at line " ++ String.fromInt ctx.row)
                    ++ (":" ++ String.fromInt ctx.col)
            )
            err.context
        ++ ("" :: dumpCodeSnippet err)


{-| Dumps a snippet of the input text that caused the parser to fail.

    import Parser exposing (Parser, andThenKeep, drop, into, parse, succeed, take)
    import Parser.Char exposing (char)
    import Parser.Common exposing (number, spaces)

    type alias Point =
        { x : Float
        , y : Float
        }

    point : Parser Point
    point =
        into "Point"
            (succeed Point
                |> drop (char '(')
                |> drop spaces
                |> take number
                |> drop spaces
                |> drop (char ',')
                |> drop spaces
                |> take number
                |> drop spaces
                |> drop (char ')')
            )

    spaces
        |> andThenKeep point
        |> parse "  (12,)  "
        |> Result.mapError dumpCodeSnippet
    --> Err
    -->     [ "1|  (12,)  "
    -->     , "    ~~~~^"
    -->     ]

    spaces
        |> andThenKeep point
        |> parse
            (String.join "\n"
                [ "  "
                , "  (  "
                , "  12  "
                , "  ,  "
                , "  )  "
                , "  "
                ]
            )
        |> Result.mapError dumpCodeSnippet
    --> Err
    -->     [ "2|  (  "
    -->     , "3|  12  "
    -->     , "4|  ,  "
    -->     , "5|  )  "
    -->     , " +~~^"
    -->     ]

-}
dumpCodeSnippet : Error -> List String
dumpCodeSnippet err =
    let
        range =
            case err.context of
                [] ->
                    { startRow = err.row
                    , startCol = err.col
                    }

                ctx :: _ ->
                    { startRow = ctx.row
                    , startCol = ctx.col
                    }

        strRows =
            { start = String.fromInt range.startRow
            , end = String.fromInt err.row
            }

        lineNoWidth =
            Basics.max (String.length strRows.start) (String.length strRows.end)

        sourceSnippet =
            String.lines err.input
                |> List.drop (range.startRow - 1)
                |> List.take (err.row - range.startRow + 1)
                |> List.indexedMap
                    (\i ln ->
                        String.padLeft lineNoWidth
                            ' '
                            (String.fromInt (range.startRow + i))
                            ++ ("|" ++ ln)
                    )

        underline =
            if List.isEmpty err.context then
                String.repeat (lineNoWidth + err.col) " " ++ "^"

            else if range.startRow == err.row then
                String.repeat (lineNoWidth + range.startCol) " "
                    ++ (String.repeat (err.col - range.startCol) "~" ++ "^")

            else
                String.repeat lineNoWidth " "
                    ++ ("+" ++ String.repeat (err.col - 1) "~" ++ "^")
    in
    sourceSnippet ++ [ underline ]
