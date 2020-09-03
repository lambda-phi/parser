module Parser.Error exposing (errorDump, errorDumpSnippet)

{-| Error reporting utilities.


# Error reporting

@docs errorDump, errorDumpSnippet

-}

import Parser exposing (Error)



---=== ERROR REPORTING ===---


{-| Dumps the error into a human-readable format.

    import Parser exposing (Parser, andThen, char, drop, into, parse, spaces, succeed, take)
    import Parser.Common exposing (number)

    spaces
        |> andThen (\_ -> number)
        |> parse "  abc  "
        |> Result.mapError (errorDump "filename.txt")
    --> Err
    -->     [ "[ERROR] filename.txt:1:3: I got stuck while parsing"
    -->     , ""
    -->     , "input text: expected a digit [0-9], but got 'a' instead"
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
        |> andThen (\_ -> point)
        |> parse "  (12,)  "
        |> Result.mapError (errorDump "filename.txt")
    --> Err
    -->     [ "[ERROR] filename.txt:1:7: I got stuck while parsing"
    -->     , "  in Point at line 1"
    -->     , ""
    -->     , "Point: expected a digit [0-9], but got ')' instead"
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
        |> andThen (\_ -> line)
        |> parse "  [(12,34),(56,)]  "
        |> Result.mapError (errorDump "filename.txt")
    --> Err
    -->     [ "[ERROR] filename.txt:1:16: I got stuck while parsing"
    -->     , "  in Point at line 1"
    -->     , "  in Line at line 1"
    -->     , ""
    -->     , "Point: expected a digit [0-9], but got ')' instead"
    -->     , ""
    -->     , "1|  [(12,34),(56,)]  "
    -->     , "             ~~~~^"
    -->     ]

-}
errorDump : String -> Error -> List String
errorDump source err =
    (("[ERROR] " ++ source ++ ":")
        ++ (String.fromInt err.row ++ ":" ++ String.fromInt err.col)
        ++ ": I got stuck while parsing"
    )
        :: List.map
            (\ctx ->
                "  in " ++ ctx.name ++ " at line " ++ String.fromInt ctx.row
            )
            err.context
        ++ [ ""
           , (List.head err.context
                |> Maybe.map (\ctx -> ctx.name)
                |> Maybe.withDefault "input text"
             )
                ++ (": " ++ err.message)
           , ""
           ]
        ++ errorDumpSnippet err


{-| Dumps a snippet of the input text that caused the parser to fail.

    import Parser exposing (Parser, andThen, char, drop, into, parse, spaces, succeed, take)
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
        |> andThen (\_ -> point)
        |> parse "  (12,)  "
        |> Result.mapError errorDumpSnippet
    --> Err
    -->     [ "1|  (12,)  "
    -->     , "    ~~~~^"
    -->     ]

    spaces
        |> andThen (\_ -> point)
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
        |> Result.mapError errorDumpSnippet
    --> Err
    -->     [ "2|  (  "
    -->     , "3|  12  "
    -->     , "4|  ,  "
    -->     , "5|  )  "
    -->     , " +~~^"
    -->     ]

-}
errorDumpSnippet : Error -> List String
errorDumpSnippet err =
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
