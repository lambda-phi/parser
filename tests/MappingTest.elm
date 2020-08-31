module MappingTest exposing (suite)

import Expect
import Parser exposing (map, map2, map3, map4, map5, mapList, parse, succeed)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Parser -- mapping"
        [ test "map" <|
            \_ ->
                map (\a -> a ++ " :)")
                    (succeed "a")
                    |> parse ""
                    |> Expect.equal (Ok "a :)")

        --
        , test "map2" <|
            \_ ->
                map2 (\a b -> a ++ b ++ " :)")
                    (succeed "a")
                    (succeed "b")
                    |> parse ""
                    |> Expect.equal (Ok "ab :)")

        --
        , test "map3" <|
            \_ ->
                map3 (\a b c -> a ++ b ++ c ++ " :)")
                    (succeed "a")
                    (succeed "b")
                    (succeed "c")
                    |> parse ""
                    |> Expect.equal (Ok "abc :)")

        --
        , test "map4" <|
            \_ ->
                map4 (\a b c d -> a ++ b ++ c ++ d ++ " :)")
                    (succeed "a")
                    (succeed "b")
                    (succeed "c")
                    (succeed "d")
                    |> parse ""
                    |> Expect.equal (Ok "abcd :)")

        --
        , test "map5" <|
            \_ ->
                map5 (\a b c d e -> a ++ b ++ c ++ d ++ e ++ " :)")
                    (succeed "a")
                    (succeed "b")
                    (succeed "c")
                    (succeed "d")
                    (succeed "e")
                    |> parse ""
                    |> Expect.equal (Ok "abcde :)")

        --
        , test "mapList" <|
            \_ ->
                mapList (\xs -> String.join "" xs ++ " :)")
                    [ succeed "a"
                    , succeed "b"
                    , succeed "c"
                    , succeed "d"
                    , succeed "e"
                    , succeed "f"
                    ]
                    |> parse ""
                    |> Expect.equal (Ok "abcdef :)")
        ]
