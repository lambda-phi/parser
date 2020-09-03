# Parser

An easy to use general-purpose parser for Elm.

This library aims to provide a clean and intuitive parsing library with good error messages out of the box.

> ℹ️ This has been inspired by the
[elm/parser](https://package.elm-lang.org/packages/elm/parser/latest/)
and [dasch/parser](https://package.elm-lang.org/packages/dasch/parser/latest/)
libraries.

## How to use

A good place to start is by defining the data type you want to parse into.
Then, it's a good idea to define a function to parse into that data type.

Here's a simple example to parse an `(x, y)` point, such as `(42, 3.14)`.

```elm
import Parser exposing (Parser, char, drop, into, parse, spaces, succeed, take)
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

parse "(2.71, 3.14)" point --> Ok {x = 2.71, y = 3.14}
```
