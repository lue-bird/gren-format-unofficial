Pretty print [gren](https://gren-lang.org/) syntax
in the style of [`elm-format`](https://github.com/avh4/gren-format).

gren had a formatter in 0.4, but it stripped a few comments and wasn’t in active development, so it was removed in 0.5.

The core team intends to re-implement formatting not before december 2025
so here's a way to keep your code tidy in the meantime.


```gren
import GrenPrint
import GrenParserLenient

"""
module   Sample  exposing(...)
plus2 (n)= {- this adds 2-} n
+
2
"""
    |> GrenParserLenient.run GrenParserLenient.module_
    |> Maybe.map
        (\syntaxModule ->
            syntaxModule
                |> GrenPrint.module_
                |> GrenPrint.toString
        )
-->
Just """module Sample exposing (..)


plus2 n =
    {- this adds 2 -}
    n
        + 2
"""
```

To try it out, you can
run [this node script](https://github.com/lue-bird/gren-format-unofficial/tree/main/node-gren-format-unofficial).


## TODO
  - support `when` `is`
  - support nested record patterns
  - comments before/after parenthesized types will get eaten because type parens are not stored in the syntax tree
  - some floats in exponent representation are formatted to without it and the other way around
  - formatting documentation markdown
  - convert codebase to gren

Please [report](https://github.com/lue-bird/gren-format-unofficial/issues/new) others issues you notice <3
