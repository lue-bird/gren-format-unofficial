module Gren.Syntax.Type exposing
    ( Type, ValueConstructor
    )

{-| This syntax represents custom types.
For example:

    {-| This is a color
    -}
    type Color
        = Blue
        | Red


## Types

@docs Type, ValueConstructor

-}

import Gren.Syntax.Node
import Gren.Syntax.TypeAnnotation


{-| Type alias that defines the syntax for a custom type.
All information that you can define in a type alias is embedded.
-}
type alias Type =
    { documentation : Maybe (Gren.Syntax.Node.Node String)
    , name : Gren.Syntax.Node.Node String
    , generics : List (Gren.Syntax.Node.Node String)
    , constructors : List (Gren.Syntax.Node.Node ValueConstructor)
    }


{-| Syntax for a custom type value constructor.
-}
type alias ValueConstructor =
    { name : Gren.Syntax.Node.Node String
    , arguments : List (Gren.Syntax.Node.Node Gren.Syntax.TypeAnnotation.TypeAnnotation)
    }
