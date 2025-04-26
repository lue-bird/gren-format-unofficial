module Gren.Syntax.TypeAlias exposing (TypeAlias)

{-| This syntax represents type aliases.

 

@docs TypeAlias

-}

import Gren.Syntax.Node
import Gren.Syntax.TypeAnnotation


{-| For example:

    {-| This is a person
    -}
    type alias Person =
        { name : String
        , age : Int
        }

-}
type alias TypeAlias =
    { documentation : Maybe (Gren.Syntax.Node.Node String)
    , name : Gren.Syntax.Node.Node String
    , generics : List (Gren.Syntax.Node.Node String)
    , typeAnnotation : Gren.Syntax.Node.Node Gren.Syntax.TypeAnnotation.TypeAnnotation
    }
