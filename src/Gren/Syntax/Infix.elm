module Gren.Syntax.Infix exposing
    ( Infix, InfixDirection(..)
    )

{-| ## Types

@docs Infix, InfixDirection

-}

import Gren.Syntax.Node


{-| Type annotation for a infix definition
-}
type alias Infix =
    { direction : Gren.Syntax.Node.Node InfixDirection
    , precedence : Gren.Syntax.Node.Node Int
    , operator : Gren.Syntax.Node.Node String
    , function : Gren.Syntax.Node.Node String
    }


{-| Union type for infix direction
-}
type InfixDirection
    = Left
    | Right
    | Non
