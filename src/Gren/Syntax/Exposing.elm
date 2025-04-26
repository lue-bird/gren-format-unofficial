module Gren.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..), ExposedType)

{-| This syntax represents the exposing declaration for both imports and module headers.
For example:

    exposing (Foo(..))
    exposing (..)


## Types

@docs Exposing, TopLevelExpose, ExposedType

-}

import Gren.Syntax.Node
import Gren.Syntax.Range


{-| Different kind of exposing declarations
-}
type Exposing
    = All Gren.Syntax.Range.Range
    | Explicit (List (Gren.Syntax.Node.Node TopLevelExpose))


{-| An exposed entity
-}
type TopLevelExpose
    = InfixExpose String
    | FunctionExpose String
    | TypeOrAliasExpose String
    | TypeExpose ExposedType


{-| Exposed Type
-}
type alias ExposedType =
    { name : String
    , open : Maybe Gren.Syntax.Range.Range
    }
