module Gren.Syntax.TypeAnnotation exposing
    ( TypeAnnotation(..), RecordDefinition, RecordField
    )

{-| This syntax represents the type annotation syntax.
For example:

    Int -> String


## Types

@docs TypeAnnotation, RecordDefinition, RecordField

-}

import Gren.Syntax.ModuleName
import Gren.Syntax.Node


{-| Custom type for different type annotations. For example:

  - `GenericType`: `a`
  - `Typed`: `Maybe (Int -> String)`
  - `Unit`: `()`
  - `Tuples`: `(a, b, c)`
  - `Record`: `{ name : String}`
  - `GenericRecord`: `{ a | name : String}`
  - `FunctionTypeAnnotation`: `Int -> String`

-}
type TypeAnnotation
    = GenericType String
    | Typed (Gren.Syntax.Node.Node ( Gren.Syntax.ModuleName.ModuleName, String )) (List (Gren.Syntax.Node.Node TypeAnnotation))
    | Unit
    | Tupled (List (Gren.Syntax.Node.Node TypeAnnotation))
    | Record RecordDefinition
    | GenericRecord (Gren.Syntax.Node.Node String) (Gren.Syntax.Node.Node RecordDefinition)
    | FunctionTypeAnnotation (Gren.Syntax.Node.Node TypeAnnotation) (Gren.Syntax.Node.Node TypeAnnotation)


{-| A list of fields in-order of a record type annotation.
-}
type alias RecordDefinition =
    List (Gren.Syntax.Node.Node RecordField)


{-| Single field of a record. A name and its type.
-}
type alias RecordField =
    ( Gren.Syntax.Node.Node String, Gren.Syntax.Node.Node TypeAnnotation )
