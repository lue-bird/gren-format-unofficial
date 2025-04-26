module Gren.Syntax.Pattern exposing
    ( Pattern(..), QualifiedNameRef
    )

{-| This syntax represents the patterns.
For example:

    Just x as someMaybe
    {name, age}


## Types

@docs Pattern, QualifiedNameRef

-}

import Gren.Syntax.ModuleName
import Gren.Syntax.Node


{-| Custom type for all patterns such as:

  - `AllPattern`: `_`
  - `UnitPattern`: `()`
  - `CharPattern`: `'c'`
  - `StringPattern`: `"hello"`
  - `IntPattern`: `42`
  - `HexPattern`: `0x11`
  - `FloatPattern`: `42.0`
  - `TuplePattern`: `(a, b)`
  - `RecordPattern`: `{name, age}`
  - `UnConsPattern`: `x :: xs`
  - `ListPattern`: `[ x, y ]`
  - `VarPattern`: `x`
  - `NamedPattern`: `Just _`
  - `AsPattern`: `_ as x`
  - `ParenthesizedPattern`: `( _ )`

-}
type Pattern
    = AllPattern
    | UnitPattern
    | CharPattern Char
    | StringPattern String
    | IntPattern Int
    | HexPattern Int
    | FloatPattern Float
    | TuplePattern (List (Gren.Syntax.Node.Node Pattern))
    | RecordPattern (List (Gren.Syntax.Node.Node String))
    | UnConsPattern (Gren.Syntax.Node.Node Pattern) (Gren.Syntax.Node.Node Pattern)
    | ListPattern (List (Gren.Syntax.Node.Node Pattern))
    | VarPattern String
    | NamedPattern QualifiedNameRef (List (Gren.Syntax.Node.Node Pattern))
    | AsPattern (Gren.Syntax.Node.Node Pattern) (Gren.Syntax.Node.Node String)
    | ParenthesizedPattern (Gren.Syntax.Node.Node Pattern)


{-| Qualified name reference such as `Maybe.Just`.
-}
type alias QualifiedNameRef =
    { moduleName : List String
    , name : String
    }


{-| Get all the modules names that are used in the pattern (and its nested patterns).
Use this to collect qualified patterns, such as `Maybe.Just x`.
-}
moduleNames : Pattern -> List Gren.Syntax.ModuleName.ModuleName
moduleNames p =
    -- TODO Remove this function or make it take a `Node Pattern`
    -- Should it return a `Set`?
    case p of
        TuplePattern xs ->
            List.concatMap (\(Gren.Syntax.Node.Node _ x) -> moduleNames x) xs

        RecordPattern _ ->
            []

        UnConsPattern (Gren.Syntax.Node.Node _ left) (Gren.Syntax.Node.Node _ right) ->
            moduleNames left ++ moduleNames right

        ListPattern xs ->
            List.concatMap (\(Gren.Syntax.Node.Node _ x) -> moduleNames x) xs

        NamedPattern qualifiedNameRef subPatterns ->
            qualifiedNameRef.moduleName :: List.concatMap (\(Gren.Syntax.Node.Node _ x) -> moduleNames x) subPatterns

        AsPattern (Gren.Syntax.Node.Node _ inner) _ ->
            moduleNames inner

        ParenthesizedPattern (Gren.Syntax.Node.Node _ inner) ->
            moduleNames inner

        _ ->
            []
