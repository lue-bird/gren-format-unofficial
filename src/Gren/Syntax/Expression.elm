module Gren.Syntax.Expression exposing (Expression(..), Lambda, LetBlock, LetDeclaration(..), RecordSetter, CaseBlock, Cases, Case, Function, FunctionImplementation)

{-| This syntax represents all that you can express in Gren.
Although it is a easy and simple language, you can express a lot! See the `Expression` type for all the things you can express.


## Types

@docs Expression, Lambda, LetBlock, LetDeclaration, RecordSetter, CaseBlock, Cases, Case, Function, FunctionImplementation

-}

import Gren.Syntax.Infix
import Gren.Syntax.ModuleName
import Gren.Syntax.Node
import Gren.Syntax.Pattern
import Gren.Syntax.Range
import Gren.Syntax.TypeAnnotation


{-| Type alias for a full function
-}
type alias Function =
    { documentation : Maybe (Gren.Syntax.Node.Node String)
    , signature :
        Maybe
            (Gren.Syntax.Node.Node
                { name : Gren.Syntax.Node.Node String
                , typeAnnotation : Gren.Syntax.Node.Node Gren.Syntax.TypeAnnotation.TypeAnnotation
                }
            )
    , declaration : Gren.Syntax.Node.Node FunctionImplementation
    }


{-| Get the full range of a function
-}
functionRange : Function -> Gren.Syntax.Range.Range
functionRange function =
    let
        declarationRange : Gren.Syntax.Range.Range
        declarationRange =
            Gren.Syntax.Node.range function.declaration

        startRange : Gren.Syntax.Range.Range
        startRange =
            case function.documentation of
                Just (Gren.Syntax.Node.Node range _) ->
                    range

                Nothing ->
                    case function.signature of
                        Just (Gren.Syntax.Node.Node range _) ->
                            range

                        Nothing ->
                            declarationRange
    in
    { start = startRange.start
    , end = declarationRange.end
    }


{-| Type alias for a function's implementation
-}
type alias FunctionImplementation =
    { name : Gren.Syntax.Node.Node String
    , arguments : List (Gren.Syntax.Node.Node Gren.Syntax.Pattern.Pattern)
    , expression : Gren.Syntax.Node.Node Expression
    }


{-| Custom type for all expressions such as:

  - `Unit`: `()`
  - `Application`: `add a b`
  - `OperatorApplication`: `a + b`
  - `FunctionOrValue`: `add` or `True`
  - `IfBlock`: `if a then b else c`
  - `PrefixOperator`: `(+)`
  - `Operator`: `+` (not possible to get in practice)
  - `Integer`: `42`
  - `Hex`: `0x1F`
  - `Floatable`: `42.0`
  - `Negation`: `-a`
  - `Literal`: `"text"`
  - `CharLiteral`: `'a'`
  - `TupledExpression`: `(a, b)` or `(a, b, c)`
  - `ParenthesizedExpression`: `(a)`
  - `LetExpression`: `let a = 4 in a`
  - `CaseExpression`: `case a of` followed by pattern matches
  - `LambdaExpression`: `(\a -> a)`
  - `RecordExpr`: `{ name = "text" }`
  - `ListExpr`: `[ x, y ]`
  - `RecordAccess`: `a.name`
  - `RecordAccessFunction`: `.name`
  - `RecordUpdateExpression`: `{ a | name = "text" }`
  - `GLSLExpression`: `[glsl| ... |]`

-}
type Expression
    = UnitExpr
    | Application (List (Gren.Syntax.Node.Node Expression))
    | OperatorApplication String (Gren.Syntax.Node.Node Expression) (Gren.Syntax.Node.Node Expression)
    | FunctionOrValue Gren.Syntax.ModuleName.ModuleName String
    | IfBlock (Gren.Syntax.Node.Node Expression) (Gren.Syntax.Node.Node Expression) (Gren.Syntax.Node.Node Expression)
    | PrefixOperator String
    | Operator String
    | Integer Int
    | Hex Int
    | Floatable Float
    | Negation (Gren.Syntax.Node.Node Expression)
    | Literal String
    | CharLiteral Char
    | TupledExpression (List (Gren.Syntax.Node.Node Expression))
    | ParenthesizedExpression (Gren.Syntax.Node.Node Expression)
    | LetExpression LetBlock
    | CaseExpression CaseBlock
    | LambdaExpression Lambda
    | RecordExpr (List (Gren.Syntax.Node.Node RecordSetter))
    | ListExpr (List (Gren.Syntax.Node.Node Expression))
    | RecordAccess (Gren.Syntax.Node.Node Expression) (Gren.Syntax.Node.Node String)
    | RecordAccessFunction String
    | RecordUpdateExpression (Gren.Syntax.Node.Node String) (List (Gren.Syntax.Node.Node RecordSetter))
    | GLSLExpression String


{-| Expression for setting a record field
-}
type alias RecordSetter =
    ( Gren.Syntax.Node.Node String, Gren.Syntax.Node.Node Expression )


{-| Expression for a let block
-}
type alias LetBlock =
    { declarations : List (Gren.Syntax.Node.Node LetDeclaration)
    , expression : Gren.Syntax.Node.Node Expression
    }


{-| Union type for all possible declarations in a let block
-}
type LetDeclaration
    = LetFunction Function
    | LetDestructuring (Gren.Syntax.Node.Node Gren.Syntax.Pattern.Pattern) (Gren.Syntax.Node.Node Expression)


{-| Expression for a lambda
-}
type alias Lambda =
    { args : List (Gren.Syntax.Node.Node Gren.Syntax.Pattern.Pattern)
    , expression : Gren.Syntax.Node.Node Expression
    }


{-| Expression for a case block
-}
type alias CaseBlock =
    { expression : Gren.Syntax.Node.Node Expression
    , cases : Cases
    }


{-| A case in a case block
-}
type alias Case =
    ( Gren.Syntax.Node.Node Gren.Syntax.Pattern.Pattern, Gren.Syntax.Node.Node Expression )


{-| Type alias for a list of cases
-}
type alias Cases =
    List Case
