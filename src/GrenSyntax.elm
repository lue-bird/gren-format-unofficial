module GrenSyntax exposing
    ( File, ModuleName, Import
    , DefaultModuleData, EffectModuleData, Module(..)
    , Exposing(..), TopLevelExpose(..), ExposedType
    , Declaration(..), Type, ValueConstructor, TypeAlias, Infix, InfixDirection(..)
    , Pattern(..), QualifiedNameRef
    , Expression(..), Lambda, LetBlock, LetDeclaration(..), RecordSetter, Case, Function, FunctionImplementation
    , StringQuotingStyle(..)
    , TypeAnnotation(..), TypeAnnotationRecordField
    , Range, Location, Node(..), nodeCombine, nodeMap, nodeRange, nodeValue
    )

{-| Gren syntax tree

@docs File, ModuleName, Import
@docs DefaultModuleData, EffectModuleData, Module
@docs Exposing, TopLevelExpose, ExposedType
@docs Declaration, Type, ValueConstructor, TypeAlias, Infix, InfixDirection
@docs Pattern, QualifiedNameRef
@docs Expression, Lambda, LetBlock, LetDeclaration, RecordSetter, Case, Function, FunctionImplementation
@docs StringQuotingStyle
@docs TypeAnnotation, TypeAnnotationRecordField
@docs Range, Location, Node, nodeCombine, nodeMap, nodeRange, nodeValue

-}


{-| module header. For example:

    module Html.Attributes exposing (style)

-}
type Module
    = NormalModule DefaultModuleData
    | PortModule DefaultModuleData
    | EffectModule EffectModuleData


{-| Used for imports, module names, and for qualification.
For example:

    module GrenSyntax ...

    import Foo.Bar ...

    import ... as Something

    My.Module.something

    My.Module.SomeType

-}
type alias ModuleName =
    List String


{-| Data for a default default
-}
type alias DefaultModuleData =
    { moduleName : Node ModuleName
    , exposingList : Node Exposing
    }


{-| Data for an effect module
-}
type alias EffectModuleData =
    { moduleName : Node ModuleName
    , exposingList : Node Exposing
    , command : Maybe (Node String)
    , subscription : Maybe (Node String)
    }


{-| Type annotation for a file
-}
type alias File =
    { moduleDefinition : Node Module
    , imports : List (Node Import)
    , declarations : List (Node Declaration)
    , comments : List (Node String)
    }


{-| exposing declaration for both imports and module headers.
For example:

    exposing (Foo(..))
    exposing (..)

-}
type Exposing
    = All Range
    | Explicit (List (Node TopLevelExpose))


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
    , open : Maybe Range
    }


{-| For example:

    import Html.Attributes as HA exposing (style)

-}
type alias Import =
    { moduleName : Node ModuleName
    , moduleAlias : Maybe (Node ModuleName)
    , exposingList : Maybe (Node Exposing)
    }


{-| A module-level declaration. Can be one of the following:

  - Functions: `add x y = x + y`
  - Custom types: `type Color = Blue | Red`
  - Type aliases: `type alias Status = Int`
  - Port declaration: `port sendMessage: String -> Cmd msg`
  - Infix declarations. You will probably not need this, while only core packages can define these.

-}
type Declaration
    = FunctionDeclaration Function
    | AliasDeclaration TypeAlias
    | CustomTypeDeclaration Type
    | PortDeclaration
        { name : Node String
        , typeAnnotation : Node TypeAnnotation
        }
    | InfixDeclaration Infix


{-| custom type. For example:

    {-| This is a color
    -}
    type Color
        = Blue
        | Red
-}
type alias Type =
    { documentation : Maybe (Node String)
    , name : Node String
    , generics : List (Node String)
    , constructors : List (Node ValueConstructor)
    }


{-| Syntax for a custom type value constructor.
-}
type alias ValueConstructor =
    { name : Node String
    , arguments : Maybe (Node TypeAnnotation)
    }


{-| For example:

    {-| This is a person
    -}
    type alias Person =
        { name : String
        , age : Int
        }

-}
type alias TypeAlias =
    { documentation : Maybe (Node String)
    , name : Node String
    , generics : List (Node String)
    , typeAnnotation : Node TypeAnnotation
    }


{-| Type annotation for a infix definition
-}
type alias Infix =
    { direction : Node InfixDirection
    , precedence : Node Int
    , operator : Node String
    , function : Node String
    }


{-| Union type for infix direction
-}
type InfixDirection
    = Left
    | Right
    | Non


{-| Type alias for a full function
-}
type alias Function =
    { documentation : Maybe (Node String)
    , signature :
        Maybe
            (Node
                { name : Node String
                , typeAnnotation : Node TypeAnnotation
                }
            )
    , declaration : Node FunctionImplementation
    }


{-| Type alias for a function's implementation
-}
type alias FunctionImplementation =
    { name : Node String
    , arguments : List (Node Pattern)
    , expression : Node Expression
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
  - `ExpressionLetIn`: `let a = 4 in a`
  - `ExpressionCaseOf`: `case a of` followed by pattern matches
  - `ExpressionLambda`: `(\a -> a)`
  - `ExpressionRecord`: `{ name = "text" }`
  - `ExpressionArray`: `[ x, y ]`
  - `ExpressionRecordAccess`: `a.name`
  - `ExpressionRecordAccessFunction`: `.name`
  - `ExpressionRecordUpdate`: `{ a | name = "text" }`
  - `ExpressionGlsl`: `[glsl| ... |]`

-}
type Expression
    = ExpressionUnit
    | ExpressionCall (List (Node Expression))
    | ExpressionInfixOperation String (Node Expression) (Node Expression)
    | ExpressionReference ModuleName String
    | ExpressionIfThenElse (Node Expression) (Node Expression) (Node Expression)
    | ExpressionOperatorFunction String
    | ExpressionInteger Int
    | ExpressionHex Int
    | ExpressionFloat Float
    | ExpressionNegation (Node Expression)
    | ExpressionString { content : String, lineSpread : StringQuotingStyle }
    | ExpressionChar Char
    | ExpressionParenthesized (Node Expression)
    | ExpressionLetIn LetBlock
    | ExpressionCaseOf
        { expression : Node Expression
        , cases : List Case
        }
    | ExpressionLambda Lambda
    | ExpressionRecord (List (Node RecordSetter))
    | ExpressionArray (List (Node Expression))
    | ExpressionRecordAccess (Node Expression) (Node String)
    | ExpressionRecordAccessFunction String
    | ExpressionRecordUpdate (Node Expression) (List (Node RecordSetter))


{-| String literals can be single double-quoted (single line) and triple double-quoted (usually multi-line)?
Used by [`ExpressionString`](#Expression) and [`PatternString`](#Pattern)
-}
type StringQuotingStyle
    = StringSingleQuoted
    | StringTripleQuoted


{-| Expression for setting a record field
-}
type alias RecordSetter =
    ( Node String, Node Expression )


{-| Expression for a let block
-}
type alias LetBlock =
    { declarations : List (Node LetDeclaration)
    , expression : Node Expression
    }


{-| Union type for all possible declarations in a let block
-}
type LetDeclaration
    = LetFunction Function
    | LetDestructuring (Node Pattern) (Node Expression)


{-| Expression for a lambda
-}
type alias Lambda =
    { args : List (Node Pattern)
    , expression : Node Expression
    }


{-| A case in a case block
-}
type alias Case =
    ( Node Pattern, Node Expression )


{-| Custom type for different type annotations. For example:

  - `TypeAnnotationVariable`: `a`
  - `TypeAnnotationConstruct`: `Maybe (Int -> String)`
  - `TypeAnnotationUnit`: `()`
  - `TypeAnnotationParenthesized`: `(a -> b)`
  - `TypeAnnotationRecord`: `{ name : String}`
  - `TypeAnnotationRecordExtension`: `{ a | name : String}`
  - `TypeAnnotationFunction`: `Int -> String`

-}
type TypeAnnotation
    = TypeAnnotationVariable String
    | TypeAnnotationConstruct (Node ( ModuleName, String )) (List (Node TypeAnnotation))
    | TypeAnnotationUnit
    | TypeAnnotationParenthesized (Node TypeAnnotation)
    | TypeAnnotationRecord (List (Node TypeAnnotationRecordField))
    | TypeAnnotationRecordExtension (Node String) (Node (List (Node TypeAnnotationRecordField)))
    | TypeAnnotationFunction (Node TypeAnnotation) (Node TypeAnnotation)


{-| Single field of a record. A name and its type.
-}
type alias TypeAnnotationRecordField =
    ( Node String, Node TypeAnnotation )


{-| Custom type for all patterns such as:

  - `PatternIgnored`: `_`
  - `PatternUnit`: `()`
  - `PatternChar`: `'c'`
  - `PatternString`: `"hello"`
  - `PatternInt`: `42`
  - `PatternHex`: `0x11`
  - `PatternTuple`: `(a, b)`
  - `PatternRecord`: `{name, age}`
  - `PatternListCons`: `x :: xs`
  - `PatternListExact`: `[ x, y ]`
  - `PatternVariable`: `x`
  - `PatternVariant`: `Just _`
  - `PatternAs`: `_ as x`
  - `PatternParenthesized`: `( _ )`

-}
type Pattern
    = PatternIgnored
    | PatternUnit
    | PatternChar Char
    | PatternString { content : String, lineSpread : StringQuotingStyle }
    | PatternInt Int
    | PatternHex Int
    | PatternRecord (List { name : Node String, value : Maybe (Node Pattern) })
    | PatternListCons (Node Pattern) (Node Pattern)
    | PatternListExact (List (Node Pattern))
    | PatternVariable String
    | PatternVariant QualifiedNameRef (Maybe (Node Pattern))
    | PatternAs { pattern : Node Pattern, variable : Node String }
    | PatternParenthesized (Node Pattern)


{-| Qualified name reference such as `Maybe.Just`.
-}
type alias QualifiedNameRef =
    { moduleName : List String
    , name : String
    }


{-|  An element of the AST (Abstract Syntax Tree).

The purpose of this type is to add the information of the [`Range`](#Range), i.e. where in the source code the
element of the tree was found.

-}
type Node a
    = Node Range a


{-| Combine two nodes, constructing a new node which will have the outer most range of the child nodes
-}
nodeCombine : (Node a -> Node b -> c) -> Node a -> Node b -> Node c
nodeCombine f ((Node aRange _) as a) ((Node bRange _) as b) =
    Node
        { start = aRange.start, end = bRange.end }
        (f a b)


{-| Map the value within a node leaving the range untouched
-}
nodeMap : (a -> b) -> Node a -> Node b
nodeMap f (Node r a) =
    Node r (f a)


{-| Extract the range out of a `Node a`
-}
nodeRange : Node a -> Range
nodeRange (Node r _) =
    r


{-| Extract the value (`a`) out of a `Node a`
-}
nodeValue : Node a -> a
nodeValue (Node _ v) =
    v


{-| Source location
-}
type alias Location =
    { row : Int
    , column : Int
    }


{-| Range for a piece of code with a start and end
-}
type alias Range =
    { start : Location
    , end : Location
    }
