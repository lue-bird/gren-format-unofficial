module GrenSyntax exposing
    ( File, ModuleName, Import
    , DefaultModuleData, EffectModuleData, Module(..)
    , Exposing(..), TopLevelExpose(..), ExposedType
    , Declaration(..), ValueOrFunctionDeclarationInfo, ChoiceTypeDeclarationInfo, TypeAliasDeclarationInfo, InfixDeclarationInfo, InfixDirection(..)
    , Pattern(..), Expression(..), LetDeclaration(..), StringQuotingStyle(..), TypeAnnotation(..)
    , Range, Location, Node, nodeCombine, nodeMap, nodeRange, nodeValue
    )

{-| Gren syntax tree

@docs File, ModuleName, Import
@docs DefaultModuleData, EffectModuleData, Module
@docs Exposing, TopLevelExpose, ExposedType
@docs Declaration, ValueOrFunctionDeclarationInfo, ChoiceTypeDeclarationInfo, TypeAliasDeclarationInfo, InfixDeclarationInfo, InfixDirection
@docs Pattern, Expression, LetDeclaration, StringQuotingStyle, TypeAnnotation
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

  - Function/value declaration: `add x y = x + y`
  - Custom type declaration: `type Color = Blue | Red`
  - Type alias declaration: `type alias Status = Int`
  - Port declaration: `port sendMessage: String -> Cmd msg`
  - Infix declaration. You will probably not need this, while only core packages can define these.

-}
type Declaration
    = ValueOrFunctionDeclaration ValueOrFunctionDeclarationInfo
    | AliasDeclaration TypeAliasDeclarationInfo
    | ChoiceTypeDeclaration ChoiceTypeDeclarationInfo
    | PortDeclaration
        { name : Node String
        , typeAnnotation : Node TypeAnnotation
        }
    | InfixDeclaration InfixDeclarationInfo


{-| custom type. For example:

    {-| This is a color
    -}
    type Color
        = Blue
        | Red

-}
type alias ChoiceTypeDeclarationInfo =
    { documentation : Maybe (Node String)
    , name : Node String
    , generics : List (Node String)
    , constructors :
        List
            (Node
                { name : Node String
                , value : Maybe (Node TypeAnnotation)
                }
            )
    }


{-| For example:

    {-| This is a person
    -}
    type alias Person =
        { name : String
        , age : Int
        }

-}
type alias TypeAliasDeclarationInfo =
    { documentation : Maybe (Node String)
    , name : Node String
    , generics : List (Node String)
    , typeAnnotation : Node TypeAnnotation
    }


{-| Type annotation for a infix definition
-}
type alias InfixDeclarationInfo =
    { direction : Node InfixDirection
    , precedence : Node Int
    , operator : Node String
    , function : Node String
    }


{-| Infix operator associativity
-}
type InfixDirection
    = Left
    | Right
    | Non


{-| value/full function declaration
-}
type alias ValueOrFunctionDeclarationInfo =
    { documentation : Maybe (Node String)
    , signature :
        Maybe
            (Node
                { name : Node String
                , typeAnnotation : Node TypeAnnotation
                }
            )
    , declaration :
        Node
            { name : Node String
            , parameters : List (Node Pattern)
            , expression : Node Expression
            }
    }


{-| A value or function:

  - `ExpressionUnit`: `()`
  - `ExpressionInteger`: `-42`
  - `ExpressionHex`: `0x1F`
  - `ExpressionFloat`: `42.0`
  - `ExpressionChar`: `'a'`
  - `ExpressionString`: `"text"`
  - `ExpressionOperatorFunction`: `(+)`
  - `ExpressionNegation`: `-a`
  - `ExpressionParenthesized`: `(a)`
  - `ExpressionCall`: `add a b`
  - `ExpressionInfixOperation`: `a + b`
  - `ExpressionReference`: `add` or `Basics.True` or `portCmd`
  - `ExpressionIfThenElse`: `if a then b else c`
  - `ExpressionLetIn`: `let a = 4 in a`
  - `ExpressionCaseOf`: `case a of` followed by pattern matches
  - `ExpressionLambda`: `(\a -> a)`
  - `ExpressionRecord`: `{ name = "text" }`
  - `ExpressionArray`: `[ x, y ]`
  - `ExpressionRecordAccess`: `a.name`
  - `ExpressionRecordAccessFunction`: `.name`
  - `ExpressionRecordUpdate`: `{ Some.record | name = "text" }`

-}
type Expression
    = ExpressionUnit
    | ExpressionInteger Int
    | ExpressionHex Int
    | ExpressionFloat Float
    | ExpressionChar Char
    | ExpressionString { content : String, quotingStyle : StringQuotingStyle }
    | ExpressionOperatorFunction String
    | ExpressionNegation (Node Expression)
    | ExpressionParenthesized (Node Expression)
    | ExpressionCall (List (Node Expression))
    | ExpressionInfixOperation
        { operator : String
        , left : Node Expression
        , right : Node Expression
        }
    | ExpressionReference { qualification : ModuleName, name : String }
    | ExpressionIfThenElse
        { condition : Node Expression
        , onTrue : Node Expression
        , onFalse : Node Expression
        }
    | ExpressionLetIn
        { declarations : List (Node LetDeclaration)
        , result : Node Expression
        }
    | ExpressionCaseOf
        { expression : Node Expression
        , cases :
            List
                { pattern : Node Pattern
                , result : Node Expression
                }
        }
    | ExpressionLambda
        { parameters : List (Node Pattern)
        , result : Node Expression
        }
    | ExpressionRecord
        (List
            (Node
                { name : Node String
                , value : Node Expression
                }
            )
        )
    | ExpressionArray (List (Node Expression))
    | ExpressionRecordAccess
        { record : Node Expression
        , field : Node String
        }
    | ExpressionRecordAccessFunction String
    | ExpressionRecordUpdate
        { record : Node Expression
        , fields :
            List
                (Node
                    { name : Node String
                    , value : Node Expression
                    }
                )
        }


{-| String literals can be single double-quoted (single line) and triple double-quoted (usually multi-line)?
Used by [`ExpressionString`](#Expression) and [`PatternString`](#Pattern)
-}
type StringQuotingStyle
    = StringSingleQuoted
    | StringTripleQuoted


{-| Union type for all possible declarations in a let block
-}
type LetDeclaration
    = LetFunction ValueOrFunctionDeclarationInfo
    | LetDestructuring (Node Pattern) (Node Expression)


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
    | TypeAnnotationConstruct (Node { qualification : ModuleName, name : String }) (List (Node TypeAnnotation))
    | TypeAnnotationUnit
    | TypeAnnotationParenthesized (Node TypeAnnotation)
    | TypeAnnotationRecord
        (List
            (Node
                { name : Node String
                , value : Node TypeAnnotation
                }
            )
        )
    | TypeAnnotationRecordExtension
        { recordVariable : Node String
        , fields :
            Node
                (List
                    (Node
                        { name : Node String
                        , value : Node TypeAnnotation
                        }
                    )
                )
        }
    | TypeAnnotationFunction
        { input : Node TypeAnnotation
        , output : Node TypeAnnotation
        }


{-| Custom type for all patterns such as:

  - `PatternIgnored`: `_` or `_name`
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
    = PatternIgnored (Maybe String)
    | PatternUnit
    | PatternChar Char
    | PatternString { content : String, quotingStyle : StringQuotingStyle }
    | PatternInt Int
    | PatternHex Int
    | PatternRecord (List { name : Node String, value : Maybe (Node Pattern) })
    | PatternListCons { head : Node Pattern, tail : Node Pattern }
    | PatternListExact (List (Node Pattern))
    | PatternVariable String
    | PatternVariant
        { qualification : List String
        , name : String
        , value : Maybe (Node Pattern)
        }
    | PatternAs { pattern : Node Pattern, variable : Node String }
    | PatternParenthesized (Node Pattern)


{-| An element of the AST (Abstract Syntax Tree).

The purpose of this type is to add the information of the [`Range`](#Range), i.e. where in the source code the
element of the tree was found.

-}
type alias Node value =
    { range : Range, value : value }


{-| Combine two nodes, constructing a new node which will have the outer most range of the child nodes
-}
nodeCombine : (Node a -> Node b -> c) -> Node a -> Node b -> Node c
nodeCombine f a b =
    { range = { start = a.range.start, end = b.range.end }, value = f a b }


{-| Map the value within a node leaving the range untouched
-}
nodeMap : (a -> b) -> Node a -> Node b
nodeMap f node =
    { range = node.range, value = f node.value }


{-| Extract the range out of a `Node a`
-}
nodeRange : Node value_ -> Range
nodeRange node =
    node.range


{-| Extract the value (`a`) out of a `Node a`
-}
nodeValue : Node value -> value
nodeValue node =
    node.value


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
