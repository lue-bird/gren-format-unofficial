module GrenSyntax exposing
    ( File, Import
    , DefaultModuleData, EffectModuleData, Module(..)
    , Declaration(..)
    )

{-| grain syntax tree

@docs File, Import
@docs DefaultModuleData, EffectModuleData, Module
@docs Declaration

-}

import Gren.Syntax.Exposing
import Gren.Syntax.Expression
import Gren.Syntax.Infix
import Gren.Syntax.ModuleName
import Gren.Syntax.Node
import Gren.Syntax.Type
import Gren.Syntax.TypeAlias
import Gren.Syntax.TypeAnnotation


{-| module header. For example:

    module Html.Attributes exposing (style)

-}
type Module
    = NormalModule DefaultModuleData
    | PortModule DefaultModuleData
    | EffectModule EffectModuleData


{-| Data for a default default
-}
type alias DefaultModuleData =
    { moduleName : Gren.Syntax.Node.Node Gren.Syntax.ModuleName.ModuleName
    , exposingList : Gren.Syntax.Node.Node Gren.Syntax.Exposing.Exposing
    }


{-| Data for an effect module
-}
type alias EffectModuleData =
    { moduleName : Gren.Syntax.Node.Node Gren.Syntax.ModuleName.ModuleName
    , exposingList : Gren.Syntax.Node.Node Gren.Syntax.Exposing.Exposing
    , command : Maybe (Gren.Syntax.Node.Node String)
    , subscription : Maybe (Gren.Syntax.Node.Node String)
    }


{-| Type annotation for a file
-}
type alias File =
    { moduleDefinition : Gren.Syntax.Node.Node Module
    , imports : List (Gren.Syntax.Node.Node Import)
    , declarations : List (Gren.Syntax.Node.Node Declaration)
    , comments : List (Gren.Syntax.Node.Node String)
    }


{-| For example:

    import Html.Attributes as HA exposing (style)

-}
type alias Import =
    { moduleName : Gren.Syntax.Node.Node Gren.Syntax.ModuleName.ModuleName
    , moduleAlias : Maybe (Gren.Syntax.Node.Node Gren.Syntax.ModuleName.ModuleName)
    , exposingList : Maybe (Gren.Syntax.Node.Node Gren.Syntax.Exposing.Exposing)
    }


{-| A module-level declaration. Can be one of the following:

  - Functions: `add x y = x + y`
  - Custom types: `type Color = Blue | Red`
  - Type aliases: `type alias Status = Int`
  - Port declaration: `port sendMessage: String -> Cmd msg`
  - Infix declarations. You will probably not need this, while only core packages can define these.

-}
type Declaration
    = FunctionDeclaration Gren.Syntax.Expression.Function
    | AliasDeclaration Gren.Syntax.TypeAlias.TypeAlias
    | CustomTypeDeclaration Gren.Syntax.Type.Type
    | PortDeclaration
        { name : Gren.Syntax.Node.Node String
        , typeAnnotation : Gren.Syntax.Node.Node Gren.Syntax.TypeAnnotation.TypeAnnotation
        }
    | InfixDeclaration Gren.Syntax.Infix.Infix
