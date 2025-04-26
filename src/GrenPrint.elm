module GrenPrint exposing
    ( toString, Print
    , module_
    , moduleHeader, moduleExposing, expose, imports, import_, importExposing, moduleLevelComments, comments, comment
    , declarations, declaration, declarationChoiceType, declarationSignature, declarationExpression, declarationInfix, declarationPort, declarationTypeAlias
    , expressionNotParenthesized, case_, patternNotParenthesized, typeNotParenthesized
    , moduleName, qualifiedReference
    )

{-| Pretty printing an [`gren-syntax`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/) tree
in a way consistent with [`gren-format`](https://github.com/avh4/gren-format).

@docs toString, Print
@docs module_

That's all you'll need most of the time.

Sometimes it's useful to print only some part of the syntax,
to, say, display only an expression in an article
or reformat only the touched declarations on save.

@docs moduleHeader, moduleExposing, expose, imports, import_, importExposing, moduleLevelComments, comments, comment
@docs declarations, declaration, declarationChoiceType, declarationSignature, declarationExpression, declarationInfix, declarationPort, declarationTypeAlias
@docs expressionNotParenthesized, case_, patternNotParenthesized, typeNotParenthesized
@docs moduleName, qualifiedReference

If you need other syntax printing like for collapsible comments to be exposed,
[open an issue](https://github.com/lue-bird/gren-format-unofficial/issues/new)

-}

import Gren.Syntax.Exposing
import Gren.Syntax.Expression
import Gren.Syntax.Infix
import Gren.Syntax.ModuleName
import Gren.Syntax.Node
import Gren.Syntax.Pattern
import Gren.Syntax.Range
import Gren.Syntax.Type
import Gren.Syntax.TypeAlias
import Gren.Syntax.TypeAnnotation
import GrenPrintDefunctionalized
import GrenSyntax
import Print


{-| Pretty printable intermediate representation.
See [`toString`](#toString)
-}
type alias Print =
    { indent : Int } -> String


{-| All other helpers in this module produce a [`Print`](#Print)
which you can in the end convert to a String with [`toString`](#toString)
-}
toString : Print -> String
toString print =
    print { indent = 0 }


{-| Print an [`GrenSyntax.File`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-File#File)
-}
module_ : GrenSyntax.File -> Print
module_ syntaxModule =
    \state ->
        GrenPrintDefunctionalized.module_ syntaxModule
            |> Print.toStringWithIndent state.indent


{-| Print the stuff after `exposing` in a module header.
For import exposing: [`importExposing`](#importExposing)
-}
moduleExposing :
    { atDocsLines : List (List String), comments : List (Gren.Syntax.Node.Node String) }
    -> Gren.Syntax.Node.Node Gren.Syntax.Exposing.Exposing
    -> Print
moduleExposing context moduleExposingNode =
    \state ->
        GrenPrintDefunctionalized.moduleExposing context moduleExposingNode
            |> Print.toStringWithIndent state.indent


{-| Print an [`GrenSyntax.Module`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Module#Module)
(confusingly, that's their name for only the `module X exposing (Y)` lines)
-}
moduleHeader :
    { atDocsLines : List (List String), comments : List (Gren.Syntax.Node.Node String) }
    -> GrenSyntax.Module
    -> Print
moduleHeader context syntaxModuleHeader =
    \state ->
        GrenPrintDefunctionalized.moduleHeader context syntaxModuleHeader
            |> Print.toStringWithIndent state.indent


{-| Print a set of [`GrenSyntax.Import`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Import#Import)s
-}
imports :
    List (Gren.Syntax.Node.Node String)
    -> List (Gren.Syntax.Node.Node GrenSyntax.Import)
    -> Print
imports syntaxComments syntaxImports =
    \state ->
        GrenPrintDefunctionalized.imports syntaxComments syntaxImports
            |> Print.toStringWithIndent state.indent


{-| Print a single [`GrenSyntax.Import`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Import#Import)
-}
import_ :
    List (Gren.Syntax.Node.Node String)
    -> Gren.Syntax.Node.Node GrenSyntax.Import
    -> Print
import_ syntaxComments importNode =
    \state ->
        GrenPrintDefunctionalized.import_ syntaxComments importNode
            |> Print.toStringWithIndent state.indent


{-| Print the stuff after `exposing` in an import.
For module header exposing: [`moduleExposing`](#moduleExposing)
-}
importExposing :
    List (Gren.Syntax.Node.Node String)
    -> Gren.Syntax.Node.Node Gren.Syntax.Exposing.Exposing
    -> Print
importExposing syntaxComments importExposingNode =
    \state ->
        GrenPrintDefunctionalized.importExposing syntaxComments importExposingNode
            |> Print.toStringWithIndent state.indent


{-| Print `--` or `{- -}` comments placed _within a declaration_.
For top-level comments: [`moduleLevelComments`](#moduleLevelComments)
-}
comments : List String -> Print
comments syntaxComments =
    \state ->
        GrenPrintDefunctionalized.comments syntaxComments
            |> Print.toStringWithIndent state.indent


{-| Print `--` or `{- -}` comments placed outside of declarations at the top level.
For comments within a declaration: [`comments`](#comments)
-}
moduleLevelComments : List String -> Print
moduleLevelComments syntaxComments =
    \state ->
        GrenPrintDefunctionalized.moduleLevelComments syntaxComments
            |> Print.toStringWithIndent state.indent


{-| Print a single `--` or `{- -}` comment.
-}
comment : String -> Print
comment syntaxComment =
    \state ->
        GrenPrintDefunctionalized.comment syntaxComment
            |> Print.toStringWithIndent state.indent


{-| Print an [`Gren.Syntax.ModuleName.ModuleName`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-ModuleName#ModuleName)
-}
moduleName : Gren.Syntax.ModuleName.ModuleName -> Print
moduleName syntaxModuleName =
    \_ ->
        GrenPrintDefunctionalized.moduleName syntaxModuleName


{-| Print a single [`Gren.Syntax.Exposing.TopLevelExpose`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Exposing#TopLevelExpose)
-}
expose : Gren.Syntax.Exposing.TopLevelExpose -> Print
expose syntaxExpose =
    \_ ->
        GrenPrintDefunctionalized.expose syntaxExpose


{-| Print an [`Gren.Syntax.Pattern.Pattern`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Pattern#Pattern)
-}
patternNotParenthesized :
    List (Gren.Syntax.Node.Node String)
    -> Gren.Syntax.Node.Node Gren.Syntax.Pattern.Pattern
    -> Print
patternNotParenthesized syntaxComments patternNode =
    \state ->
        GrenPrintDefunctionalized.patternNotParenthesized syntaxComments patternNode
            |> Print.toStringWithIndent state.indent


{-| Print a name with its qualification (`[]` for no qualification)
-}
qualifiedReference : { qualification : List String, unqualified : String } -> Print
qualifiedReference syntaxReference =
    \_ ->
        GrenPrintDefunctionalized.qualifiedReference syntaxReference


{-| Print an [`Gren.Syntax.TypeAnnotation.TypeAnnotation`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-TypeAnnotation#TypeAnnotation)
-}
typeNotParenthesized :
    List (Gren.Syntax.Node.Node String)
    -> Gren.Syntax.Node.Node Gren.Syntax.TypeAnnotation.TypeAnnotation
    -> Print
typeNotParenthesized syntaxComments typeNode =
    \state ->
        GrenPrintDefunctionalized.typeNotParenthesized syntaxComments typeNode
            |> Print.toStringWithIndent state.indent


{-| Print a list of [`GrenSyntax.Declaration`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Declaration#Declaration)s
and comments in between
-}
declarations :
    { portDocumentationComments : List (Gren.Syntax.Node.Node String)
    , comments : List (Gren.Syntax.Node.Node String)
    , previousEnd : Gren.Syntax.Range.Location
    }
    -> List (Gren.Syntax.Node.Node GrenSyntax.Declaration)
    -> Print
declarations context syntaxDeclarations =
    \state ->
        GrenPrintDefunctionalized.declarations context syntaxDeclarations
            |> Print.toStringWithIndent state.indent


{-| Print an [`GrenSyntax.Declaration`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Declaration#Declaration)
-}
declaration :
    { portDocumentationComment : Maybe (Gren.Syntax.Node.Node String)
    , comments : List (Gren.Syntax.Node.Node String)
    }
    -> GrenSyntax.Declaration
    -> Print
declaration syntaxComments syntaxDeclaration =
    \state ->
        GrenPrintDefunctionalized.declaration syntaxComments syntaxDeclaration
            |> Print.toStringWithIndent state.indent


{-| Print an [`Gren.Syntax.Signature.Signature`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Signature#Signature)
as `name : Type`
-}
declarationSignature :
    List (Gren.Syntax.Node.Node String)
    ->
        { name : Gren.Syntax.Node.Node String
        , typeAnnotation : Gren.Syntax.Node.Node Gren.Syntax.TypeAnnotation.TypeAnnotation
        }
    -> Print
declarationSignature syntaxComments signature =
    \state ->
        GrenPrintDefunctionalized.declarationSignature syntaxComments signature
            |> Print.toStringWithIndent state.indent


{-| Print a `port` [`GrenSyntax.Declaration`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Declaration#Declaration)
-}
declarationPort :
    { documentationComment : Maybe (Gren.Syntax.Node.Node String)
    , comments : List (Gren.Syntax.Node.Node String)
    }
    ->
        { name : Gren.Syntax.Node.Node String
        , typeAnnotation : Gren.Syntax.Node.Node Gren.Syntax.TypeAnnotation.TypeAnnotation
        }
    -> Print
declarationPort syntaxComments signature =
    \state ->
        GrenPrintDefunctionalized.declarationPort syntaxComments signature
            |> Print.toStringWithIndent state.indent


{-| Print an [`Gren.Syntax.TypeAlias.TypeAlias`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-TypeAlias#TypeAlias) declaration
-}
declarationTypeAlias :
    List (Gren.Syntax.Node.Node String)
    -> Gren.Syntax.TypeAlias.TypeAlias
    -> Print
declarationTypeAlias syntaxComments syntaxTypeAliasDeclaration =
    \state ->
        GrenPrintDefunctionalized.declarationTypeAlias syntaxComments syntaxTypeAliasDeclaration
            |> Print.toStringWithIndent state.indent


{-| Print an [`Gren.Syntax.Type.Type`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Type#Type) declaration
-}
declarationChoiceType :
    List (Gren.Syntax.Node.Node String)
    -> Gren.Syntax.Type.Type
    -> Print
declarationChoiceType syntaxComments syntaxChoiceTypeDeclaration =
    \state ->
        GrenPrintDefunctionalized.declarationChoiceType syntaxComments syntaxChoiceTypeDeclaration
            |> Print.toStringWithIndent state.indent


{-| Print an [`Gren.Syntax.Infix.Infix`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Infix#Infix) declaration
-}
declarationInfix : Gren.Syntax.Infix.Infix -> Print
declarationInfix syntaxInfixDeclaration =
    \state ->
        GrenPrintDefunctionalized.declarationInfix syntaxInfixDeclaration
            |> Print.toStringWithIndent state.indent


{-| Print an [`Gren.Syntax.Expression.Function`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Expression#Function) declaration
-}
declarationExpression :
    List (Gren.Syntax.Node.Node String)
    -> Gren.Syntax.Expression.Function
    -> Print
declarationExpression syntaxComments syntaxExpressionDeclaration =
    \state ->
        GrenPrintDefunctionalized.declarationExpression syntaxComments syntaxExpressionDeclaration
            |> Print.toStringWithIndent state.indent


{-| Print an [`Gren.Syntax.Expression.Expression`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Expression#Expression)
-}
expressionNotParenthesized :
    List (Gren.Syntax.Node.Node String)
    -> Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
    -> Print
expressionNotParenthesized syntaxComments expressionNode =
    \state ->
        GrenPrintDefunctionalized.expressionNotParenthesized syntaxComments expressionNode
            |> Print.toStringWithIndent state.indent


{-| Print a single [`Gren.Syntax.Expression.Case`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Expression#Case)
-}
case_ :
    List (Gren.Syntax.Node.Node String)
    -> Gren.Syntax.Expression.Case
    -> Print
case_ syntaxComments syntaxCase =
    \state ->
        GrenPrintDefunctionalized.case_ syntaxComments syntaxCase
            |> Print.toStringWithIndent state.indent
