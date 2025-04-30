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
    { atDocsLines : List (List String), comments : List (GrenSyntax.Node String) }
    -> GrenSyntax.Node GrenSyntax.Exposing
    -> Print
moduleExposing context moduleExposingNode =
    \state ->
        GrenPrintDefunctionalized.moduleExposing context moduleExposingNode
            |> Print.toStringWithIndent state.indent


{-| Print an [`GrenSyntax.Module`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Module#Module)
(confusingly, that's their name for only the `module X exposing (Y)` lines)
-}
moduleHeader :
    { atDocsLines : List (List String), comments : List (GrenSyntax.Node String) }
    -> GrenSyntax.Module
    -> Print
moduleHeader context syntaxModuleHeader =
    \state ->
        GrenPrintDefunctionalized.moduleHeader context syntaxModuleHeader
            |> Print.toStringWithIndent state.indent


{-| Print a set of [`GrenSyntax.Import`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Import#Import)s
-}
imports :
    List (GrenSyntax.Node String)
    -> List (GrenSyntax.Node GrenSyntax.Import)
    -> Print
imports syntaxComments syntaxImports =
    \state ->
        GrenPrintDefunctionalized.imports syntaxComments syntaxImports
            |> Print.toStringWithIndent state.indent


{-| Print a single [`GrenSyntax.Import`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Import#Import)
-}
import_ :
    List (GrenSyntax.Node String)
    -> GrenSyntax.Node GrenSyntax.Import
    -> Print
import_ syntaxComments importNode =
    \state ->
        GrenPrintDefunctionalized.import_ syntaxComments importNode
            |> Print.toStringWithIndent state.indent


{-| Print the stuff after `exposing` in an import.
For module header exposing: [`moduleExposing`](#moduleExposing)
-}
importExposing :
    List (GrenSyntax.Node String)
    -> GrenSyntax.Node GrenSyntax.Exposing
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


{-| Print an [`GrenSyntax.ModuleName`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-ModuleName#ModuleName)
-}
moduleName : GrenSyntax.ModuleName -> Print
moduleName syntaxModuleName =
    \_ ->
        GrenPrintDefunctionalized.moduleName syntaxModuleName


{-| Print a single [`GrenSyntax.TopLevelExpose`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Exposing#TopLevelExpose)
-}
expose : GrenSyntax.TopLevelExpose -> Print
expose syntaxExpose =
    \_ ->
        GrenPrintDefunctionalized.expose syntaxExpose


{-| Print an [`GrenSyntax.Pattern`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Pattern#Pattern)
-}
patternNotParenthesized :
    List (GrenSyntax.Node String)
    -> GrenSyntax.Node GrenSyntax.Pattern
    -> Print
patternNotParenthesized syntaxComments patternNode =
    \state ->
        GrenPrintDefunctionalized.patternNotParenthesized syntaxComments patternNode
            |> Print.toStringWithIndent state.indent


{-| Print a name with its qualification (`[]` for no qualification)
-}
qualifiedReference : { qualification : List String, name : String } -> Print
qualifiedReference syntaxReference =
    \_ ->
        GrenPrintDefunctionalized.qualifiedReference syntaxReference


{-| Print an [`GrenSyntax.TypeAnnotation`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-TypeAnnotation#TypeAnnotation)
-}
typeNotParenthesized :
    List (GrenSyntax.Node String)
    -> GrenSyntax.Node GrenSyntax.TypeAnnotation
    -> Print
typeNotParenthesized syntaxComments typeNode =
    \state ->
        GrenPrintDefunctionalized.typeNotParenthesized syntaxComments typeNode
            |> Print.toStringWithIndent state.indent


{-| Print a list of [`GrenSyntax.Declaration`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Declaration#Declaration)s
and comments in between
-}
declarations :
    { portDocumentationComments : List (GrenSyntax.Node String)
    , comments : List (GrenSyntax.Node String)
    , previousEnd : GrenSyntax.Location
    }
    -> List (GrenSyntax.Node GrenSyntax.Declaration)
    -> Print
declarations context syntaxDeclarations =
    \state ->
        GrenPrintDefunctionalized.declarations context syntaxDeclarations
            |> Print.toStringWithIndent state.indent


{-| Print an [`GrenSyntax.Declaration`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Declaration#Declaration)
-}
declaration :
    { portDocumentationComment : Maybe (GrenSyntax.Node String)
    , comments : List (GrenSyntax.Node String)
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
    List (GrenSyntax.Node String)
    ->
        { name : GrenSyntax.Node String
        , typeAnnotation : GrenSyntax.Node GrenSyntax.TypeAnnotation
        }
    -> Print
declarationSignature syntaxComments signature =
    \state ->
        GrenPrintDefunctionalized.declarationSignature syntaxComments signature
            |> Print.toStringWithIndent state.indent


{-| Print a `port` [`GrenSyntax.Declaration`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Declaration#Declaration)
-}
declarationPort :
    { documentationComment : Maybe (GrenSyntax.Node String)
    , comments : List (GrenSyntax.Node String)
    }
    ->
        { name : GrenSyntax.Node String
        , typeAnnotation : GrenSyntax.Node GrenSyntax.TypeAnnotation
        }
    -> Print
declarationPort syntaxComments signature =
    \state ->
        GrenPrintDefunctionalized.declarationPort syntaxComments signature
            |> Print.toStringWithIndent state.indent


{-| Print an [`GrenSyntax.TypeAlias`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-TypeAlias#TypeAlias) declaration
-}
declarationTypeAlias :
    List (GrenSyntax.Node String)
    -> GrenSyntax.TypeAlias
    -> Print
declarationTypeAlias syntaxComments syntaxTypeAliasDeclaration =
    \state ->
        GrenPrintDefunctionalized.declarationTypeAlias syntaxComments syntaxTypeAliasDeclaration
            |> Print.toStringWithIndent state.indent


{-| Print an [`GrenSyntax.Type`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Type#Type) declaration
-}
declarationChoiceType :
    List (GrenSyntax.Node String)
    -> GrenSyntax.Type
    -> Print
declarationChoiceType syntaxComments syntaxChoiceTypeDeclaration =
    \state ->
        GrenPrintDefunctionalized.declarationChoiceType syntaxComments syntaxChoiceTypeDeclaration
            |> Print.toStringWithIndent state.indent


{-| Print an [`GrenSyntax.Infix`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Infix#Infix) declaration
-}
declarationInfix : GrenSyntax.Infix -> Print
declarationInfix syntaxInfixDeclaration =
    \state ->
        GrenPrintDefunctionalized.declarationInfix syntaxInfixDeclaration
            |> Print.toStringWithIndent state.indent


{-| Print an [`GrenSyntax.Function`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Expression#Function) declaration
-}
declarationExpression :
    List (GrenSyntax.Node String)
    -> GrenSyntax.Function
    -> Print
declarationExpression syntaxComments syntaxExpressionDeclaration =
    \state ->
        GrenPrintDefunctionalized.declarationExpression syntaxComments syntaxExpressionDeclaration
            |> Print.toStringWithIndent state.indent


{-| Print an [`GrenSyntax.Expression`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Expression#Expression)
-}
expressionNotParenthesized :
    List (GrenSyntax.Node String)
    -> GrenSyntax.Node GrenSyntax.Expression
    -> Print
expressionNotParenthesized syntaxComments expressionNode =
    \state ->
        GrenPrintDefunctionalized.expressionNotParenthesized syntaxComments expressionNode
            |> Print.toStringWithIndent state.indent


{-| Print a single [`GrenSyntax.Case`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Expression#Case)
-}
case_ :
    List (GrenSyntax.Node String)
    -> GrenSyntax.Case
    -> Print
case_ syntaxComments syntaxCase =
    \state ->
        GrenPrintDefunctionalized.case_ syntaxComments syntaxCase
            |> Print.toStringWithIndent state.indent
