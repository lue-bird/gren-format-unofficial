module GrenParserLenient exposing
    ( Parser, run, module_
    , Comments, commentsToList
    , expose, exposing_
    , moduleHeader, import_, declarations, declaration
    , type_, pattern, expression
    , multiLineComment, singleLineComment, whitespaceAndComments
    , moduleName, nameLowercase, nameUppercase
    , RopeFilled(..)
    )

{-| Like [`Gren.Parser`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Parser)
but able to parse badly indented code and similar somewhat incorrect syntax,
similar to gren-format.

This is **not** a fault-tolerant parser!
So if you write something it can't recognize in a file,
the whole thing will fail.

Also, precise ranges of some parts in in the parsed result are not reliable.
Though they will still be correct when viewed relative to each other
and will tell you how many lines they span.
This means [`GrenPrint`](GrenPrint)
can pick this up and format it in a way compatible
with the compiler or [`Gren.Parser`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Parser).

Some additional lenient parsing:

  - `f | g` → `f |> g`

  - `a != b` or `a !== b` → `a /= b`

  - `a === b` → `a == b`

  - `a ** b` → `a ^ b`

  - `\a => b` or `\a. b` → `\a -> b`

  - `case ... of a. b` or `case ... of a b` → `case ... of a -> b`

  - merges consecutive `,` in record, list or explicit exposing

  - removes extra `,` before first record field, list element or expose

  - merges consecutive `|` in choice type declaration

  - removes remove extra `|` before first variant declaration

  - merges consecutive `->` in function type

  - `port module` to `module` if no ports exist and the other way round

  - `(...)` → `(..)` in exposing and type expose that includes its variants

  - removes empty `exposing ()` after import

  - expression record field name-value separators

    `{ name : value }` or `{ name value }`

    → `{ name = value }`

  - type record field name-value separators

    `{ name = value }` or `{ name value }`

    → `{ name : value }`

  - expands expression record field punning

    `{ field }` → `{ field = field }`

  - `->` to `=` in an expression declaration and let expression declaration

    `function parameters -> result`

    → `function parameters = result`

  - corrects names that collide with keywords

    `Html.Attributes.type` → `Html.Attributes.type_`

  - allows omitting the name before the type in an expression declaration or let expression declaration

        : Type
        function parameters =
            result

    →

        function : Type
        function parameters =
            result

  - allows matching everything before

        3 |> String.toInt case
            Nothing ->
                0

            Just n ->
                n

    →

        case 3 |> String.toInt of
            Nothing ->
                0

            Just n ->
                n

  - moves import statements anywhere at the top level to the import section

@docs Parser, run, module_

That's all you'll need most of the time.

Sometimes it's useful to parse only some part of the syntax,
to, say, display only an expression in an article
or reparse only the touched declarations on save.

@docs Comments, commentsToList
@docs expose, exposing_
@docs moduleHeader, import_, declarations, declaration
@docs type_, pattern, expression


### whitespace

@docs multiLineComment, singleLineComment, whitespaceAndComments


### low-level

@docs moduleName, nameLowercase, nameUppercase
@docs RopeFilled

-}

import Char.Extra
import GrenSyntax
import ParserFast


{-| Can turn a String into syntax or Nothing.
See [`GrenParserLenient.run`](#run)

(This is not related to [`gren/parser`](https://gren-lang.org/packages/gren/parser/latest/).
[Open an issue](https://github.com/lue-bird/gren-format-unofficial/issues/new)
if you need a way to covert to that)

-}
type alias Parser a =
    ParserFast.Parser a


{-| Turn a given source String into `Just` the parsed syntax
or `Nothing` if any unrecognizable part is found.
-}
run : Parser a -> String -> Maybe a
run syntaxParser source =
    ParserFast.run syntaxParser source


{-| [`Parser`](#Parser) for an [`GrenSyntax.File`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-File#File)
-}
module_ : Parser GrenSyntax.File
module_ =
    ParserFast.map4
        (\moduleHeaderResult moduleComments importsResult declarationsResult ->
            let
                importStartLocation : GrenSyntax.Location
                importStartLocation =
                    case importsResult.syntax of
                        (GrenSyntax.Node import0Range _) :: _ ->
                            import0Range.start

                        [] ->
                            case declarationsResult.syntax of
                                declarationAndLateImports0 :: _ ->
                                    declarationAndLateImports0.declaration
                                        |> GrenSyntax.nodeRange
                                        |> .start

                                [] ->
                                    -- invalid syntax
                                    { row = 2, column = 1 }

                moduleHeaderBasedOnExistingPorts :
                    GrenSyntax.DefaultModuleData
                    -> GrenSyntax.Module
                moduleHeaderBasedOnExistingPorts existingModuleHeaderInfo =
                    if
                        declarationsResult.syntax
                            |> List.any
                                (\declarationAndLateImports ->
                                    declarationAndLateImports.declaration
                                        |> GrenSyntax.nodeValue
                                        |> declarationIsPort
                                )
                    then
                        GrenSyntax.PortModule existingModuleHeaderInfo

                    else
                        GrenSyntax.NormalModule existingModuleHeaderInfo
            in
            { moduleDefinition =
                moduleHeaderResult.syntax
                    |> GrenSyntax.nodeMap
                        (\syntaxModuleHeader ->
                            case syntaxModuleHeader of
                                GrenSyntax.EffectModule effectModuleHeader ->
                                    GrenSyntax.EffectModule effectModuleHeader

                                GrenSyntax.NormalModule normalModuleHeader ->
                                    moduleHeaderBasedOnExistingPorts normalModuleHeader

                                GrenSyntax.PortModule normalModuleHeader ->
                                    moduleHeaderBasedOnExistingPorts normalModuleHeader
                        )
            , imports =
                (declarationsResult.syntax
                    |> List.concatMap .lateImports
                    |> List.map
                        (\(GrenSyntax.Node _ lateImport) ->
                            GrenSyntax.Node
                                { start = importStartLocation
                                , end = importStartLocation
                                }
                                lateImport
                        )
                )
                    ++ importsResult.syntax
            , declarations =
                declarationsResult.syntax
                    |> List.map .declaration
            , comments =
                moduleHeaderResult.comments
                    |> ropePrependTo moduleComments
                    |> ropePrependTo importsResult.comments
                    |> ropePrependTo declarationsResult.comments
                    |> commentsToList
            }
        )
        (whitespaceAndCommentsEndsTopIndentedFollowedByWithComments
            moduleHeader
        )
        (whitespaceAndCommentsEndsTopIndentedFollowedByComments
            (ParserFast.map2OrSucceed
                (\moduleDocumentation commentsAfter ->
                    ropeOne moduleDocumentation |> ropeFilledPrependTo commentsAfter
                )
                documentationComment
                whitespaceAndCommentsEndsTopIndented
                ropeEmpty
            )
        )
        (manyWithComments importFollowedByWhitespaceAndComments)
        (manyWithComments
            (topIndentedFollowedBy
                (ParserFast.map3
                    (\declarationParsed commentsAfter lateImportsResult ->
                        { comments =
                            declarationParsed.comments
                                |> ropePrependTo commentsAfter
                                |> ropePrependTo lateImportsResult.comments
                        , syntax =
                            { declaration = declarationParsed.syntax
                            , lateImports = lateImportsResult.syntax
                            }
                        }
                    )
                    declaration
                    whitespaceAndComments
                    (manyWithComments importFollowedByWhitespaceAndComments)
                )
            )
        )


declarationIsPort : GrenSyntax.Declaration -> Bool
declarationIsPort syntaxDeclaration =
    case syntaxDeclaration of
        GrenSyntax.PortDeclaration _ ->
            True

        GrenSyntax.FunctionDeclaration _ ->
            False

        GrenSyntax.AliasDeclaration _ ->
            False

        GrenSyntax.CustomTypeDeclaration _ ->
            False

        GrenSyntax.InfixDeclaration _ ->
            False


{-| [`Parser`](#Parser) for an [`GrenSyntax.ModuleName`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-ModuleName#ModuleName)
-}
moduleName : Parser (GrenSyntax.Node GrenSyntax.ModuleName)
moduleName =
    ParserFast.map2WithRange
        (\range head tail ->
            GrenSyntax.Node range (head :: tail)
        )
        nameUppercase
        (ParserFast.loopWhileSucceedsRightToLeftStackUnsafe
            (ParserFast.symbolFollowedBy "." nameUppercase)
            []
            (::)
        )


exposeDefinition : Parser (WithComments (GrenSyntax.Node GrenSyntax.Exposing))
exposeDefinition =
    ParserFast.map2WithRange
        (\range commentsAfterExposing exposingListInnerResult ->
            { comments =
                commentsAfterExposing
                    |> ropePrependTo exposingListInnerResult.comments
            , syntax =
                GrenSyntax.Node range exposingListInnerResult.syntax
            }
        )
        (ParserFast.symbolFollowedBy "exposing" whitespaceAndComments)
        exposing_


{-| [`Parser`](#Parser) for an [`GrenSyntax.Exposing`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Exposing#Exposing)
(the stuff after `exposing` in an import or module)
-}
exposing_ : Parser { comments : Comments, syntax : GrenSyntax.Exposing }
exposing_ =
    ParserFast.symbolFollowedBy "("
        (ParserFast.map2
            (\commentsBefore inner ->
                { comments = commentsBefore |> ropePrependTo inner.comments
                , syntax = inner.syntax
                }
            )
            whitespaceAndComments
            (ParserFast.oneOf3
                (ParserFast.mapWithRange
                    (\range comments ->
                        { comments = comments
                        , syntax = GrenSyntax.All range
                        }
                    )
                    (ParserFast.symbolFollowedBy "..." whitespaceAndComments)
                )
                (ParserFast.mapWithRange
                    (\range comments ->
                        { comments = comments
                        , syntax = GrenSyntax.All range
                        }
                    )
                    (ParserFast.symbolFollowedBy ".." whitespaceAndComments)
                )
                (exposingWithinParensExplicitFollowedByWhitespaceAndCommentsMap identity)
            )
        )
        |> ParserFast.followedBySymbol ")"


exposingWithinParensExplicitFollowedByWhitespaceAndCommentsMap : (GrenSyntax.Exposing -> syntax) -> ParserFast.Parser (WithComments syntax)
exposingWithinParensExplicitFollowedByWhitespaceAndCommentsMap exposingToSyntax =
    ParserFast.map4
        (\commentsBeforeHeadElement headElement commentsAfterHeadElement tailElements ->
            { comments =
                commentsBeforeHeadElement
                    |> ropePrependTo headElement.comments
                    |> ropePrependTo commentsAfterHeadElement
                    |> ropePrependTo tailElements.comments
            , syntax =
                GrenSyntax.Explicit
                    (headElement.syntax :: tailElements.syntax)
                    |> exposingToSyntax
            }
        )
        (ParserFast.orSucceed
            (ParserFast.symbolFollowedBy "," whitespaceAndComments)
            ropeEmpty
        )
        expose
        whitespaceAndComments
        (manyWithComments
            (ParserFast.symbolFollowedBy ","
                (ParserFast.map4
                    (\commentsBefore commentsWithExtraComma result commentsAfter ->
                        { comments =
                            commentsBefore
                                |> ropePrependTo commentsWithExtraComma
                                |> ropePrependTo result.comments
                                |> ropePrependTo commentsAfter
                        , syntax = result.syntax
                        }
                    )
                    whitespaceAndComments
                    (ParserFast.orSucceed
                        (ParserFast.symbolFollowedBy "," whitespaceAndComments)
                        ropeEmpty
                    )
                    expose
                    whitespaceAndComments
                )
            )
        )


{-| [`Parser`](#Parser) for a single [`GrenSyntax.TopLevelExpose`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Exposing#TopLevelExpose)
-}
expose : Parser { comments : Comments, syntax : GrenSyntax.Node GrenSyntax.TopLevelExpose }
expose =
    ParserFast.oneOf3
        functionExpose
        typeExpose
        infixExpose


infixExpose : Parser (WithComments (GrenSyntax.Node GrenSyntax.TopLevelExpose))
infixExpose =
    ParserFast.map2WithRange
        (\range infixName () ->
            { comments = ropeEmpty
            , syntax = GrenSyntax.Node range (GrenSyntax.InfixExpose infixName)
            }
        )
        (ParserFast.symbolFollowedBy "("
            (ParserFast.ifFollowedByWhileWithoutLinebreak
                (\c ->
                    case c of
                        ')' ->
                            False

                        '\n' ->
                            False

                        ' ' ->
                            False

                        _ ->
                            True
                )
                (\c ->
                    case c of
                        ')' ->
                            False

                        '\n' ->
                            False

                        ' ' ->
                            False

                        _ ->
                            True
                )
            )
        )
        (ParserFast.symbol ")" ())


typeExpose : Parser (WithComments (GrenSyntax.Node GrenSyntax.TopLevelExpose))
typeExpose =
    ParserFast.map3
        (\(GrenSyntax.Node typeNameRange typeExposeName) commentsBeforeMaybeOpen maybeOpen ->
            case maybeOpen of
                Nothing ->
                    { comments = commentsBeforeMaybeOpen
                    , syntax =
                        GrenSyntax.Node typeNameRange (GrenSyntax.TypeOrAliasExpose typeExposeName)
                    }

                Just open ->
                    { comments = commentsBeforeMaybeOpen |> ropePrependTo open.comments
                    , syntax =
                        GrenSyntax.Node
                            { start = typeNameRange.start
                            , end = open.syntax.end
                            }
                            (GrenSyntax.TypeExpose { name = typeExposeName, open = Just open.syntax })
                    }
        )
        nameUppercaseNode
        whitespaceAndComments
        (ParserFast.map2WithRangeOrSucceed
            (\range left right ->
                Just { comments = left |> ropePrependTo right, syntax = range }
            )
            (ParserFast.symbolFollowedBy "(" whitespaceAndComments)
            (ParserFast.oneOf2
                (ParserFast.symbolFollowedBy "..." whitespaceAndComments)
                (ParserFast.symbolFollowedBy ".." whitespaceAndComments)
                |> ParserFast.followedBySymbol ")"
            )
            Nothing
        )


functionExpose : Parser (WithComments (GrenSyntax.Node GrenSyntax.TopLevelExpose))
functionExpose =
    nameLowercaseMapWithRange
        (\range name ->
            { comments = ropeEmpty
            , syntax =
                GrenSyntax.Node range (GrenSyntax.FunctionExpose name)
            }
        )


{-| [`Parser`](#Parser) for an [`GrenSyntax.Module`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Module#Module)
(confusingly, that's their name for only the `module X exposing (Y)` lines)
-}
moduleHeader : Parser { comments : Comments, syntax : GrenSyntax.Node GrenSyntax.Module }
moduleHeader =
    ParserFast.oneOf3
        normalModuleDefinition
        portModuleDefinition
        effectModuleDefinition


effectWhereClause : Parser (WithComments ( String, GrenSyntax.Node String ))
effectWhereClause =
    ParserFast.map4
        (\fnName commentsAfterFnName commentsAfterEqual fnTypeName ->
            { comments = commentsAfterFnName |> ropePrependTo commentsAfterEqual
            , syntax = ( fnName, fnTypeName )
            }
        )
        nameLowercaseUnderscoreSuffixingKeywords
        whitespaceAndComments
        (ParserFast.symbolFollowedBy "=" whitespaceAndComments)
        nameUppercaseNode


whereBlock : Parser (WithComments { command : Maybe (GrenSyntax.Node String), subscription : Maybe (GrenSyntax.Node String) })
whereBlock =
    ParserFast.symbolFollowedBy "{"
        (ParserFast.map4
            (\commentsBeforeHead head commentsAfterHead tail ->
                let
                    pairs : List ( String, GrenSyntax.Node String )
                    pairs =
                        head.syntax :: tail.syntax
                in
                { comments =
                    commentsBeforeHead
                        |> ropePrependTo head.comments
                        |> ropePrependTo commentsAfterHead
                        |> ropePrependTo tail.comments
                , syntax =
                    { command =
                        pairs
                            |> listFirstWhere
                                (\( fnName, _ ) ->
                                    case fnName of
                                        "command" ->
                                            True

                                        _ ->
                                            False
                                )
                            |> Maybe.map Tuple.second
                    , subscription =
                        pairs
                            |> listFirstWhere
                                (\( fnName, _ ) ->
                                    case fnName of
                                        "subscription" ->
                                            True

                                        _ ->
                                            False
                                )
                            |> Maybe.map Tuple.second
                    }
                }
            )
            whitespaceAndComments
            effectWhereClause
            whitespaceAndComments
            (manyWithComments
                (ParserFast.symbolFollowedBy ","
                    (ParserFast.map3
                        (\commentsBefore v commentsAfter ->
                            { comments =
                                commentsBefore
                                    |> ropePrependTo v.comments
                                    |> ropePrependTo commentsAfter
                            , syntax = v.syntax
                            }
                        )
                        whitespaceAndComments
                        effectWhereClause
                        whitespaceAndComments
                    )
                )
            )
        )
        |> ParserFast.followedBySymbol "}"


listFirstWhere : (a -> Bool) -> List a -> Maybe a
listFirstWhere predicate list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if predicate x then
                Just x

            else
                listFirstWhere predicate xs


effectWhereClauses : Parser (WithComments { command : Maybe (GrenSyntax.Node String), subscription : Maybe (GrenSyntax.Node String) })
effectWhereClauses =
    ParserFast.map2
        (\commentsBefore whereResult ->
            { comments = commentsBefore |> ropePrependTo whereResult.comments
            , syntax = whereResult.syntax
            }
        )
        (ParserFast.keywordFollowedBy "where" whitespaceAndComments)
        whereBlock


effectModuleDefinition : Parser (WithComments (GrenSyntax.Node GrenSyntax.Module))
effectModuleDefinition =
    ParserFast.map7WithRange
        (\range commentsAfterEffect commentsAfterModule name commentsAfterName whereClauses commentsAfterWhereClauses exp ->
            { comments =
                commentsAfterEffect
                    |> ropePrependTo commentsAfterModule
                    |> ropePrependTo commentsAfterName
                    |> ropePrependTo whereClauses.comments
                    |> ropePrependTo commentsAfterWhereClauses
                    |> ropePrependTo exp.comments
            , syntax =
                GrenSyntax.Node range
                    (GrenSyntax.EffectModule
                        { moduleName = name
                        , exposingList = exp.syntax
                        , command = whereClauses.syntax.command
                        , subscription = whereClauses.syntax.subscription
                        }
                    )
            }
        )
        (ParserFast.keywordFollowedBy "effect" whitespaceAndComments)
        (ParserFast.keywordFollowedBy "module" whitespaceAndComments)
        moduleName
        whitespaceAndComments
        effectWhereClauses
        whitespaceAndComments
        exposeDefinition


normalModuleDefinition : Parser (WithComments (GrenSyntax.Node GrenSyntax.Module))
normalModuleDefinition =
    ParserFast.map4WithRange
        (\range commentsAfterModule moduleNameNode commentsAfterModuleName exposingList ->
            { comments =
                commentsAfterModule
                    |> ropePrependTo commentsAfterModuleName
                    |> ropePrependTo exposingList.comments
            , syntax =
                GrenSyntax.Node range
                    (GrenSyntax.NormalModule
                        { moduleName = moduleNameNode
                        , exposingList = exposingList.syntax
                        }
                    )
            }
        )
        (ParserFast.keywordFollowedBy "module" whitespaceAndComments)
        moduleName
        whitespaceAndComments
        exposeDefinition


portModuleDefinition : Parser (WithComments (GrenSyntax.Node GrenSyntax.Module))
portModuleDefinition =
    ParserFast.map5WithRange
        (\range commentsAfterPort commentsAfterModule moduleNameNode commentsAfterModuleName exposingList ->
            { comments =
                commentsAfterPort
                    |> ropePrependTo commentsAfterModule
                    |> ropePrependTo commentsAfterModuleName
                    |> ropePrependTo exposingList.comments
            , syntax =
                GrenSyntax.Node range
                    (GrenSyntax.PortModule { moduleName = moduleNameNode, exposingList = exposingList.syntax })
            }
        )
        (ParserFast.keywordFollowedBy "port" whitespaceAndComments)
        (ParserFast.keywordFollowedBy "module" whitespaceAndComments)
        moduleName
        whitespaceAndComments
        exposeDefinition


{-| [`Parser`](#Parser) for a single [`GrenSyntax.Import`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Import#Import)
-}
import_ : Parser { comments : Comments, syntax : GrenSyntax.Node GrenSyntax.Import }
import_ =
    importFollowedByWhitespaceAndComments


importFollowedByWhitespaceAndComments : Parser { comments : Comments, syntax : GrenSyntax.Node GrenSyntax.Import }
importFollowedByWhitespaceAndComments =
    ParserFast.map5WithStartLocation
        (\start commentsAfterImport moduleNameNode commentsAfterModuleName maybeModuleAlias maybeExposingResult ->
            let
                commentsBeforeAlias : Comments
                commentsBeforeAlias =
                    commentsAfterImport
                        |> ropePrependTo commentsAfterModuleName
            in
            case maybeModuleAlias of
                Nothing ->
                    case maybeExposingResult.syntax of
                        Nothing ->
                            let
                                (GrenSyntax.Node moduleNameRange _) =
                                    moduleNameNode
                            in
                            { comments =
                                commentsBeforeAlias
                                    |> ropePrependTo maybeExposingResult.comments
                            , syntax =
                                GrenSyntax.Node { start = start, end = moduleNameRange.end }
                                    { moduleName = moduleNameNode
                                    , moduleAlias = Nothing
                                    , exposingList = Nothing
                                    }
                            }

                        Just exposingListValue ->
                            let
                                (GrenSyntax.Node exposingRange _) =
                                    exposingListValue
                            in
                            { comments =
                                commentsBeforeAlias
                                    |> ropePrependTo maybeExposingResult.comments
                            , syntax =
                                GrenSyntax.Node { start = start, end = exposingRange.end }
                                    { moduleName = moduleNameNode
                                    , moduleAlias = Nothing
                                    , exposingList = Just exposingListValue
                                    }
                            }

                Just moduleAliasResult ->
                    case maybeExposingResult.syntax of
                        Nothing ->
                            let
                                (GrenSyntax.Node aliasRange _) =
                                    moduleAliasResult.syntax
                            in
                            { comments =
                                commentsBeforeAlias
                                    |> ropePrependTo moduleAliasResult.comments
                                    |> ropePrependTo maybeExposingResult.comments
                            , syntax =
                                GrenSyntax.Node { start = start, end = aliasRange.end }
                                    { moduleName = moduleNameNode
                                    , moduleAlias = Just moduleAliasResult.syntax
                                    , exposingList = Nothing
                                    }
                            }

                        Just exposingListValue ->
                            let
                                (GrenSyntax.Node exposingRange _) =
                                    exposingListValue
                            in
                            { comments =
                                commentsBeforeAlias
                                    |> ropePrependTo moduleAliasResult.comments
                                    |> ropePrependTo maybeExposingResult.comments
                            , syntax =
                                GrenSyntax.Node { start = start, end = exposingRange.end }
                                    { moduleName = moduleNameNode
                                    , moduleAlias = Just moduleAliasResult.syntax
                                    , exposingList = Just exposingListValue
                                    }
                            }
        )
        (ParserFast.keywordFollowedBy "import" whitespaceAndComments)
        moduleName
        whitespaceAndComments
        (ParserFast.map3OrSucceed
            (\commentsBefore moduleAliasNode commentsAfter ->
                Just
                    { comments = commentsBefore |> ropePrependTo commentsAfter
                    , syntax = moduleAliasNode
                    }
            )
            (ParserFast.keywordFollowedBy "as" whitespaceAndComments)
            (nameUppercaseMapWithRange
                (\range moduleAlias ->
                    GrenSyntax.Node range [ moduleAlias ]
                )
            )
            whitespaceAndComments
            Nothing
        )
        (ParserFast.map2OrSucceed
            (\exposingResult commentsAfter ->
                { comments = exposingResult.comments |> ropePrependTo commentsAfter
                , syntax = exposingResult.syntax
                }
            )
            (ParserFast.map2WithRange
                (\range commentsAfterExposing exposingListInnerResult ->
                    { comments =
                        commentsAfterExposing
                            |> ropePrependTo exposingListInnerResult.comments
                    , syntax =
                        case exposingListInnerResult.syntax of
                            Nothing ->
                                Nothing

                            Just exposingListInner ->
                                Just (GrenSyntax.Node range exposingListInner)
                    }
                )
                (ParserFast.symbolFollowedBy "exposing" whitespaceAndComments)
                (ParserFast.symbolFollowedBy "("
                    (ParserFast.map2
                        (\commentsBefore inner ->
                            { comments = commentsBefore |> ropePrependTo inner.comments
                            , syntax = inner.syntax
                            }
                        )
                        whitespaceAndComments
                        (ParserFast.oneOf4
                            (ParserFast.mapWithRange
                                (\range comments ->
                                    { comments = comments
                                    , syntax = Just (GrenSyntax.All range)
                                    }
                                )
                                (ParserFast.symbolFollowedBy "..." whitespaceAndComments)
                                |> ParserFast.followedBySymbol ")"
                            )
                            (ParserFast.mapWithRange
                                (\range comments ->
                                    { comments = comments
                                    , syntax = Just (GrenSyntax.All range)
                                    }
                                )
                                (ParserFast.symbolFollowedBy ".." whitespaceAndComments)
                                |> ParserFast.followedBySymbol ")"
                            )
                            (ParserFast.symbol ")" { comments = ropeEmpty, syntax = Nothing })
                            (exposingWithinParensExplicitFollowedByWhitespaceAndCommentsMap Just
                                |> ParserFast.followedBySymbol ")"
                            )
                        )
                    )
                )
            )
            whitespaceAndComments
            { comments = ropeEmpty, syntax = Nothing }
        )


{-| [`Parser`](#Parser) for a list of [`GrenSyntax.Declaration`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Declaration#Declaration)s
and comments in between
-}
declarations : Parser { comments : Comments, syntax : List (GrenSyntax.Node GrenSyntax.Declaration) }
declarations =
    manyWithComments
        (topIndentedFollowedBy
            (ParserFast.map2
                (\declarationParsed commentsAfter ->
                    { comments = declarationParsed.comments |> ropePrependTo commentsAfter
                    , syntax = declarationParsed.syntax
                    }
                )
                declaration
                whitespaceAndComments
            )
        )


{-| [`Parser`](#Parser) for an [`GrenSyntax.Declaration`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Declaration#Declaration)
-}
declaration : Parser { comments : Comments, syntax : GrenSyntax.Node GrenSyntax.Declaration }
declaration =
    ParserFast.oneOf5
        functionDeclarationWithoutDocumentation
        declarationWithDocumentation
        typeOrTypeAliasDefinitionWithoutDocumentation
        portDeclarationWithoutDocumentation
        infixDeclaration


documentationComment : Parser (GrenSyntax.Node String)
documentationComment =
    -- technically making the whole parser fail on multi-line comments would be "correct"
    -- but in practice, all declaration comments allow layout before which already handles
    -- these.
    ParserFast.nestableMultiCommentMapWithRange GrenSyntax.Node
        ( '{', "-" )
        ( '-', "}" )


declarationWithDocumentation : Parser (WithComments (GrenSyntax.Node GrenSyntax.Declaration))
declarationWithDocumentation =
    ParserFast.map2
        (\documentation afterDocumentation ->
            let
                start : GrenSyntax.Location
                start =
                    (GrenSyntax.nodeRange documentation).start
            in
            case afterDocumentation.syntax of
                FunctionDeclarationAfterDocumentation functionDeclarationAfterDocumentation ->
                    case functionDeclarationAfterDocumentation.signature of
                        Just signature ->
                            let
                                (GrenSyntax.Node implementationNameRange _) =
                                    signature.implementationName

                                (GrenSyntax.Node expressionRange _) =
                                    functionDeclarationAfterDocumentation.expression
                            in
                            { comments = afterDocumentation.comments
                            , syntax =
                                GrenSyntax.Node { start = start, end = expressionRange.end }
                                    (GrenSyntax.FunctionDeclaration
                                        { documentation = Just documentation
                                        , signature =
                                            Just
                                                (GrenSyntax.nodeCombine
                                                    (\name value -> { name = name, typeAnnotation = value })
                                                    functionDeclarationAfterDocumentation.startName
                                                    signature.typeAnnotation
                                                )
                                        , declaration =
                                            GrenSyntax.Node
                                                { start = implementationNameRange.start
                                                , end = expressionRange.end
                                                }
                                                { name = signature.implementationName
                                                , arguments = functionDeclarationAfterDocumentation.arguments
                                                , expression = functionDeclarationAfterDocumentation.expression
                                                }
                                        }
                                    )
                            }

                        Nothing ->
                            let
                                (GrenSyntax.Node startNameRange _) =
                                    functionDeclarationAfterDocumentation.startName

                                (GrenSyntax.Node expressionRange _) =
                                    functionDeclarationAfterDocumentation.expression
                            in
                            { comments = afterDocumentation.comments
                            , syntax =
                                GrenSyntax.Node { start = start, end = expressionRange.end }
                                    (GrenSyntax.FunctionDeclaration
                                        { documentation = Just documentation
                                        , signature = Nothing
                                        , declaration =
                                            GrenSyntax.Node { start = startNameRange.start, end = expressionRange.end }
                                                { name = functionDeclarationAfterDocumentation.startName
                                                , arguments = functionDeclarationAfterDocumentation.arguments
                                                , expression = functionDeclarationAfterDocumentation.expression
                                                }
                                        }
                                    )
                            }

                TypeDeclarationAfterDocumentation typeDeclarationAfterDocumentation ->
                    let
                        end : GrenSyntax.Location
                        end =
                            case typeDeclarationAfterDocumentation.tailVariantsReverse of
                                (GrenSyntax.Node range _) :: _ ->
                                    range.end

                                [] ->
                                    let
                                        (GrenSyntax.Node headVariantRange _) =
                                            typeDeclarationAfterDocumentation.headVariant
                                    in
                                    headVariantRange.end
                    in
                    { comments = afterDocumentation.comments
                    , syntax =
                        GrenSyntax.Node { start = start, end = end }
                            (GrenSyntax.CustomTypeDeclaration
                                { documentation = Just documentation
                                , name = typeDeclarationAfterDocumentation.name
                                , generics = typeDeclarationAfterDocumentation.parameters
                                , constructors =
                                    typeDeclarationAfterDocumentation.headVariant
                                        :: List.reverse typeDeclarationAfterDocumentation.tailVariantsReverse
                                }
                            )
                    }

                TypeAliasDeclarationAfterDocumentation typeAliasDeclarationAfterDocumentation ->
                    let
                        (GrenSyntax.Node typeAnnotationRange _) =
                            typeAliasDeclarationAfterDocumentation.typeAnnotation
                    in
                    { comments = afterDocumentation.comments
                    , syntax =
                        GrenSyntax.Node { start = start, end = typeAnnotationRange.end }
                            (GrenSyntax.AliasDeclaration
                                { documentation = Just documentation
                                , name = typeAliasDeclarationAfterDocumentation.name
                                , generics = typeAliasDeclarationAfterDocumentation.parameters
                                , typeAnnotation = typeAliasDeclarationAfterDocumentation.typeAnnotation
                                }
                            )
                    }

                PortDeclarationAfterDocumentation portDeclarationAfterName ->
                    let
                        (GrenSyntax.Node typeAnnotationRange _) =
                            portDeclarationAfterName.typeAnnotation
                    in
                    { comments =
                        ropeOne documentation
                            |> ropeFilledPrependTo afterDocumentation.comments
                    , syntax =
                        GrenSyntax.Node
                            { start = portDeclarationAfterName.startLocation
                            , end = typeAnnotationRange.end
                            }
                            (GrenSyntax.PortDeclaration
                                { name = portDeclarationAfterName.name
                                , typeAnnotation = portDeclarationAfterName.typeAnnotation
                                }
                            )
                    }
        )
        documentationComment
        (whitespaceAndCommentsEndsTopIndentedFollowedByWithComments
            (ParserFast.oneOf3
                functionAfterDocumentation
                typeOrTypeAliasDefinitionAfterDocumentation
                portDeclarationAfterDocumentation
            )
        )
        |> ParserFast.validate
            (\result ->
                let
                    (GrenSyntax.Node _ decl) =
                        result.syntax
                in
                case decl of
                    GrenSyntax.FunctionDeclaration letFunctionDeclaration ->
                        case letFunctionDeclaration.signature of
                            Nothing ->
                                True

                            Just (GrenSyntax.Node _ signature) ->
                                let
                                    (GrenSyntax.Node _ implementationName) =
                                        implementation.name

                                    (GrenSyntax.Node _ implementation) =
                                        letFunctionDeclaration.declaration

                                    (GrenSyntax.Node _ signatureName) =
                                        signature.name
                                in
                                implementationName == signatureName

                    _ ->
                        True
            )


type DeclarationAfterDocumentation
    = FunctionDeclarationAfterDocumentation
        { startName : GrenSyntax.Node String
        , signature :
            Maybe
                { typeAnnotation : GrenSyntax.Node GrenSyntax.TypeAnnotation
                , implementationName : GrenSyntax.Node String
                }
        , arguments : List (GrenSyntax.Node GrenSyntax.Pattern)
        , expression : GrenSyntax.Node GrenSyntax.Expression
        }
    | TypeDeclarationAfterDocumentation
        { name : GrenSyntax.Node String
        , parameters : List (GrenSyntax.Node String)
        , headVariant : GrenSyntax.Node GrenSyntax.ValueConstructor
        , tailVariantsReverse : List (GrenSyntax.Node GrenSyntax.ValueConstructor)
        }
    | TypeAliasDeclarationAfterDocumentation
        { name : GrenSyntax.Node String
        , parameters : List (GrenSyntax.Node String)
        , typeAnnotation : GrenSyntax.Node GrenSyntax.TypeAnnotation
        }
    | PortDeclarationAfterDocumentation
        { startLocation : GrenSyntax.Location
        , name : GrenSyntax.Node String
        , typeAnnotation : GrenSyntax.Node GrenSyntax.TypeAnnotation
        }


type TypeOrTypeAliasDeclarationWithoutDocumentation
    = TypeDeclarationWithoutDocumentation
        { name : GrenSyntax.Node String
        , parameters : List (GrenSyntax.Node String)
        , headVariant : GrenSyntax.Node GrenSyntax.ValueConstructor
        , tailVariantsReverse : List (GrenSyntax.Node GrenSyntax.ValueConstructor)
        }
    | TypeAliasDeclarationWithoutDocumentation
        { name : GrenSyntax.Node String
        , parameters : List (GrenSyntax.Node String)
        , typeAnnotation : GrenSyntax.Node GrenSyntax.TypeAnnotation
        }


functionAfterDocumentation : Parser (WithComments DeclarationAfterDocumentation)
functionAfterDocumentation =
    ParserFast.oneOf2
        (ParserFast.map6
            (\startName commentsAfterStartName maybeSignature arguments commentsAfterEqual result ->
                { comments =
                    (commentsAfterStartName |> ropePrependTo maybeSignature.comments)
                        |> ropePrependTo arguments.comments
                        |> ropePrependTo commentsAfterEqual
                        |> ropePrependTo result.comments
                , syntax =
                    FunctionDeclarationAfterDocumentation
                        { startName = startName
                        , signature = maybeSignature.syntax
                        , arguments = arguments.syntax
                        , expression = result.syntax
                        }
                }
            )
            -- infix declarations itself don't have documentation
            nameLowercaseNode
            whitespaceAndComments
            (ParserFast.map4OrSucceed
                (\commentsBeforeTypeAnnotation typeAnnotationResult implementationName afterImplementationName ->
                    { comments =
                        commentsBeforeTypeAnnotation
                            |> ropePrependTo typeAnnotationResult.comments
                            |> ropePrependTo implementationName.comments
                            |> ropePrependTo afterImplementationName
                    , syntax =
                        Just
                            { implementationName = implementationName.syntax
                            , typeAnnotation = typeAnnotationResult.syntax
                            }
                    }
                )
                (ParserFast.symbolFollowedBy ":" whitespaceAndComments)
                type_
                (whitespaceAndCommentsEndsTopIndentedFollowedBy
                    nameLowercaseNode
                )
                whitespaceAndComments
                { comments = ropeEmpty, syntax = Nothing }
            )
            parameterPatternsEquals
            whitespaceAndComments
            expressionFollowedByWhitespaceAndComments
        )
        (ParserFast.map8WithStartLocation
            (\start commentsBeforeTypeAnnotation typeAnnotationResult commentsBetweenTypeAndName nameNode afterImplementationName arguments commentsAfterEqual result ->
                { comments =
                    commentsBeforeTypeAnnotation
                        |> ropePrependTo typeAnnotationResult.comments
                        |> ropePrependTo commentsBetweenTypeAndName
                        |> ropePrependTo afterImplementationName
                        |> ropePrependTo arguments.comments
                        |> ropePrependTo commentsAfterEqual
                        |> ropePrependTo result.comments
                , syntax =
                    FunctionDeclarationAfterDocumentation
                        { startName =
                            -- dummy
                            GrenSyntax.Node
                                { start = start, end = start }
                                (nameNode |> GrenSyntax.nodeValue)
                        , signature =
                            Just
                                { implementationName = nameNode
                                , typeAnnotation = typeAnnotationResult.syntax
                                }
                        , arguments = arguments.syntax
                        , expression = result.syntax
                        }
                }
            )
            (ParserFast.symbolFollowedBy ":" whitespaceAndComments)
            type_
            whitespaceAndCommentsEndsTopIndented
            nameLowercaseNode
            whitespaceAndComments
            parameterPatternsEquals
            whitespaceAndComments
            expressionFollowedByWhitespaceAndComments
        )


functionDeclarationWithoutDocumentation : Parser (WithComments (GrenSyntax.Node GrenSyntax.Declaration))
functionDeclarationWithoutDocumentation =
    ParserFast.oneOf2
        (ParserFast.map6WithStartLocation
            (\startNameStart startNameNode commentsAfterStartName maybeSignature arguments commentsAfterEqual result ->
                let
                    (GrenSyntax.Node expressionRange _) =
                        result.syntax
                in
                case maybeSignature of
                    Nothing ->
                        { comments =
                            commentsAfterStartName
                                |> ropePrependTo arguments.comments
                                |> ropePrependTo commentsAfterEqual
                                |> ropePrependTo result.comments
                        , syntax =
                            GrenSyntax.Node { start = startNameStart, end = expressionRange.end }
                                (GrenSyntax.FunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        GrenSyntax.Node { start = startNameStart, end = expressionRange.end }
                                            { name = startNameNode
                                            , arguments = arguments.syntax
                                            , expression = result.syntax
                                            }
                                    }
                                )
                        }

                    Just signature ->
                        let
                            (GrenSyntax.Node implementationNameRange _) =
                                signature.implementationName
                        in
                        { comments =
                            (commentsAfterStartName |> ropePrependTo signature.comments)
                                |> ropePrependTo arguments.comments
                                |> ropePrependTo commentsAfterEqual
                                |> ropePrependTo result.comments
                        , syntax =
                            GrenSyntax.Node { start = startNameStart, end = expressionRange.end }
                                (GrenSyntax.FunctionDeclaration
                                    { documentation = Nothing
                                    , signature =
                                        Just
                                            (GrenSyntax.nodeCombine
                                                (\name typeAnnotation -> { name = name, typeAnnotation = typeAnnotation })
                                                startNameNode
                                                signature.typeAnnotation
                                            )
                                    , declaration =
                                        GrenSyntax.Node
                                            { start = implementationNameRange.start
                                            , end = expressionRange.end
                                            }
                                            { name = signature.implementationName
                                            , arguments = arguments.syntax
                                            , expression = result.syntax
                                            }
                                    }
                                )
                        }
            )
            functionNameNotInfixNode
            whitespaceAndComments
            (ParserFast.map4OrSucceed
                (\commentsBeforeTypeAnnotation typeAnnotationResult implementationName afterImplementationName ->
                    Just
                        { comments =
                            commentsBeforeTypeAnnotation
                                |> ropePrependTo typeAnnotationResult.comments
                                |> ropePrependTo implementationName.comments
                                |> ropePrependTo afterImplementationName
                        , implementationName = implementationName.syntax
                        , typeAnnotation = typeAnnotationResult.syntax
                        }
                )
                (ParserFast.symbolFollowedBy ":" whitespaceAndComments)
                type_
                (whitespaceAndCommentsEndsTopIndentedFollowedBy
                    nameLowercaseNode
                )
                whitespaceAndComments
                Nothing
            )
            parameterPatternsEquals
            whitespaceAndComments
            expressionFollowedByWhitespaceAndComments
            |> ParserFast.validate
                (\result ->
                    let
                        (GrenSyntax.Node _ decl) =
                            result.syntax
                    in
                    case decl of
                        GrenSyntax.FunctionDeclaration letFunctionDeclaration ->
                            case letFunctionDeclaration.signature of
                                Nothing ->
                                    True

                                Just (GrenSyntax.Node _ signature) ->
                                    let
                                        (GrenSyntax.Node _ implementationName) =
                                            implementation.name

                                        (GrenSyntax.Node _ implementation) =
                                            letFunctionDeclaration.declaration

                                        (GrenSyntax.Node _ signatureName) =
                                            signature.name
                                    in
                                    implementationName == signatureName

                        GrenSyntax.AliasDeclaration _ ->
                            True

                        GrenSyntax.CustomTypeDeclaration _ ->
                            True

                        GrenSyntax.PortDeclaration _ ->
                            True

                        GrenSyntax.InfixDeclaration _ ->
                            True
                )
        )
        (ParserFast.map8WithStartLocation
            (\start commentsBeforeTypeAnnotation typeAnnotationResult commentsBetweenTypeAndName nameNode afterImplementationName arguments commentsAfterEqual result ->
                { comments =
                    commentsBeforeTypeAnnotation
                        |> ropePrependTo typeAnnotationResult.comments
                        |> ropePrependTo commentsBetweenTypeAndName
                        |> ropePrependTo afterImplementationName
                        |> ropePrependTo arguments.comments
                        |> ropePrependTo commentsAfterEqual
                        |> ropePrependTo result.comments
                , syntax =
                    GrenSyntax.Node
                        { start = start
                        , end = result.syntax |> GrenSyntax.nodeRange |> .end
                        }
                        (GrenSyntax.FunctionDeclaration
                            { documentation = Nothing
                            , signature =
                                Just
                                    (GrenSyntax.Node
                                        { start = start
                                        , end = typeAnnotationResult.syntax |> GrenSyntax.nodeRange |> .end
                                        }
                                        { name =
                                            -- dummy
                                            GrenSyntax.Node
                                                { start = start, end = start }
                                                (nameNode |> GrenSyntax.nodeValue)
                                        , typeAnnotation = typeAnnotationResult.syntax
                                        }
                                    )
                            , declaration =
                                GrenSyntax.Node
                                    { start = nameNode |> GrenSyntax.nodeRange |> .start
                                    , end = result.syntax |> GrenSyntax.nodeRange |> .end
                                    }
                                    { name = nameNode
                                    , arguments = arguments.syntax
                                    , expression = result.syntax
                                    }
                            }
                        )
                }
            )
            (ParserFast.symbolFollowedBy ":" whitespaceAndComments)
            type_
            whitespaceAndCommentsEndsTopIndented
            nameLowercaseNode
            whitespaceAndComments
            parameterPatternsEquals
            whitespaceAndComments
            expressionFollowedByWhitespaceAndComments
        )


parameterPatternsEquals : Parser (WithComments (List (GrenSyntax.Node GrenSyntax.Pattern)))
parameterPatternsEquals =
    untilWithComments
        (ParserFast.oneOf2
            (ParserFast.symbol "=" ())
            (ParserFast.symbol "->" ())
        )
        (ParserFast.map2
            (\patternResult commentsAfterPattern ->
                { comments = patternResult.comments |> ropePrependTo commentsAfterPattern
                , syntax = patternResult.syntax
                }
            )
            patternNotSpaceSeparated
            whitespaceAndComments
        )


infixDeclaration : Parser (WithComments (GrenSyntax.Node GrenSyntax.Declaration))
infixDeclaration =
    ParserFast.map9WithRange
        (\range commentsAfterInfix direction commentsAfterDirection precedence commentsAfterPrecedence operator commentsAfterOperator commentsAfterEqual fn ->
            { comments =
                commentsAfterInfix
                    |> ropePrependTo commentsAfterDirection
                    |> ropePrependTo commentsAfterPrecedence
                    |> ropePrependTo commentsAfterOperator
                    |> ropePrependTo commentsAfterEqual
            , syntax =
                GrenSyntax.Node range
                    (GrenSyntax.InfixDeclaration
                        { direction = direction, precedence = precedence, operator = operator, function = fn }
                    )
            }
        )
        (ParserFast.keywordFollowedBy "infix" whitespaceAndComments)
        infixDirection
        whitespaceAndComments
        (ParserFast.integerDecimalMapWithRange GrenSyntax.Node)
        whitespaceAndComments
        (ParserFast.symbolFollowedBy "("
            (ParserFast.whileAtMost3WithoutLinebreakAnd2PartUtf16ValidateMapWithRangeBacktrackableFollowedBySymbol
                (\operatorRange operator ->
                    GrenSyntax.Node
                        { start = { row = operatorRange.start.row, column = operatorRange.start.column - 1 }
                        , end = { row = operatorRange.end.row, column = operatorRange.end.column + 1 }
                        }
                        operator
                )
                isOperatorSymbolCharAsString
                isAllowedOperatorToken
                ")"
            )
        )
        whitespaceAndComments
        (ParserFast.symbolFollowedBy "=" whitespaceAndComments)
        nameLowercaseNode


infixDirection : Parser (GrenSyntax.Node GrenSyntax.InfixDirection)
infixDirection =
    ParserFast.oneOf3
        (ParserFast.mapWithRange GrenSyntax.Node (ParserFast.keyword "right" GrenSyntax.Right))
        (ParserFast.mapWithRange GrenSyntax.Node (ParserFast.keyword "left" GrenSyntax.Left))
        (ParserFast.mapWithRange GrenSyntax.Node (ParserFast.keyword "non" GrenSyntax.Non))


portDeclarationAfterDocumentation : Parser (WithComments DeclarationAfterDocumentation)
portDeclarationAfterDocumentation =
    ParserFast.map5
        (\commentsAfterPort nameNode commentsAfterName commentsAfterColon typeAnnotationResult ->
            let
                (GrenSyntax.Node nameRange _) =
                    nameNode
            in
            { comments =
                commentsAfterPort
                    |> ropePrependTo commentsAfterName
                    |> ropePrependTo typeAnnotationResult.comments
                    |> ropePrependTo commentsAfterColon
            , syntax =
                PortDeclarationAfterDocumentation
                    { startLocation = { row = nameRange.start.row, column = 1 }
                    , name = nameNode
                    , typeAnnotation = typeAnnotationResult.syntax
                    }
            }
        )
        (ParserFast.keywordFollowedBy "port" whitespaceAndComments)
        nameLowercaseNodeUnderscoreSuffixingKeywords
        whitespaceAndComments
        (ParserFast.symbolFollowedBy ":" whitespaceAndComments)
        type_


portDeclarationWithoutDocumentation : Parser (WithComments (GrenSyntax.Node GrenSyntax.Declaration))
portDeclarationWithoutDocumentation =
    ParserFast.map5
        (\commentsAfterPort nameNode commentsAfterName commentsAfterColon typeAnnotationResult ->
            let
                (GrenSyntax.Node nameRange _) =
                    nameNode

                (GrenSyntax.Node typeRange _) =
                    typeAnnotationResult.syntax
            in
            { comments =
                commentsAfterPort
                    |> ropePrependTo commentsAfterName
                    |> ropePrependTo commentsAfterColon
                    |> ropePrependTo typeAnnotationResult.comments
            , syntax =
                GrenSyntax.Node
                    { start = { row = nameRange.start.row, column = 1 }
                    , end = typeRange.end
                    }
                    (GrenSyntax.PortDeclaration
                        { name = nameNode
                        , typeAnnotation = typeAnnotationResult.syntax
                        }
                    )
            }
        )
        (ParserFast.keywordFollowedBy "port" whitespaceAndComments)
        nameLowercaseNodeUnderscoreSuffixingKeywords
        whitespaceAndComments
        (ParserFast.symbolFollowedBy ":" whitespaceAndComments)
        type_


typeOrTypeAliasDefinitionAfterDocumentation : Parser (WithComments DeclarationAfterDocumentation)
typeOrTypeAliasDefinitionAfterDocumentation =
    ParserFast.map2
        (\commentsAfterType declarationAfterDocumentation ->
            { comments = commentsAfterType |> ropePrependTo declarationAfterDocumentation.comments
            , syntax = declarationAfterDocumentation.syntax
            }
        )
        (ParserFast.keywordFollowedBy "type" whitespaceAndComments)
        (ParserFast.oneOf2
            typeAliasDefinitionAfterDocumentationAfterTypePrefix
            choiceTypeDefinitionAfterDocumentationAfterTypePrefix
        )


typeAliasDefinitionAfterDocumentationAfterTypePrefix : Parser (WithComments DeclarationAfterDocumentation)
typeAliasDefinitionAfterDocumentationAfterTypePrefix =
    ParserFast.map6
        (\commentsAfterAlias name commentsAfterName parameters commentsAfterEquals typeAnnotationResult ->
            { comments =
                commentsAfterAlias
                    |> ropePrependTo commentsAfterName
                    |> ropePrependTo parameters.comments
                    |> ropePrependTo commentsAfterEquals
                    |> ropePrependTo typeAnnotationResult.comments
            , syntax =
                TypeAliasDeclarationAfterDocumentation
                    { name = name
                    , parameters = parameters.syntax
                    , typeAnnotation = typeAnnotationResult.syntax
                    }
            }
        )
        (ParserFast.keywordFollowedBy "alias" whitespaceAndComments)
        nameUppercaseNode
        whitespaceAndComments
        typeGenericListEquals
        whitespaceAndComments
        type_


choiceTypeDefinitionAfterDocumentationAfterTypePrefix : Parser (WithComments DeclarationAfterDocumentation)
choiceTypeDefinitionAfterDocumentationAfterTypePrefix =
    ParserFast.map7
        (\name commentsAfterName parameters commentsAfterEqual commentsBeforeHeadVariant headVariant tailVariantsReverse ->
            { comments =
                commentsAfterName
                    |> ropePrependTo parameters.comments
                    |> ropePrependTo commentsAfterEqual
                    |> ropePrependTo commentsBeforeHeadVariant
                    |> ropePrependTo headVariant.comments
                    |> ropePrependTo tailVariantsReverse.comments
            , syntax =
                TypeDeclarationAfterDocumentation
                    { name = name
                    , parameters = parameters.syntax
                    , headVariant = headVariant.syntax
                    , tailVariantsReverse = tailVariantsReverse.syntax
                    }
            }
        )
        nameUppercaseNode
        whitespaceAndComments
        typeGenericListEquals
        whitespaceAndComments
        (ParserFast.orSucceed
            (ParserFast.symbolFollowedBy "|" whitespaceAndComments)
            ropeEmpty
        )
        variantDeclarationFollowedByWhitespaceAndComments
        (manyWithCommentsReverse
            (ParserFast.symbolFollowedBy "|"
                (ParserFast.map3
                    (\commentsBeforePipe commentsWithExtraPipe variantResult ->
                        { comments =
                            commentsBeforePipe
                                |> ropePrependTo commentsWithExtraPipe
                                |> ropePrependTo variantResult.comments
                        , syntax = variantResult.syntax
                        }
                    )
                    whitespaceAndComments
                    (ParserFast.orSucceed
                        (ParserFast.symbolFollowedBy "|" whitespaceAndComments)
                        ropeEmpty
                    )
                    variantDeclarationFollowedByWhitespaceAndComments
                )
            )
        )


typeOrTypeAliasDefinitionWithoutDocumentation : Parser (WithComments (GrenSyntax.Node GrenSyntax.Declaration))
typeOrTypeAliasDefinitionWithoutDocumentation =
    ParserFast.map2WithStartLocation
        (\start commentsAfterType afterStart ->
            let
                allComments : Comments
                allComments =
                    commentsAfterType |> ropePrependTo afterStart.comments
            in
            case afterStart.syntax of
                TypeDeclarationWithoutDocumentation typeDeclarationAfterDocumentation ->
                    let
                        end : GrenSyntax.Location
                        end =
                            case typeDeclarationAfterDocumentation.tailVariantsReverse of
                                (GrenSyntax.Node range _) :: _ ->
                                    range.end

                                [] ->
                                    let
                                        (GrenSyntax.Node headVariantRange _) =
                                            typeDeclarationAfterDocumentation.headVariant
                                    in
                                    headVariantRange.end
                    in
                    { comments = allComments
                    , syntax =
                        GrenSyntax.Node { start = start, end = end }
                            (GrenSyntax.CustomTypeDeclaration
                                { documentation = Nothing
                                , name = typeDeclarationAfterDocumentation.name
                                , generics = typeDeclarationAfterDocumentation.parameters
                                , constructors =
                                    typeDeclarationAfterDocumentation.headVariant
                                        :: List.reverse typeDeclarationAfterDocumentation.tailVariantsReverse
                                }
                            )
                    }

                TypeAliasDeclarationWithoutDocumentation typeAliasDeclarationAfterDocumentation ->
                    let
                        (GrenSyntax.Node typeAnnotationRange _) =
                            typeAliasDeclarationAfterDocumentation.typeAnnotation
                    in
                    { comments = allComments
                    , syntax =
                        GrenSyntax.Node { start = start, end = typeAnnotationRange.end }
                            (GrenSyntax.AliasDeclaration
                                { documentation = Nothing
                                , name = typeAliasDeclarationAfterDocumentation.name
                                , generics = typeAliasDeclarationAfterDocumentation.parameters
                                , typeAnnotation = typeAliasDeclarationAfterDocumentation.typeAnnotation
                                }
                            )
                    }
        )
        (ParserFast.keywordFollowedBy "type" whitespaceAndComments)
        (ParserFast.oneOf2
            typeAliasDefinitionWithoutDocumentationAfterTypePrefix
            choiceTypeDefinitionWithoutDocumentationAfterTypePrefix
        )


typeAliasDefinitionWithoutDocumentationAfterTypePrefix : Parser (WithComments TypeOrTypeAliasDeclarationWithoutDocumentation)
typeAliasDefinitionWithoutDocumentationAfterTypePrefix =
    ParserFast.map6
        (\commentsAfterAlias name commentsAfterName parameters commentsAfterEqual typeAnnotationResult ->
            { comments =
                commentsAfterAlias
                    |> ropePrependTo commentsAfterName
                    |> ropePrependTo parameters.comments
                    |> ropePrependTo commentsAfterEqual
                    |> ropePrependTo typeAnnotationResult.comments
            , syntax =
                TypeAliasDeclarationWithoutDocumentation
                    { name = name
                    , parameters = parameters.syntax
                    , typeAnnotation = typeAnnotationResult.syntax
                    }
            }
        )
        (ParserFast.keywordFollowedBy "alias" whitespaceAndComments)
        nameUppercaseNode
        whitespaceAndComments
        typeGenericListEquals
        whitespaceAndComments
        type_


choiceTypeDefinitionWithoutDocumentationAfterTypePrefix : Parser (WithComments TypeOrTypeAliasDeclarationWithoutDocumentation)
choiceTypeDefinitionWithoutDocumentationAfterTypePrefix =
    ParserFast.map7
        (\name commentsAfterName parameters commentsAfterEqual commentsBeforeHeadVariant headVariant tailVariantsReverse ->
            { comments =
                commentsAfterName
                    |> ropePrependTo parameters.comments
                    |> ropePrependTo commentsAfterEqual
                    |> ropePrependTo commentsBeforeHeadVariant
                    |> ropePrependTo headVariant.comments
                    |> ropePrependTo tailVariantsReverse.comments
            , syntax =
                TypeDeclarationWithoutDocumentation
                    { name = name
                    , parameters = parameters.syntax
                    , headVariant = headVariant.syntax
                    , tailVariantsReverse = tailVariantsReverse.syntax
                    }
            }
        )
        nameUppercaseNode
        whitespaceAndComments
        typeGenericListEquals
        whitespaceAndComments
        (ParserFast.orSucceed
            (ParserFast.symbolFollowedBy "|" whitespaceAndComments)
            ropeEmpty
        )
        variantDeclarationFollowedByWhitespaceAndComments
        (manyWithCommentsReverse
            (ParserFast.symbolFollowedBy "|"
                (ParserFast.map3
                    (\commentsBeforePipe commentsWithExtraPipe variantResult ->
                        { comments =
                            commentsBeforePipe
                                |> ropePrependTo commentsWithExtraPipe
                                |> ropePrependTo variantResult.comments
                        , syntax = variantResult.syntax
                        }
                    )
                    whitespaceAndComments
                    (ParserFast.orSucceed
                        (ParserFast.symbolFollowedBy "|" whitespaceAndComments)
                        ropeEmpty
                    )
                    variantDeclarationFollowedByWhitespaceAndComments
                )
            )
        )


variantDeclarationFollowedByWhitespaceAndComments : Parser (WithComments (GrenSyntax.Node GrenSyntax.ValueConstructor))
variantDeclarationFollowedByWhitespaceAndComments =
    ParserFast.map3
        (\nameNode commentsAfterName argumentsReverse ->
            let
                (GrenSyntax.Node nameRange _) =
                    nameNode

                fullRange : GrenSyntax.Range
                fullRange =
                    case argumentsReverse.syntax of
                        (GrenSyntax.Node lastArgRange _) :: _ ->
                            { start = nameRange.start, end = lastArgRange.end }

                        [] ->
                            nameRange
            in
            { comments =
                commentsAfterName
                    |> ropePrependTo argumentsReverse.comments
            , syntax =
                GrenSyntax.Node fullRange
                    { name = nameNode
                    , arguments = List.reverse argumentsReverse.syntax
                    }
            }
        )
        nameUppercaseNode
        whitespaceAndComments
        (manyWithCommentsReverse
            (positivelyIndentedFollowedBy
                (ParserFast.map2
                    (\typeAnnotationResult commentsAfter ->
                        { comments = typeAnnotationResult.comments |> ropePrependTo commentsAfter
                        , syntax = typeAnnotationResult.syntax
                        }
                    )
                    typeNotSpaceSeparated
                    whitespaceAndComments
                )
            )
        )


typeGenericListEquals : Parser (WithComments (List (GrenSyntax.Node String)))
typeGenericListEquals =
    untilWithComments (ParserFast.symbol "=" ())
        (ParserFast.map2
            (\name commentsAfterName ->
                { comments = commentsAfterName
                , syntax = name
                }
            )
            nameLowercaseNodeUnderscoreSuffixingKeywords
            whitespaceAndComments
        )


{-| [`Parser`](#Parser) for an [`GrenSyntax.TypeAnnotation`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-TypeAnnotation#TypeAnnotation)
-}
type_ : Parser { comments : Comments, syntax : GrenSyntax.Node GrenSyntax.TypeAnnotation }
type_ =
    ParserFast.loopWhileSucceedsOntoResultFromParserRightToLeftStackUnsafe
        (ParserFast.map2
            (\startType commentsAfter ->
                { comments =
                    startType.comments
                        |> ropePrependTo commentsAfter
                , syntax = startType.syntax
                }
            )
            (ParserFast.lazy (\() -> typeNotFunction))
            whitespaceAndComments
        )
        (ParserFast.symbolFollowedBy "->"
            (ParserFast.map4
                (\commentsAfterArrow commentsWithExtraArrow typeAnnotationResult commentsAfterType ->
                    { comments =
                        commentsAfterArrow
                            |> ropePrependTo commentsWithExtraArrow
                            |> ropePrependTo typeAnnotationResult.comments
                            |> ropePrependTo commentsAfterType
                    , syntax = typeAnnotationResult.syntax
                    }
                )
                whitespaceAndComments
                (ParserFast.orSucceed
                    (ParserFast.symbolFollowedBy "->" whitespaceAndComments)
                    ropeEmpty
                )
                (ParserFast.lazy (\() -> typeNotFunction))
                whitespaceAndComments
            )
        )
        (\inType outType ->
            { comments =
                inType.comments
                    |> ropePrependTo outType.comments
            , syntax =
                GrenSyntax.nodeCombine
                    GrenSyntax.TypeAnnotationFunction
                    inType.syntax
                    outType.syntax
            }
        )


typeNotSpaceSeparated : Parser (WithComments (GrenSyntax.Node GrenSyntax.TypeAnnotation))
typeNotSpaceSeparated =
    ParserFast.oneOf4
        typeParenthesizedOrOldUnit
        typeConstructWithoutArguments
        typeVariable
        typeRecordOrRecordExtension


typeNotFunction : Parser (WithComments (GrenSyntax.Node GrenSyntax.TypeAnnotation))
typeNotFunction =
    ParserFast.oneOf4
        typeParenthesizedOrOldUnit
        typeConstructWithArgumentsFollowedByWhitespaceAndComments
        typeVariable
        typeRecordOrRecordExtension


typeParenthesizedOrOldUnit : Parser (WithComments (GrenSyntax.Node GrenSyntax.TypeAnnotation))
typeParenthesizedOrOldUnit =
    ParserFast.symbolFollowedBy "("
        (ParserFast.oneOf2
            (ParserFast.symbolWithEndLocation ")"
                (\end ->
                    { comments = ropeEmpty
                    , syntax =
                        GrenSyntax.Node
                            { start = { row = end.row, column = end.column - 2 }
                            , end = end
                            }
                            GrenSyntax.TypeAnnotationUnit
                    }
                )
            )
            (ParserFast.map3WithRange
                (\rangeAfterOpeningParens commentsBeforeFirstPart inParens commentsAfterFirstPart ->
                    { comments =
                        commentsBeforeFirstPart
                            |> ropePrependTo inParens.comments
                            |> ropePrependTo commentsAfterFirstPart
                    , syntax =
                        GrenSyntax.Node
                            { start = { row = rangeAfterOpeningParens.start.row, column = rangeAfterOpeningParens.start.column - 1 }
                            , end = rangeAfterOpeningParens.end
                            }
                            (GrenSyntax.TypeAnnotationParenthesized
                                inParens.syntax
                            )
                    }
                )
                whitespaceAndComments
                type_
                (whitespaceAndComments
                    |> ParserFast.followedBySymbol ")"
                )
            )
        )


typeVariable : Parser (WithComments (GrenSyntax.Node GrenSyntax.TypeAnnotation))
typeVariable =
    nameLowercaseMapWithRange
        (\range var ->
            { comments = ropeEmpty
            , syntax =
                GrenSyntax.Node range (GrenSyntax.TypeAnnotationVariable var)
            }
        )


typeRecordOrRecordExtension : Parser (WithComments (GrenSyntax.Node GrenSyntax.TypeAnnotation))
typeRecordOrRecordExtension =
    ParserFast.map2WithRange
        (\range commentsBefore afterCurly ->
            case afterCurly of
                Nothing ->
                    { comments = commentsBefore
                    , syntax =
                        GrenSyntax.Node range typeRecordEmpty
                    }

                Just afterCurlyResult ->
                    { comments =
                        commentsBefore
                            |> ropePrependTo afterCurlyResult.comments
                    , syntax =
                        GrenSyntax.Node range afterCurlyResult.syntax
                    }
        )
        (ParserFast.symbolFollowedBy "{" whitespaceAndComments)
        (ParserFast.oneOf2
            (ParserFast.symbol "}" Nothing)
            (ParserFast.map4
                (\commentsBeforeFirstName firstNameNode commentsAfterFirstName afterFirstName ->
                    Just
                        { comments =
                            commentsBeforeFirstName
                                |> ropePrependTo commentsAfterFirstName
                                |> ropePrependTo afterFirstName.comments
                        , syntax =
                            case afterFirstName.syntax of
                                RecordExtensionExpressionAfterName fields ->
                                    GrenSyntax.TypeAnnotationRecordExtension firstNameNode fields

                                FieldsAfterName fieldsAfterName ->
                                    GrenSyntax.TypeAnnotationRecord
                                        (GrenSyntax.nodeCombine Tuple.pair
                                            firstNameNode
                                            fieldsAfterName.firstFieldValue
                                            :: fieldsAfterName.tailFields
                                        )
                        }
                )
                (ParserFast.orSucceed
                    (ParserFast.symbolFollowedBy "," whitespaceAndComments)
                    ropeEmpty
                )
                nameLowercaseNodeUnderscoreSuffixingKeywords
                whitespaceAndComments
                (ParserFast.oneOf2
                    (ParserFast.symbolFollowedBy "|"
                        (ParserFast.map3WithRange
                            (\range commentsBefore head tail ->
                                { comments =
                                    commentsBefore
                                        |> ropePrependTo head.comments
                                        |> ropePrependTo tail.comments
                                , syntax =
                                    RecordExtensionExpressionAfterName
                                        (GrenSyntax.Node range (head.syntax :: tail.syntax))
                                }
                            )
                            whitespaceAndComments
                            typeRecordFieldDefinitionFollowedByWhitespaceAndComments
                            (manyWithComments
                                (ParserFast.symbolFollowedBy ","
                                    (ParserFast.map3
                                        (\commentsBefore commentsWithExtraComma field ->
                                            { comments =
                                                commentsBefore
                                                    |> ropePrependTo commentsWithExtraComma
                                                    |> ropePrependTo field.comments
                                            , syntax = field.syntax
                                            }
                                        )
                                        whitespaceAndComments
                                        (ParserFast.orSucceed
                                            (ParserFast.symbolFollowedBy "," whitespaceAndComments)
                                            ropeEmpty
                                        )
                                        typeRecordFieldDefinitionFollowedByWhitespaceAndComments
                                    )
                                )
                            )
                        )
                    )
                    (ParserFast.map4
                        (\commentsBeforeFirstFieldValue firstFieldValue commentsAfterFirstFieldValue tailFields ->
                            { comments =
                                commentsBeforeFirstFieldValue
                                    |> ropePrependTo firstFieldValue.comments
                                    |> ropePrependTo commentsAfterFirstFieldValue
                                    |> ropePrependTo tailFields.comments
                            , syntax =
                                FieldsAfterName
                                    { firstFieldValue = firstFieldValue.syntax
                                    , tailFields = tailFields.syntax
                                    }
                            }
                        )
                        (ParserFast.oneOf2OrSucceed
                            (ParserFast.symbolFollowedBy ":" whitespaceAndComments)
                            (ParserFast.symbolFollowedBy "=" whitespaceAndComments)
                            ropeEmpty
                        )
                        type_
                        whitespaceAndComments
                        (ParserFast.orSucceed
                            (ParserFast.symbolFollowedBy "," recordFieldsTypeAnnotation)
                            { comments = ropeEmpty, syntax = [] }
                        )
                    )
                )
                |> ParserFast.followedBySymbol "}"
            )
        )


typeRecordEmpty : GrenSyntax.TypeAnnotation
typeRecordEmpty =
    GrenSyntax.TypeAnnotationRecord []


type RecordFieldsOrExtensionAfterName
    = RecordExtensionExpressionAfterName (GrenSyntax.Node (List (GrenSyntax.Node GrenSyntax.TypeAnnotationRecordField)))
    | FieldsAfterName { firstFieldValue : GrenSyntax.Node GrenSyntax.TypeAnnotation, tailFields : List (GrenSyntax.Node GrenSyntax.TypeAnnotationRecordField) }


recordFieldsTypeAnnotation : Parser (WithComments (List (GrenSyntax.Node GrenSyntax.TypeAnnotationRecordField)))
recordFieldsTypeAnnotation =
    ParserFast.map4
        (\commentsBefore commentsWithExtraComma head tail ->
            { comments =
                commentsWithExtraComma
                    |> ropePrependTo commentsBefore
                    |> ropePrependTo head.comments
                    |> ropePrependTo tail.comments
            , syntax = head.syntax :: tail.syntax
            }
        )
        whitespaceAndComments
        (ParserFast.orSucceed
            (ParserFast.symbolFollowedBy "," whitespaceAndComments)
            ropeEmpty
        )
        typeRecordFieldDefinitionFollowedByWhitespaceAndComments
        (manyWithComments
            (ParserFast.symbolFollowedBy ","
                (ParserFast.map3
                    (\commentsBefore commentsWithExtraComma field ->
                        { comments =
                            commentsBefore
                                |> ropePrependTo commentsWithExtraComma
                                |> ropePrependTo field.comments
                        , syntax = field.syntax
                        }
                    )
                    whitespaceAndComments
                    (ParserFast.orSucceed
                        (ParserFast.symbolFollowedBy "," whitespaceAndComments)
                        ropeEmpty
                    )
                    typeRecordFieldDefinitionFollowedByWhitespaceAndComments
                )
            )
        )


typeRecordFieldDefinitionFollowedByWhitespaceAndComments : Parser (WithComments (GrenSyntax.Node GrenSyntax.TypeAnnotationRecordField))
typeRecordFieldDefinitionFollowedByWhitespaceAndComments =
    ParserFast.map5WithRange
        (\range name commentsAfterName commentsAfterColon value commentsAfterValue ->
            { comments =
                commentsAfterName
                    |> ropePrependTo commentsAfterColon
                    |> ropePrependTo value.comments
                    |> ropePrependTo commentsAfterValue
            , syntax = GrenSyntax.Node range ( name, value.syntax )
            }
        )
        nameLowercaseNodeUnderscoreSuffixingKeywords
        whitespaceAndComments
        (ParserFast.oneOf2OrSucceed
            (ParserFast.symbolFollowedBy ":" whitespaceAndComments)
            (ParserFast.symbolFollowedBy "=" whitespaceAndComments)
            ropeEmpty
        )
        type_
        -- This extra whitespace is just included for compatibility with earlier version
        -- TODO for v8: move to recordFieldsTypeAnnotation
        whitespaceAndComments


typeConstructWithoutArguments : Parser (WithComments (GrenSyntax.Node GrenSyntax.TypeAnnotation))
typeConstructWithoutArguments =
    ParserFast.map2WithRange
        (\range startName afterStartName ->
            let
                name : ( GrenSyntax.ModuleName, String )
                name =
                    case afterStartName of
                        Nothing ->
                            ( [], startName )

                        Just ( qualificationAfterStartName, unqualified ) ->
                            ( startName :: qualificationAfterStartName, unqualified )
            in
            { comments = ropeEmpty
            , syntax =
                GrenSyntax.Node range
                    (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node range name) [])
            }
        )
        nameUppercase
        maybeDotNamesUppercaseTuple


maybeDotNamesUppercaseTuple : Parser (Maybe ( List String, String ))
maybeDotNamesUppercaseTuple =
    ParserFast.map2OrSucceed
        (\firstName afterFirstName ->
            case afterFirstName of
                Nothing ->
                    Just ( [], firstName )

                Just ( qualificationAfter, unqualified ) ->
                    Just ( firstName :: qualificationAfter, unqualified )
        )
        (ParserFast.symbolFollowedBy "." nameUppercase)
        (ParserFast.lazy (\() -> maybeDotNamesUppercaseTuple))
        Nothing


typeConstructWithArgumentsFollowedByWhitespaceAndComments : Parser (WithComments (GrenSyntax.Node GrenSyntax.TypeAnnotation))
typeConstructWithArgumentsFollowedByWhitespaceAndComments =
    ParserFast.map3
        (\nameNode commentsAfterName argsReverse ->
            let
                (GrenSyntax.Node nameRange _) =
                    nameNode

                range : GrenSyntax.Range
                range =
                    case argsReverse.syntax of
                        [] ->
                            nameRange

                        (GrenSyntax.Node lastArgRange _) :: _ ->
                            { start = nameRange.start, end = lastArgRange.end }
            in
            { comments =
                commentsAfterName
                    |> ropePrependTo argsReverse.comments
            , syntax =
                GrenSyntax.Node range (GrenSyntax.TypeAnnotationConstruct nameNode (List.reverse argsReverse.syntax))
            }
        )
        (ParserFast.map2WithRange
            (\range startName afterStartName ->
                let
                    name : ( GrenSyntax.ModuleName, String )
                    name =
                        case afterStartName of
                            Nothing ->
                                ( [], startName )

                            Just ( qualificationAfterStartName, unqualified ) ->
                                ( startName :: qualificationAfterStartName, unqualified )
                in
                GrenSyntax.Node range name
            )
            nameUppercase
            maybeDotNamesUppercaseTuple
        )
        whitespaceAndComments
        (manyWithCommentsReverse
            (positivelyIndentedFollowedBy
                (ParserFast.map2
                    (\typeAnnotationResult commentsAfter ->
                        { comments =
                            typeAnnotationResult.comments
                                |> ropePrependTo commentsAfter
                        , syntax = typeAnnotationResult.syntax
                        }
                    )
                    typeNotSpaceSeparated
                    whitespaceAndComments
                )
            )
        )


subExpression : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
subExpression =
    -- functionally, a simple oneOf would be correct as well.
    -- However, since this parser is called _a lot_,
    --   we squeeze out a bit more speed by de-duplicating slices etc
    ParserFast.offsetSourceAndThen
        (\offset source ->
            case String.slice offset (offset + 1) source of
                "\"" ->
                    expressionString

                "(" ->
                    expressionStartingWithParensOpeningIfNecessaryFollowedByRecordAccess

                "[" ->
                    expressionListOrGlsl

                "{" ->
                    expressionRecordFollowedByRecordAccess

                "." ->
                    expressionRecordAccessFunction

                "-" ->
                    expressionNegation

                "'" ->
                    expressionChar

                _ ->
                    referenceOrNumberExpression
        )


referenceOrNumberExpression : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
referenceOrNumberExpression =
    ParserFast.oneOf3
        expressionQualifiedOrVariantOrRecordConstructorReferenceFollowedByRecordAccess
        expressionUnqualifiedFunctionReferenceFollowedByRecordAccess
        expressionNumber


followedByMultiRecordAccess : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression)) -> Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
followedByMultiRecordAccess beforeRecordAccesses =
    ParserFast.loopWhileSucceedsOntoResultFromParser
        (ParserFast.symbolFollowedBy "."
            nameLowercaseNodeUnderscoreSuffixingKeywords
        )
        beforeRecordAccesses
        (\fieldNode leftResult ->
            let
                (GrenSyntax.Node fieldRange _) =
                    fieldNode

                (GrenSyntax.Node leftRange _) =
                    leftResult.syntax
            in
            { comments = leftResult.comments
            , syntax =
                GrenSyntax.Node { start = leftRange.start, end = fieldRange.end }
                    (GrenSyntax.ExpressionRecordAccess leftResult.syntax fieldNode)
            }
        )
        Basics.identity


precedence1ApR : InfixOperatorInfo
precedence1ApR =
    infixLeft 1 "|>"


precedence1ApL : InfixOperatorInfo
precedence1ApL =
    infixRight 1 "<|"


precedence2Or : InfixOperatorInfo
precedence2Or =
    infixRight 2 "||"


precedence3And : InfixOperatorInfo
precedence3And =
    infixRight 3 "&&"


precedence4Eq : InfixOperatorInfo
precedence4Eq =
    infixNonAssociative 4 "=="


precedence4Neq : InfixOperatorInfo
precedence4Neq =
    infixNonAssociative 4 "/="


precedence4Le : InfixOperatorInfo
precedence4Le =
    infixNonAssociative 4 "<="


precedence4Ge : InfixOperatorInfo
precedence4Ge =
    infixNonAssociative 4 ">="


precedence4Gt : InfixOperatorInfo
precedence4Gt =
    infixNonAssociative 4 ">"


precedence4Lt : InfixOperatorInfo
precedence4Lt =
    infixNonAssociative 4 "<"


precedence5append : InfixOperatorInfo
precedence5append =
    infixRight 5 "++"


precedence5Cons : InfixOperatorInfo
precedence5Cons =
    infixRight 5 "::"


precedence5Keep : InfixOperatorInfo
precedence5Keep =
    infixLeft 5 "|="


precedence6Add : InfixOperatorInfo
precedence6Add =
    infixLeft 6 "+"


precedence6Sub : InfixOperatorInfo
precedence6Sub =
    infixLeft 6 "-"


precedence6Ignore : InfixOperatorInfo
precedence6Ignore =
    infixLeft 6 "|."


precedence7Idiv : InfixOperatorInfo
precedence7Idiv =
    infixLeft 7 "//"


precedence7Mul : InfixOperatorInfo
precedence7Mul =
    infixLeft 7 "*"


precedence7Fdiv : InfixOperatorInfo
precedence7Fdiv =
    infixLeft 7 "/"


precedence7Slash : InfixOperatorInfo
precedence7Slash =
    infixRight 7 "</>"


precedence8QuestionMark : InfixOperatorInfo
precedence8QuestionMark =
    infixLeft 8 "<?>"


precedence8Pow : InfixOperatorInfo
precedence8Pow =
    infixRight 8 "^"


precedence9ComposeR : InfixOperatorInfo
precedence9ComposeR =
    infixRight 9 ">>"


precedence9ComposeL : InfixOperatorInfo
precedence9ComposeL =
    infixLeft 9 "<<"


{-| [`Parser`](#Parser) for an [`GrenSyntax.Expression`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Expression#Expression)
-}
expression : Parser { comments : Comments, syntax : GrenSyntax.Node GrenSyntax.Expression }
expression =
    expressionFollowedByWhitespaceAndComments


expressionFollowedByWhitespaceAndComments : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
expressionFollowedByWhitespaceAndComments =
    ParserFast.map2
        (\expressionResult maybeCases ->
            case maybeCases of
                Nothing ->
                    expressionResult

                Just cases ->
                    { comments =
                        expressionResult.comments
                            |> ropePrependTo cases.comments
                    , syntax =
                        GrenSyntax.Node
                            { start =
                                expressionResult.syntax
                                    |> GrenSyntax.nodeRange
                                    |> .start
                            , end = cases.end
                            }
                            (GrenSyntax.ExpressionCaseOf
                                { expression = expressionResult.syntax
                                , cases = cases.cases
                                }
                            )
                    }
        )
        (extendedSubExpressionFollowedByWhitespaceAndComments
            { afterCommitting = .extensionRightParser
            , validateRightPrecedence = Just
            }
        )
        (ParserFast.orSucceed
            (ParserFast.keywordFollowedBy "case"
                (ParserFast.map2
                    (\commentsAfterCase casesResult ->
                        let
                            ( firstCase, lastToSecondCase ) =
                                casesResult.syntax
                        in
                        Just
                            { comments =
                                commentsAfterCase
                                    |> ropePrependTo casesResult.comments
                            , end =
                                case lastToSecondCase of
                                    ( _, GrenSyntax.Node lastCaseExpressionRange _ ) :: _ ->
                                        lastCaseExpressionRange.end

                                    [] ->
                                        let
                                            ( _, GrenSyntax.Node firstCaseExpressionRange _ ) =
                                                firstCase
                                        in
                                        firstCaseExpressionRange.end
                            , cases = firstCase :: List.reverse lastToSecondCase
                            }
                    )
                    whitespaceAndComments
                    (ParserFast.withIndentSetToColumn
                        (ParserFast.lazy (\() -> caseStatementsFollowedByWhitespaceAndComments))
                    )
                )
            )
            Nothing
        )


glslExpressionAfterOpeningSquareBracket : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
glslExpressionAfterOpeningSquareBracket =
    ParserFast.symbolFollowedBy "glsl|"
        (ParserFast.mapWithRange
            (\range s ->
                { comments = ropeEmpty
                , syntax =
                    GrenSyntax.Node
                        -- TODO for v8: don't include extra end width (from bug in gren/parser) in range
                        { start = { row = range.start.row, column = range.start.column - 6 }
                        , end = { row = range.end.row, column = range.end.column + 2 }
                        }
                        (GrenSyntax.ExpressionGlsl s)
                }
            )
            (ParserFast.loopUntil
                (ParserFast.symbol "|]" ())
                (ParserFast.oneOf2
                    (ParserFast.symbol "|" "|")
                    (ParserFast.while
                        (\c ->
                            case c of
                                '|' ->
                                    False

                                _ ->
                                    True
                        )
                    )
                )
                ""
                (\extension soFar ->
                    soFar ++ extension ++ ""
                )
                identity
            )
        )


expressionListOrGlsl : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
expressionListOrGlsl =
    ParserFast.symbolFollowedBy "[" expressionAfterOpeningSquareBracket


expressionAfterOpeningSquareBracket : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
expressionAfterOpeningSquareBracket =
    ParserFast.oneOf2
        glslExpressionAfterOpeningSquareBracket
        (ParserFast.map2WithRange
            (\range commentsBefore elements ->
                { comments = commentsBefore |> ropePrependTo elements.comments
                , syntax =
                    GrenSyntax.Node
                        { start = { row = range.start.row, column = range.start.column - 1 }
                        , end = range.end
                        }
                        elements.syntax
                }
            )
            whitespaceAndComments
            (ParserFast.oneOf2
                (ParserFast.symbol "]" { comments = ropeEmpty, syntax = GrenSyntax.ExpressionArray [] })
                (ParserFast.map3
                    (\commentsBeforeHead head tail ->
                        { comments =
                            commentsBeforeHead
                                |> ropePrependTo head.comments
                                |> ropePrependTo tail.comments
                        , syntax = GrenSyntax.ExpressionArray (head.syntax :: tail.syntax)
                        }
                    )
                    (ParserFast.orSucceed
                        (ParserFast.symbolFollowedBy "," whitespaceAndComments)
                        ropeEmpty
                    )
                    expressionFollowedByWhitespaceAndComments
                    (manyWithComments
                        (ParserFast.symbolFollowedBy ","
                            (ParserFast.map3
                                (\commentsBefore commentsWithExtraComma expressionResult ->
                                    { comments =
                                        commentsBefore
                                            |> ropePrependTo commentsWithExtraComma
                                            |> ropePrependTo expressionResult.comments
                                    , syntax = expressionResult.syntax
                                    }
                                )
                                whitespaceAndComments
                                (ParserFast.orSucceed
                                    (ParserFast.symbolFollowedBy "," whitespaceAndComments)
                                    ropeEmpty
                                )
                                expressionFollowedByWhitespaceAndComments
                            )
                        )
                    )
                    |> ParserFast.followedBySymbol "]"
                )
            )
        )


expressionRecordFollowedByRecordAccess : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
expressionRecordFollowedByRecordAccess =
    ParserFast.symbolFollowedBy "{"
        (ParserFast.map2WithRange
            (\range commentsBefore afterCurly ->
                { comments =
                    commentsBefore
                        |> ropePrependTo afterCurly.comments
                , syntax = GrenSyntax.Node (rangeMoveStartLeftByOneColumn range) afterCurly.syntax
                }
            )
            whitespaceAndComments
            recordContentsFollowedByCurlyEnd
            |> followedByMultiRecordAccess
        )


recordContentsFollowedByCurlyEnd : Parser (WithComments GrenSyntax.Expression)
recordContentsFollowedByCurlyEnd =
    ParserFast.oneOf3
        (ParserFast.map5
            (\nameNode commentsAfterName afterNameBeforeFields tailFields commentsBeforeClosingCurly ->
                { comments =
                    commentsAfterName
                        |> ropePrependTo afterNameBeforeFields.comments
                        |> ropePrependTo tailFields.comments
                        |> ropePrependTo commentsBeforeClosingCurly
                , syntax =
                    case afterNameBeforeFields.syntax of
                        RecordUpdateFirstSetter firstField ->
                            GrenSyntax.ExpressionRecordUpdate nameNode (firstField :: tailFields.syntax)

                        FieldsFirstValue firstFieldValue ->
                            GrenSyntax.ExpressionRecord
                                (GrenSyntax.nodeCombine Tuple.pair
                                    nameNode
                                    firstFieldValue
                                    :: tailFields.syntax
                                )

                        FieldsFirstValuePunned () ->
                            GrenSyntax.ExpressionRecord
                                (GrenSyntax.Node (nameNode |> GrenSyntax.nodeRange)
                                    ( nameNode
                                    , -- dummy
                                      GrenSyntax.Node
                                        { start = nameNode |> GrenSyntax.nodeRange |> .end
                                        , end = nameNode |> GrenSyntax.nodeRange |> .end
                                        }
                                        (GrenSyntax.ExpressionReference []
                                            (nameNode |> GrenSyntax.nodeValue)
                                        )
                                    )
                                    :: tailFields.syntax
                                )
                }
            )
            nameLowercaseNodeUnderscoreSuffixingKeywords
            whitespaceAndComments
            (ParserFast.oneOf2
                (ParserFast.symbolFollowedBy "|"
                    (ParserFast.map2
                        (\commentsBefore setterResult ->
                            { comments = commentsBefore |> ropePrependTo setterResult.comments
                            , syntax = RecordUpdateFirstSetter setterResult.syntax
                            }
                        )
                        whitespaceAndComments
                        recordSetterNodeFollowedByWhitespaceAndComments
                    )
                )
                (ParserFast.map2
                    (\commentsBefore maybeValueResult ->
                        case maybeValueResult of
                            Nothing ->
                                { comments = commentsBefore
                                , syntax = FieldsFirstValuePunned ()
                                }

                            Just expressionResult ->
                                { comments =
                                    commentsBefore
                                        |> ropePrependTo expressionResult.comments
                                , syntax = FieldsFirstValue expressionResult.syntax
                                }
                    )
                    (ParserFast.oneOf2OrSucceed
                        (ParserFast.symbolFollowedBy ":" whitespaceAndComments)
                        (ParserFast.symbolFollowedBy "=" whitespaceAndComments)
                        ropeEmpty
                    )
                    (ParserFast.mapOrSucceed
                        Just
                        expressionFollowedByWhitespaceAndComments
                        Nothing
                    )
                )
            )
            recordFields
            (whitespaceAndComments |> ParserFast.followedBySymbol "}")
        )
        (ParserFast.symbol "}" { comments = ropeEmpty, syntax = GrenSyntax.ExpressionRecord [] })
        -- prefixed comma
        (ParserFast.map2
            (\recordFieldsResult commentsAfterFields ->
                { comments =
                    recordFieldsResult.comments
                        |> ropePrependTo commentsAfterFields
                , syntax =
                    GrenSyntax.ExpressionRecord recordFieldsResult.syntax
                }
            )
            recordFields
            (whitespaceAndComments |> ParserFast.followedBySymbol "}")
        )


type RecordFieldsOrUpdateAfterName
    = RecordUpdateFirstSetter (GrenSyntax.Node GrenSyntax.RecordSetter)
    | FieldsFirstValue (GrenSyntax.Node GrenSyntax.Expression)
    | FieldsFirstValuePunned ()


recordFields : Parser (WithComments (List (GrenSyntax.Node GrenSyntax.RecordSetter)))
recordFields =
    manyWithComments
        (ParserFast.symbolFollowedBy ","
            (ParserFast.map3
                (\commentsBefore commentsWithExtraComma setterResult ->
                    { comments =
                        commentsBefore
                            |> ropePrependTo commentsWithExtraComma
                            |> ropePrependTo setterResult.comments
                    , syntax = setterResult.syntax
                    }
                )
                whitespaceAndComments
                (ParserFast.orSucceed
                    (ParserFast.symbolFollowedBy "," whitespaceAndComments)
                    ropeEmpty
                )
                recordSetterNodeFollowedByWhitespaceAndComments
            )
        )


recordSetterNodeFollowedByWhitespaceAndComments : Parser (WithComments (GrenSyntax.Node GrenSyntax.RecordSetter))
recordSetterNodeFollowedByWhitespaceAndComments =
    ParserFast.map4WithRange
        (\range nameNode commentsAfterName commentsAfterEquals maybeValueResult ->
            -- This extra whitespace is just included for compatibility with earlier version
            -- TODO for v8: remove
            case maybeValueResult of
                Nothing ->
                    { comments =
                        commentsAfterName |> ropePrependTo commentsAfterEquals
                    , syntax =
                        GrenSyntax.Node range
                            ( nameNode
                            , -- dummy
                              GrenSyntax.Node
                                { start = nameNode |> GrenSyntax.nodeRange |> .end
                                , end = nameNode |> GrenSyntax.nodeRange |> .end
                                }
                                (GrenSyntax.ExpressionReference []
                                    (nameNode |> GrenSyntax.nodeValue)
                                )
                            )
                    }

                Just expressionResult ->
                    { comments =
                        commentsAfterName
                            |> ropePrependTo commentsAfterEquals
                            |> ropePrependTo expressionResult.comments
                    , syntax =
                        GrenSyntax.Node range
                            ( nameNode
                            , expressionResult.syntax
                            )
                    }
        )
        nameLowercaseNodeUnderscoreSuffixingKeywords
        whitespaceAndComments
        (ParserFast.oneOf2OrSucceed
            (ParserFast.symbolFollowedBy ":" whitespaceAndComments)
            (ParserFast.symbolFollowedBy "=" whitespaceAndComments)
            ropeEmpty
        )
        (ParserFast.mapOrSucceed
            Just
            expressionFollowedByWhitespaceAndComments
            Nothing
        )


expressionString : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
expressionString =
    singleOrTripleQuotedStringLiteralMapWithRange
        (\range string ->
            { comments = ropeEmpty
            , syntax =
                GrenSyntax.Node range
                    (GrenSyntax.ExpressionString string)
            }
        )


expressionChar : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
expressionChar =
    characterLiteralMapWithRange
        (\range char ->
            { comments = ropeEmpty
            , syntax =
                GrenSyntax.Node range
                    (GrenSyntax.ExpressionChar char)
            }
        )


expressionLambdaFollowedByWhitespaceAndComments : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
expressionLambdaFollowedByWhitespaceAndComments =
    ParserFast.map6WithStartLocation
        (\start commentsAfterBackslash parameter0 commentsAfterParameter0 parameter1Up commentsAfterArrow expressionResult ->
            let
                (GrenSyntax.Node expressionRange _) =
                    expressionResult.syntax
            in
            { comments =
                commentsAfterBackslash
                    |> ropePrependTo parameter0.comments
                    |> ropePrependTo commentsAfterParameter0
                    |> ropePrependTo parameter1Up.comments
                    |> ropePrependTo commentsAfterArrow
                    |> ropePrependTo expressionResult.comments
            , syntax =
                GrenSyntax.Node
                    { start = start
                    , end = expressionRange.end
                    }
                    (GrenSyntax.ExpressionLambda
                        { args = parameter0.syntax :: parameter1Up.syntax
                        , expression = expressionResult.syntax
                        }
                    )
            }
        )
        (ParserFast.symbolFollowedBy "\\" whitespaceAndComments)
        patternNotSpaceSeparated
        whitespaceAndComments
        (untilWithComments
            (ParserFast.oneOf3
                (ParserFast.symbol "->" ())
                (ParserFast.symbol "=>" ())
                (ParserFast.symbol "." ())
            )
            (ParserFast.map2
                (\patternResult commentsAfter ->
                    { comments =
                        patternResult.comments
                            |> ropePrependTo commentsAfter
                    , syntax = patternResult.syntax
                    }
                )
                patternNotSpaceSeparated
                whitespaceAndComments
            )
        )
        whitespaceAndComments
        expressionFollowedByWhitespaceAndComments


expressionCaseOfFollowedByOptimisticLayout : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
expressionCaseOfFollowedByOptimisticLayout =
    ParserFast.map4WithStartLocation
        (\start commentsAfterCase casedExpressionResult commentsAfterOf casesResult ->
            let
                ( firstCase, lastToSecondCase ) =
                    casesResult.syntax
            in
            { comments =
                commentsAfterCase
                    |> ropePrependTo casedExpressionResult.comments
                    |> ropePrependTo commentsAfterOf
                    |> ropePrependTo casesResult.comments
            , syntax =
                GrenSyntax.Node
                    { start = start
                    , end =
                        case lastToSecondCase of
                            ( _, GrenSyntax.Node lastCaseExpressionRange _ ) :: _ ->
                                lastCaseExpressionRange.end

                            [] ->
                                let
                                    ( _, GrenSyntax.Node firstCaseExpressionRange _ ) =
                                        firstCase
                                in
                                firstCaseExpressionRange.end
                    }
                    (GrenSyntax.ExpressionCaseOf
                        { expression = casedExpressionResult.syntax
                        , cases = firstCase :: List.reverse lastToSecondCase
                        }
                    )
            }
        )
        (ParserFast.keywordFollowedBy "case" whitespaceAndComments)
        expressionFollowedByWhitespaceAndComments
        (ParserFast.keywordFollowedBy "of" whitespaceAndComments)
        (ParserFast.withIndentSetToColumn
            caseStatementsFollowedByWhitespaceAndComments
        )


caseStatementsFollowedByWhitespaceAndComments : Parser (WithComments ( GrenSyntax.Case, List GrenSyntax.Case ))
caseStatementsFollowedByWhitespaceAndComments =
    ParserFast.map5
        (\firstCasePatternResult commentsAfterFirstCasePattern commentsAfterFirstCaseArrowRight firstCaseExpressionResult lastToSecondCase ->
            { comments =
                firstCasePatternResult.comments
                    |> ropePrependTo commentsAfterFirstCasePattern
                    |> ropePrependTo commentsAfterFirstCaseArrowRight
                    |> ropePrependTo firstCaseExpressionResult.comments
                    |> ropePrependTo lastToSecondCase.comments
            , syntax =
                ( ( firstCasePatternResult.syntax, firstCaseExpressionResult.syntax )
                , lastToSecondCase.syntax
                )
            }
        )
        pattern
        whitespaceAndComments
        (ParserFast.oneOf2OrSucceed
            (ParserFast.symbolFollowedBy "->" whitespaceAndComments)
            (ParserFast.symbolFollowedBy "." whitespaceAndComments)
            ropeEmpty
        )
        expressionFollowedByWhitespaceAndComments
        (manyWithCommentsReverse caseStatementFollowedByWhitespaceAndComments)


caseStatementFollowedByWhitespaceAndComments : Parser (WithComments GrenSyntax.Case)
caseStatementFollowedByWhitespaceAndComments =
    topIndentedFollowedBy
        (ParserFast.map4
            (\patternResult commentsBeforeArrowRight commentsAfterArrowRight expr ->
                { comments =
                    patternResult.comments
                        |> ropePrependTo commentsBeforeArrowRight
                        |> ropePrependTo commentsAfterArrowRight
                        |> ropePrependTo expr.comments
                , syntax = ( patternResult.syntax, expr.syntax )
                }
            )
            pattern
            whitespaceAndComments
            (ParserFast.symbolFollowedBy "->" whitespaceAndComments)
            expressionFollowedByWhitespaceAndComments
        )


letExpressionFollowedByOptimisticLayout : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
letExpressionFollowedByOptimisticLayout =
    ParserFast.map3WithStartLocation
        (\start letDeclarationsResult commentsAfterIn expressionResult ->
            let
                (GrenSyntax.Node expressionRange _) =
                    expressionResult.syntax
            in
            { comments =
                letDeclarationsResult.comments
                    |> ropePrependTo commentsAfterIn
                    |> ropePrependTo expressionResult.comments
            , syntax =
                GrenSyntax.Node
                    { start = start
                    , end = expressionRange.end
                    }
                    (GrenSyntax.ExpressionLetIn
                        { declarations = letDeclarationsResult.declarations
                        , expression = expressionResult.syntax
                        }
                    )
            }
        )
        (ParserFast.withIndentSetToColumn
            (ParserFast.keywordFollowedBy "let"
                (ParserFast.map2
                    (\commentsAfterLet letDeclarationsResult ->
                        { comments =
                            commentsAfterLet
                                |> ropePrependTo letDeclarationsResult.comments
                        , declarations = letDeclarationsResult.syntax
                        }
                    )
                    whitespaceAndComments
                    (ParserFast.withIndentSetToColumn letDeclarationsIn)
                )
            )
        )
        whitespaceAndComments
        expressionFollowedByWhitespaceAndComments


letDeclarationsIn : Parser (WithComments (List (GrenSyntax.Node GrenSyntax.LetDeclaration)))
letDeclarationsIn =
    topIndentedFollowedBy
        (ParserFast.map3
            (\headLetResult commentsAfter tailLetResult ->
                { comments =
                    headLetResult.comments
                        |> ropePrependTo commentsAfter
                        |> ropePrependTo tailLetResult.comments
                , syntax = headLetResult.syntax :: tailLetResult.syntax
                }
            )
            (ParserFast.oneOf2
                letFunctionFollowedByOptimisticLayout
                letDestructuringDeclarationFollowedByOptimisticLayout
            )
            whitespaceAndComments
            (untilWithComments
                (ParserFast.keyword "in" ())
                letBlockElementFollowedByOptimisticLayout
            )
        )


letBlockElementFollowedByOptimisticLayout : Parser (WithComments (GrenSyntax.Node GrenSyntax.LetDeclaration))
letBlockElementFollowedByOptimisticLayout =
    topIndentedFollowedBy
        (ParserFast.oneOf2
            letFunctionFollowedByOptimisticLayout
            letDestructuringDeclarationFollowedByOptimisticLayout
        )


letDestructuringDeclarationFollowedByOptimisticLayout : Parser (WithComments (GrenSyntax.Node GrenSyntax.LetDeclaration))
letDestructuringDeclarationFollowedByOptimisticLayout =
    ParserFast.map4
        (\patternResult commentsAfterPattern commentsAfterEquals expressionResult ->
            let
                (GrenSyntax.Node patternRange _) =
                    patternResult.syntax

                (GrenSyntax.Node destructuredExpressionRange _) =
                    expressionResult.syntax
            in
            { comments =
                patternResult.comments
                    |> ropePrependTo commentsAfterPattern
                    |> ropePrependTo commentsAfterEquals
                    |> ropePrependTo expressionResult.comments
            , syntax =
                GrenSyntax.Node { start = patternRange.start, end = destructuredExpressionRange.end }
                    (GrenSyntax.LetDestructuring patternResult.syntax expressionResult.syntax)
            }
        )
        patternNotSpaceSeparated
        whitespaceAndComments
        (ParserFast.symbolFollowedBy "=" whitespaceAndComments)
        expressionFollowedByWhitespaceAndComments


letFunctionFollowedByOptimisticLayout : Parser (WithComments (GrenSyntax.Node GrenSyntax.LetDeclaration))
letFunctionFollowedByOptimisticLayout =
    ParserFast.oneOf2
        (ParserFast.map6WithStartLocation
            (\startNameStart startNameNode commentsAfterStartName maybeSignature arguments commentsAfterEqual expressionResult ->
                case maybeSignature of
                    Nothing ->
                        let
                            (GrenSyntax.Node expressionRange _) =
                                expressionResult.syntax
                        in
                        { comments =
                            commentsAfterStartName
                                |> ropePrependTo arguments.comments
                                |> ropePrependTo commentsAfterEqual
                                |> ropePrependTo expressionResult.comments
                        , syntax =
                            GrenSyntax.Node { start = startNameStart, end = expressionRange.end }
                                (GrenSyntax.LetFunction
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        GrenSyntax.Node { start = startNameStart, end = expressionRange.end }
                                            { name = startNameNode
                                            , arguments = arguments.syntax
                                            , expression = expressionResult.syntax
                                            }
                                    }
                                )
                        }

                    Just signature ->
                        let
                            (GrenSyntax.Node implementationNameRange _) =
                                signature.implementationName

                            (GrenSyntax.Node expressionRange _) =
                                expressionResult.syntax
                        in
                        { comments =
                            (commentsAfterStartName |> ropePrependTo signature.comments)
                                |> ropePrependTo arguments.comments
                                |> ropePrependTo commentsAfterEqual
                                |> ropePrependTo expressionResult.comments
                        , syntax =
                            GrenSyntax.Node { start = startNameStart, end = expressionRange.end }
                                (GrenSyntax.LetFunction
                                    { documentation = Nothing
                                    , signature =
                                        Just
                                            (GrenSyntax.nodeCombine
                                                (\name value -> { name = name, typeAnnotation = value })
                                                startNameNode
                                                signature.typeAnnotation
                                            )
                                    , declaration =
                                        GrenSyntax.Node { start = implementationNameRange.start, end = expressionRange.end }
                                            { name = signature.implementationName
                                            , arguments = arguments.syntax
                                            , expression = expressionResult.syntax
                                            }
                                    }
                                )
                        }
            )
            nameLowercaseNodeUnderscoreSuffixingKeywords
            whitespaceAndComments
            (ParserFast.map4OrSucceed
                (\commentsBeforeTypeAnnotation typeAnnotationResult implementationName afterImplementationName ->
                    Just
                        { comments =
                            commentsBeforeTypeAnnotation
                                |> ropePrependTo typeAnnotationResult.comments
                                |> ropePrependTo implementationName.comments
                                |> ropePrependTo afterImplementationName
                        , implementationName = implementationName.syntax
                        , typeAnnotation = typeAnnotationResult.syntax
                        }
                )
                (ParserFast.symbolFollowedBy ":" whitespaceAndComments)
                type_
                (whitespaceAndCommentsEndsTopIndentedFollowedBy
                    nameLowercaseNodeUnderscoreSuffixingKeywords
                )
                whitespaceAndComments
                Nothing
            )
            parameterPatternsEquals
            whitespaceAndComments
            expressionFollowedByWhitespaceAndComments
            |> ParserFast.validate
                (\result ->
                    let
                        (GrenSyntax.Node _ letDeclaration) =
                            result.syntax
                    in
                    case letDeclaration of
                        GrenSyntax.LetDestructuring _ _ ->
                            True

                        GrenSyntax.LetFunction letFunctionDeclaration ->
                            case letFunctionDeclaration.signature of
                                Nothing ->
                                    True

                                Just (GrenSyntax.Node _ signature) ->
                                    let
                                        (GrenSyntax.Node _ implementationName) =
                                            implementation.name

                                        (GrenSyntax.Node _ implementation) =
                                            letFunctionDeclaration.declaration

                                        (GrenSyntax.Node _ signatureName) =
                                            signature.name
                                    in
                                    implementationName == signatureName
                )
        )
        (ParserFast.map8WithStartLocation
            (\start commentsBeforeTypeAnnotation typeAnnotationResult commentsBetweenTypeAndName nameNode afterImplementationName arguments commentsAfterEqual result ->
                { comments =
                    commentsBeforeTypeAnnotation
                        |> ropePrependTo typeAnnotationResult.comments
                        |> ropePrependTo commentsBetweenTypeAndName
                        |> ropePrependTo afterImplementationName
                        |> ropePrependTo arguments.comments
                        |> ropePrependTo commentsAfterEqual
                        |> ropePrependTo result.comments
                , syntax =
                    GrenSyntax.Node
                        { start = start
                        , end = result.syntax |> GrenSyntax.nodeRange |> .end
                        }
                        (GrenSyntax.LetFunction
                            { documentation = Nothing
                            , signature =
                                Just
                                    (GrenSyntax.Node
                                        { start = start
                                        , end = typeAnnotationResult.syntax |> GrenSyntax.nodeRange |> .end
                                        }
                                        { name =
                                            -- dummy
                                            GrenSyntax.Node
                                                { start = start, end = start }
                                                (nameNode |> GrenSyntax.nodeValue)
                                        , typeAnnotation = typeAnnotationResult.syntax
                                        }
                                    )
                            , declaration =
                                GrenSyntax.Node
                                    { start = nameNode |> GrenSyntax.nodeRange |> .start
                                    , end = result.syntax |> GrenSyntax.nodeRange |> .end
                                    }
                                    { name = nameNode
                                    , arguments = arguments.syntax
                                    , expression = result.syntax
                                    }
                            }
                        )
                }
            )
            (ParserFast.symbolFollowedBy ":" whitespaceAndComments)
            type_
            whitespaceAndCommentsEndsTopIndented
            nameLowercaseNodeUnderscoreSuffixingKeywords
            whitespaceAndComments
            parameterPatternsEquals
            whitespaceAndComments
            expressionFollowedByWhitespaceAndComments
        )


expressionNumber : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
expressionNumber =
    ParserFast.floatOrIntegerDecimalOrHexadecimalMapWithRange
        (\range n ->
            { comments = ropeEmpty
            , syntax = GrenSyntax.Node range (GrenSyntax.ExpressionFloat n)
            }
        )
        (\range n ->
            { comments = ropeEmpty
            , syntax = GrenSyntax.Node range (GrenSyntax.ExpressionInteger n)
            }
        )
        (\range n ->
            { comments = ropeEmpty
            , syntax = GrenSyntax.Node range (GrenSyntax.ExpressionHex n)
            }
        )


expressionIfThenElseFollowedByOptimisticLayout : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
expressionIfThenElseFollowedByOptimisticLayout =
    ParserFast.map6WithStartLocation
        (\start commentsAfterIf condition commentsAfterThen ifTrue commentsAfterElse ifFalse ->
            let
                (GrenSyntax.Node ifFalseRange _) =
                    ifFalse.syntax
            in
            { comments =
                commentsAfterIf
                    |> ropePrependTo condition.comments
                    |> ropePrependTo commentsAfterThen
                    |> ropePrependTo ifTrue.comments
                    |> ropePrependTo commentsAfterElse
                    |> ropePrependTo ifFalse.comments
            , syntax =
                GrenSyntax.Node
                    { start = start
                    , end = ifFalseRange.end
                    }
                    (GrenSyntax.ExpressionIfThenElse
                        condition.syntax
                        ifTrue.syntax
                        ifFalse.syntax
                    )
            }
        )
        (ParserFast.keywordFollowedBy "if" whitespaceAndComments)
        expressionFollowedByWhitespaceAndComments
        (ParserFast.oneOf2
            (ParserFast.keywordFollowedBy "then" whitespaceAndComments)
            (ParserFast.keywordFollowedBy "->" whitespaceAndComments)
        )
        expressionFollowedByWhitespaceAndComments
        (ParserFast.keywordFollowedBy "else" whitespaceAndComments)
        expressionFollowedByWhitespaceAndComments


expressionNegation : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
expressionNegation =
    ParserFast.symbolBacktrackableFollowedBy "-"
        (ParserFast.offsetSourceAndThen
            (\offset source ->
                case String.slice (offset - 2) (offset - 1) source of
                    " " ->
                        negationAfterMinus

                    -- not "\n" or "\r" since expressions are always indented
                    "(" ->
                        negationAfterMinus

                    ")" ->
                        negationAfterMinus

                    -- from the end of a multiline comment
                    "}" ->
                        negationAfterMinus

                    -- from lambda arrow
                    ">" ->
                        negationAfterMinus

                    -- from field or assignment
                    "=" ->
                        negationAfterMinus

                    -- from list or tuple or triple
                    "," ->
                        negationAfterMinus

                    -- from let...in
                    "n" ->
                        if
                            (String.slice (offset - 3) (offset - 2) source == "i")
                                && Basics.not
                                    (String.all Char.Extra.isLatinAlphaNumOrUnderscoreFast
                                        (String.slice (offset - 4) (offset - 3) source)
                                    )
                        then
                            negationAfterMinus

                        else
                            ParserFast.problem

                    _ ->
                        ParserFast.problem
            )
        )


negationAfterMinus : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
negationAfterMinus =
    ParserFast.map
        (\subExpressionResult ->
            let
                (GrenSyntax.Node subExpressionRange _) =
                    subExpressionResult.syntax
            in
            { comments = subExpressionResult.comments
            , syntax =
                GrenSyntax.Node
                    { start =
                        { row = subExpressionRange.start.row
                        , column = subExpressionRange.start.column - 1
                        }
                    , end = subExpressionRange.end
                    }
                    (GrenSyntax.ExpressionNegation subExpressionResult.syntax)
            }
        )
        subExpression


expressionQualifiedOrVariantOrRecordConstructorReferenceFollowedByRecordAccess : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
expressionQualifiedOrVariantOrRecordConstructorReferenceFollowedByRecordAccess =
    ParserFast.map2WithRange
        (\range firstName after ->
            { comments = ropeEmpty
            , syntax =
                GrenSyntax.Node range
                    (case after of
                        Nothing ->
                            GrenSyntax.ExpressionReference [] firstName

                        Just ( qualificationAfter, unqualified ) ->
                            GrenSyntax.ExpressionReference (firstName :: qualificationAfter) unqualified
                    )
            }
        )
        nameUppercase
        maybeDotReferenceExpressionTuple
        |> followedByMultiRecordAccess


maybeDotReferenceExpressionTuple : Parser (Maybe ( List String, String ))
maybeDotReferenceExpressionTuple =
    ParserFast.orSucceed
        (ParserFast.symbolFollowedBy "."
            (ParserFast.oneOf2Map
                Just
                (ParserFast.map2
                    (\firstName after ->
                        case after of
                            Nothing ->
                                ( [], firstName )

                            Just ( qualificationAfter, unqualified ) ->
                                ( firstName :: qualificationAfter, unqualified )
                    )
                    nameUppercase
                    (ParserFast.lazy (\() -> maybeDotReferenceExpressionTuple))
                )
                (\name -> Just ( [], name ))
                nameLowercaseUnderscoreSuffixingKeywords
            )
        )
        Nothing


expressionUnqualifiedFunctionReferenceFollowedByRecordAccess : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
expressionUnqualifiedFunctionReferenceFollowedByRecordAccess =
    nameLowercaseMapWithRange
        (\range unqualified ->
            { comments = ropeEmpty
            , syntax =
                GrenSyntax.Node range (GrenSyntax.ExpressionReference [] unqualified)
            }
        )
        |> followedByMultiRecordAccess


expressionRecordAccessFunction : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
expressionRecordAccessFunction =
    ParserFast.symbolFollowedBy "."
        (nameLowercaseMapWithRange
            (\range field ->
                { comments = ropeEmpty
                , syntax =
                    GrenSyntax.Node (range |> rangeMoveStartLeftByOneColumn)
                        (GrenSyntax.ExpressionRecordAccessFunction ("." ++ field))
                }
            )
        )


rangeMoveStartLeftByOneColumn : GrenSyntax.Range -> GrenSyntax.Range
rangeMoveStartLeftByOneColumn range =
    { start = { row = range.start.row, column = range.start.column - 1 }
    , end = range.end
    }


expressionStartingWithParensOpeningIfNecessaryFollowedByRecordAccess : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
expressionStartingWithParensOpeningIfNecessaryFollowedByRecordAccess =
    ParserFast.symbolFollowedBy "("
        (ParserFast.oneOf3
            (ParserFast.symbolWithEndLocation ")"
                (\end ->
                    { comments = ropeEmpty
                    , syntax =
                        GrenSyntax.Node { start = { row = end.row, column = end.column - 2 }, end = end }
                            GrenSyntax.ExpressionUnit
                    }
                )
            )
            allowedPrefixOperatorFollowedByClosingParensOneOf
            expressionParenthesizedOrTupleOrTripleAfterOpeningParens
        )


allowedPrefixOperatorFollowedByClosingParensOneOf : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
allowedPrefixOperatorFollowedByClosingParensOneOf =
    ParserFast.whileAtMost3WithoutLinebreakAnd2PartUtf16ValidateMapWithRangeBacktrackableFollowedBySymbol
        (\operatorRange operator ->
            { comments = ropeEmpty
            , syntax =
                GrenSyntax.Node
                    { start = { row = operatorRange.start.row, column = operatorRange.start.column - 1 }
                    , end = { row = operatorRange.end.row, column = operatorRange.end.column + 1 }
                    }
                    (GrenSyntax.ExpressionOperatorFunction operator)
            }
        )
        isOperatorSymbolCharAsString
        isAllowedOperatorToken
        ")"


expressionParenthesizedOrTupleOrTripleAfterOpeningParens : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
expressionParenthesizedOrTupleOrTripleAfterOpeningParens =
    ParserFast.map3WithRange
        (\rangeAfterOpeningParens commentsBeforeFirstPart firstPart tailParts ->
            { comments =
                commentsBeforeFirstPart
                    |> ropePrependTo firstPart.comments
                    |> ropePrependTo tailParts.comments
            , syntax =
                case tailParts.syntax of
                    TupledParenthesized () () ->
                        GrenSyntax.Node
                            { start = { row = rangeAfterOpeningParens.start.row, column = rangeAfterOpeningParens.start.column - 1 }
                            , end = rangeAfterOpeningParens.end
                            }
                            (GrenSyntax.ExpressionParenthesized firstPart.syntax)

                    TupledTwoOrThree secondPart maybeThirdPart ->
                        GrenSyntax.Node
                            { start = { row = rangeAfterOpeningParens.start.row, column = rangeAfterOpeningParens.start.column - 1 }
                            , end = rangeAfterOpeningParens.end
                            }
                            (case maybeThirdPart of
                                Nothing ->
                                    GrenSyntax.ExpressionTupled [ firstPart.syntax, secondPart ]

                                Just thirdPart ->
                                    GrenSyntax.ExpressionTupled [ firstPart.syntax, secondPart, thirdPart ]
                            )
            }
        )
        whitespaceAndComments
        expressionFollowedByWhitespaceAndComments
        (ParserFast.oneOf2
            (ParserFast.symbol ")"
                { comments = ropeEmpty, syntax = TupledParenthesized () () }
            )
            (ParserFast.symbolFollowedBy ","
                (ParserFast.map3
                    (\commentsBefore partResult maybeThirdPart ->
                        { comments =
                            commentsBefore
                                |> ropePrependTo partResult.comments
                                |> ropePrependTo maybeThirdPart.comments
                        , syntax = TupledTwoOrThree partResult.syntax maybeThirdPart.syntax
                        }
                    )
                    whitespaceAndComments
                    expressionFollowedByWhitespaceAndComments
                    (ParserFast.oneOf2
                        (ParserFast.symbol ")" { comments = ropeEmpty, syntax = Nothing })
                        (ParserFast.symbolFollowedBy ","
                            (ParserFast.map2
                                (\commentsBefore partResult ->
                                    { comments =
                                        commentsBefore
                                            |> ropePrependTo partResult.comments
                                    , syntax = Just partResult.syntax
                                    }
                                )
                                whitespaceAndComments
                                expressionFollowedByWhitespaceAndComments
                                |> ParserFast.followedBySymbol ")"
                            )
                        )
                    )
                )
            )
        )
        |> followedByMultiRecordAccess


type Tupled
    = TupledParenthesized () ()
    | TupledTwoOrThree (GrenSyntax.Node GrenSyntax.Expression) (Maybe (GrenSyntax.Node GrenSyntax.Expression))



---


extendedSubExpressionFollowedByWhitespaceAndComments :
    { afterCommitting : InfixOperatorInfo -> Parser (WithComments ExtensionRight)
    , validateRightPrecedence : InfixOperatorInfo -> Maybe InfixOperatorInfo
    }
    -> Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
extendedSubExpressionFollowedByWhitespaceAndComments info =
    ParserFast.loopWhileSucceedsOntoResultFromParser
        (infixOperatorAndThen info)
        subExpressionMaybeAppliedFollowedByWhitespaceAndComments
        (\extensionRightResult leftNodeWithComments ->
            { comments =
                leftNodeWithComments.comments
                    |> ropePrependTo extensionRightResult.comments
            , syntax =
                leftNodeWithComments.syntax
                    |> applyExtensionRight extensionRightResult.syntax
            }
        )
        Basics.identity


extensionRightParser :
    { afterCommitting : InfixOperatorInfo -> Parser (WithComments ExtensionRight)
    , direction : GrenSyntax.InfixDirection
    , symbol : String
    , validateRightPrecedence : InfixOperatorInfo -> Maybe InfixOperatorInfo
    }
    -> Parser (WithComments ExtensionRight)
extensionRightParser extensionRightInfo =
    ParserFast.map2
        (\commentsBefore right ->
            { comments = commentsBefore |> ropePrependTo right.comments
            , syntax =
                ExtendRightByOperation
                    { symbol = extensionRightInfo.symbol
                    , direction = extensionRightInfo.direction
                    , expression = right.syntax
                    }
            }
        )
        whitespaceAndComments
        (ParserFast.lazy
            (\() ->
                extendedSubExpressionFollowedByWhitespaceAndComments
                    { afterCommitting = extensionRightInfo.afterCommitting
                    , validateRightPrecedence = extensionRightInfo.validateRightPrecedence
                    }
            )
        )


infixOperatorAndThen :
    { afterCommitting : InfixOperatorInfo -> Parser (WithComments ExtensionRight)
    , validateRightPrecedence : InfixOperatorInfo -> Maybe InfixOperatorInfo
    }
    -> Parser (WithComments ExtensionRight)
infixOperatorAndThen extensionRightConstraints =
    let
        toResult : InfixOperatorInfo -> Maybe InfixOperatorInfo
        toResult =
            extensionRightConstraints.validateRightPrecedence

        apRResult : Maybe InfixOperatorInfo
        apRResult =
            toResult precedence1ApR

        appendResult : Maybe InfixOperatorInfo
        appendResult =
            toResult precedence5append

        apLResult : Maybe InfixOperatorInfo
        apLResult =
            toResult precedence1ApL

        composeRResult : Maybe InfixOperatorInfo
        composeRResult =
            toResult precedence9ComposeR

        eqResult : Maybe InfixOperatorInfo
        eqResult =
            toResult precedence4Eq

        mulResult : Maybe InfixOperatorInfo
        mulResult =
            toResult precedence7Mul

        consResult : Maybe InfixOperatorInfo
        consResult =
            toResult precedence5Cons

        addResult : Maybe InfixOperatorInfo
        addResult =
            toResult precedence6Add

        subResult : Maybe InfixOperatorInfo
        subResult =
            toResult precedence6Sub

        ignoreResult : Maybe InfixOperatorInfo
        ignoreResult =
            toResult precedence6Ignore

        andResult : Maybe InfixOperatorInfo
        andResult =
            toResult precedence3And

        keepResult : Maybe InfixOperatorInfo
        keepResult =
            toResult precedence5Keep

        composeLResult : Maybe InfixOperatorInfo
        composeLResult =
            toResult precedence9ComposeL

        neqResult : Maybe InfixOperatorInfo
        neqResult =
            toResult precedence4Neq

        idivResult : Maybe InfixOperatorInfo
        idivResult =
            toResult precedence7Idiv

        fdivResult : Maybe InfixOperatorInfo
        fdivResult =
            toResult precedence7Fdiv

        slashResult : Maybe InfixOperatorInfo
        slashResult =
            toResult precedence7Slash

        orResult : Maybe InfixOperatorInfo
        orResult =
            toResult precedence2Or

        leResult : Maybe InfixOperatorInfo
        leResult =
            toResult precedence4Le

        geResult : Maybe InfixOperatorInfo
        geResult =
            toResult precedence4Ge

        gtResult : Maybe InfixOperatorInfo
        gtResult =
            toResult precedence4Gt

        questionMarkResult : Maybe InfixOperatorInfo
        questionMarkResult =
            toResult precedence8QuestionMark

        ltResult : Maybe InfixOperatorInfo
        ltResult =
            toResult precedence4Lt

        powResult : Maybe InfixOperatorInfo
        powResult =
            toResult precedence8Pow
    in
    ParserFast.whileAtMost3WithoutLinebreakAnd2PartUtf16ToResultAndThen
        isOperatorSymbolCharAsString
        (\operator ->
            case operator of
                "|>" ->
                    apRResult

                "|" ->
                    apRResult

                "++" ->
                    appendResult

                "<|" ->
                    apLResult

                ">>" ->
                    composeRResult

                "==" ->
                    eqResult

                "===" ->
                    eqResult

                "*" ->
                    mulResult

                "::" ->
                    consResult

                "+" ->
                    addResult

                "-" ->
                    subResult

                "|." ->
                    ignoreResult

                "&&" ->
                    andResult

                "|=" ->
                    keepResult

                "<<" ->
                    composeLResult

                "/=" ->
                    neqResult

                "!=" ->
                    neqResult

                "!==" ->
                    neqResult

                "//" ->
                    idivResult

                "/" ->
                    fdivResult

                "</>" ->
                    slashResult

                "||" ->
                    orResult

                "<=" ->
                    leResult

                ">=" ->
                    geResult

                ">" ->
                    gtResult

                "<?>" ->
                    questionMarkResult

                "<" ->
                    ltResult

                "^" ->
                    powResult

                "**" ->
                    powResult

                _ ->
                    Nothing
        )
        extensionRightConstraints.afterCommitting


subExpressionMaybeAppliedFollowedByWhitespaceAndComments : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
subExpressionMaybeAppliedFollowedByWhitespaceAndComments =
    -- functionally, a simple oneOf would be correct as well.
    -- However, since this parser is called _a lot_,
    --   we squeeze out a bit more speed by de-duplicating slices etc
    ParserFast.offsetSourceAndThen
        (\offset source ->
            case String.slice offset (offset + 1) source of
                "\"" ->
                    literalExpressionOptimisticLayout

                "(" ->
                    tupledExpressionIfNecessaryFollowedByRecordAccessMaybeApplied

                "[" ->
                    listOrGlslExpressionOptimisticLayout

                "{" ->
                    recordExpressionFollowedByRecordAccessMaybeApplied

                "c" ->
                    caseOrUnqualifiedReferenceExpressionMaybeApplied

                "\\" ->
                    expressionLambdaFollowedByWhitespaceAndComments

                "l" ->
                    letOrUnqualifiedReferenceExpressionMaybeApplied

                "i" ->
                    ifOrUnqualifiedReferenceExpressionMaybeApplied

                "." ->
                    recordAccessFunctionExpressionMaybeApplied

                "-" ->
                    negationOperationOptimisticLayout

                "'" ->
                    charLiteralExpressionOptimisticLayout

                _ ->
                    referenceOrNumberExpressionMaybeApplied
        )


negationOperationOptimisticLayout : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
negationOperationOptimisticLayout =
    expressionNegation |> followedByOptimisticLayout


charLiteralExpressionOptimisticLayout : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
charLiteralExpressionOptimisticLayout =
    expressionChar |> followedByOptimisticLayout


literalExpressionOptimisticLayout : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
literalExpressionOptimisticLayout =
    expressionString |> followedByOptimisticLayout


listOrGlslExpressionOptimisticLayout : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
listOrGlslExpressionOptimisticLayout =
    expressionListOrGlsl |> followedByOptimisticLayout


followedByOptimisticLayout : Parser (WithComments a) -> Parser (WithComments a)
followedByOptimisticLayout parser =
    ParserFast.map2
        (\result commentsAfter ->
            { comments = result.comments |> ropePrependTo commentsAfter
            , syntax = result.syntax
            }
        )
        parser
        whitespaceAndComments


recordAccessFunctionExpressionMaybeApplied : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
recordAccessFunctionExpressionMaybeApplied =
    expressionRecordAccessFunction |> followedByMultiArgumentApplication


recordExpressionFollowedByRecordAccessMaybeApplied : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
recordExpressionFollowedByRecordAccessMaybeApplied =
    -- TODO don't check for applied if record access
    expressionRecordFollowedByRecordAccess
        |> followedByMultiArgumentApplication


tupledExpressionIfNecessaryFollowedByRecordAccessMaybeApplied : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
tupledExpressionIfNecessaryFollowedByRecordAccessMaybeApplied =
    -- TODO don't check for applied if not parenthesized
    expressionStartingWithParensOpeningIfNecessaryFollowedByRecordAccess
        |> followedByMultiArgumentApplication


caseOrUnqualifiedReferenceExpressionMaybeApplied : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
caseOrUnqualifiedReferenceExpressionMaybeApplied =
    ParserFast.oneOf2
        expressionCaseOfFollowedByOptimisticLayout
        (expressionUnqualifiedFunctionReferenceFollowedByRecordAccess
            |> followedByMultiArgumentApplication
        )


letOrUnqualifiedReferenceExpressionMaybeApplied : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
letOrUnqualifiedReferenceExpressionMaybeApplied =
    ParserFast.oneOf2
        letExpressionFollowedByOptimisticLayout
        (expressionUnqualifiedFunctionReferenceFollowedByRecordAccess
            |> followedByMultiArgumentApplication
        )


ifOrUnqualifiedReferenceExpressionMaybeApplied : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
ifOrUnqualifiedReferenceExpressionMaybeApplied =
    ParserFast.oneOf2
        expressionIfThenElseFollowedByOptimisticLayout
        (expressionUnqualifiedFunctionReferenceFollowedByRecordAccess
            |> followedByMultiArgumentApplication
        )


referenceOrNumberExpressionMaybeApplied : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
referenceOrNumberExpressionMaybeApplied =
    ParserFast.oneOf3
        (expressionQualifiedOrVariantOrRecordConstructorReferenceFollowedByRecordAccess
            |> followedByMultiArgumentApplication
        )
        (expressionUnqualifiedFunctionReferenceFollowedByRecordAccess
            |> followedByMultiArgumentApplication
        )
        (expressionNumber |> followedByOptimisticLayout)


followedByMultiArgumentApplication : Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression)) -> Parser (WithComments (GrenSyntax.Node GrenSyntax.Expression))
followedByMultiArgumentApplication appliedExpressionParser =
    ParserFast.map3
        (\leftExpressionResult commentsBeforeExtension maybeArgsReverse ->
            { comments =
                leftExpressionResult.comments
                    |> ropePrependTo commentsBeforeExtension
                    |> ropePrependTo maybeArgsReverse.comments
            , syntax =
                case maybeArgsReverse.syntax of
                    [] ->
                        leftExpressionResult.syntax

                    (GrenSyntax.Node lastArgRange _) :: _ ->
                        let
                            (GrenSyntax.Node leftRange _) =
                                leftExpressionResult.syntax
                        in
                        GrenSyntax.Node { start = leftRange.start, end = lastArgRange.end }
                            (GrenSyntax.ExpressionCall
                                (leftExpressionResult.syntax :: List.reverse maybeArgsReverse.syntax)
                            )
            }
        )
        appliedExpressionParser
        whitespaceAndComments
        (manyWithCommentsReverse
            (positivelyIndentedFollowedBy
                (ParserFast.map2
                    (\arg commentsAfter ->
                        { comments = arg.comments |> ropePrependTo commentsAfter
                        , syntax = arg.syntax
                        }
                    )
                    subExpression
                    whitespaceAndComments
                )
            )
        )


applyExtensionRight : ExtensionRight -> GrenSyntax.Node GrenSyntax.Expression -> GrenSyntax.Node GrenSyntax.Expression
applyExtensionRight (ExtendRightByOperation operation) leftNode =
    let
        (GrenSyntax.Node leftRange _) =
            leftNode

        (GrenSyntax.Node rightExpressionRange _) =
            operation.expression
    in
    GrenSyntax.Node { start = leftRange.start, end = rightExpressionRange.end }
        (GrenSyntax.ExpressionInfixOperation operation.symbol
            leftNode
            operation.expression
        )


type alias InfixOperatorInfo =
    { leftPrecedence : Int
    , symbol : String
    , extensionRightParser : Parser (WithComments ExtensionRight)
    }


infixLeft : Int -> String -> InfixOperatorInfo
infixLeft leftPrecedence symbol =
    { leftPrecedence = leftPrecedence
    , symbol = symbol
    , extensionRightParser =
        extensionRightParser
            { afterCommitting = .extensionRightParser
            , direction = GrenSyntax.Left
            , symbol = symbol
            , validateRightPrecedence =
                \rightInfo ->
                    if rightInfo.leftPrecedence > leftPrecedence then
                        Just rightInfo

                    else
                        Nothing
            }
    }


infixRight : Int -> String -> InfixOperatorInfo
infixRight leftPrecedence symbol =
    { leftPrecedence = leftPrecedence
    , symbol = symbol
    , extensionRightParser =
        extensionRightParser
            { afterCommitting = .extensionRightParser
            , direction = GrenSyntax.Right
            , symbol = symbol
            , validateRightPrecedence =
                \rightInfo ->
                    if rightInfo.leftPrecedence >= leftPrecedence then
                        Just rightInfo

                    else
                        Nothing
            }
    }


infixNonAssociative : Int -> String -> InfixOperatorInfo
infixNonAssociative leftPrecedence symbol =
    { leftPrecedence = leftPrecedence
    , symbol = symbol
    , extensionRightParser =
        extensionRightParser
            { afterCommitting =
                \rightInfo ->
                    if rightInfo.leftPrecedence == leftPrecedence then
                        ParserFast.problem

                    else
                        rightInfo.extensionRightParser
            , direction = GrenSyntax.Non
            , symbol = symbol
            , validateRightPrecedence =
                \rightInfo ->
                    if rightInfo.leftPrecedence >= leftPrecedence then
                        Just rightInfo

                    else
                        Nothing
            }
    }


type ExtensionRight
    = ExtendRightByOperation
        { symbol : String
        , direction : GrenSyntax.InfixDirection
        , expression : GrenSyntax.Node GrenSyntax.Expression
        }


{-| [`Parser`](#Parser) for an [`GrenSyntax.Pattern`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Pattern#Pattern)
-}
pattern : Parser { comments : Comments, syntax : GrenSyntax.Node GrenSyntax.Pattern }
pattern =
    ParserFast.map2
        (\leftMaybeConsed maybeAsExtension ->
            case maybeAsExtension of
                Nothing ->
                    leftMaybeConsed

                Just asExtension ->
                    { comments =
                        leftMaybeConsed.comments
                            |> ropePrependTo asExtension.comments
                    , syntax =
                        GrenSyntax.nodeCombine GrenSyntax.PatternAs
                            leftMaybeConsed.syntax
                            asExtension.syntax
                    }
        )
        (ParserFast.loopWhileSucceedsOntoResultFromParserRightToLeftStackUnsafe
            (ParserFast.map2
                (\startPatternResult commentsAfter ->
                    { comments = startPatternResult.comments |> ropePrependTo commentsAfter
                    , syntax = startPatternResult.syntax
                    }
                )
                (ParserFast.lazy (\() -> composablePattern))
                whitespaceAndComments
            )
            (ParserFast.symbolFollowedBy "::"
                (ParserFast.map3
                    (\commentsAfterCons patternResult commentsAfterTailSubPattern ->
                        { comments =
                            commentsAfterCons
                                |> ropePrependTo patternResult.comments
                                |> ropePrependTo commentsAfterTailSubPattern
                        , syntax = patternResult.syntax
                        }
                    )
                    whitespaceAndComments
                    (ParserFast.lazy (\() -> composablePattern))
                    whitespaceAndComments
                )
            )
            (\consed afterCons ->
                { comments = consed.comments |> ropePrependTo afterCons.comments
                , syntax =
                    GrenSyntax.nodeCombine GrenSyntax.PatternListCons
                        consed.syntax
                        afterCons.syntax
                }
            )
        )
        (ParserFast.orSucceed
            (ParserFast.keywordFollowedBy "as"
                (ParserFast.map2
                    (\commentsAfterAs name ->
                        Just
                            { comments = commentsAfterAs
                            , syntax = name
                            }
                    )
                    whitespaceAndComments
                    nameLowercaseNodeUnderscoreSuffixingKeywords
                )
            )
            Nothing
        )


patternParenthesizedOrOldUnit : Parser (WithComments (GrenSyntax.Node GrenSyntax.Pattern))
patternParenthesizedOrOldUnit =
    ParserFast.symbolFollowedBy "("
        (ParserFast.map2WithRange
            (\range commentsBeforeHead contentResult ->
                { comments =
                    commentsBeforeHead
                        |> ropePrependTo contentResult.comments
                , syntax =
                    GrenSyntax.Node { start = { row = range.start.row, column = range.start.column - 1 }, end = range.end }
                        contentResult.syntax
                }
            )
            whitespaceAndComments
            -- yes, (  ) is a valid pattern in the old elm syntax but not a valid type or expression
            (ParserFast.oneOf2
                (ParserFast.symbol ")" { comments = ropeEmpty, syntax = GrenSyntax.PatternUnit })
                (ParserFast.map2
                    (\headResult commentsAfterHead ->
                        { comments =
                            headResult.comments
                                |> ropePrependTo commentsAfterHead
                        , syntax =
                            GrenSyntax.PatternParenthesized headResult.syntax
                        }
                    )
                    pattern
                    (whitespaceAndComments
                        |> ParserFast.followedBySymbol ")"
                    )
                )
            )
        )


varPattern : Parser (WithComments (GrenSyntax.Node GrenSyntax.Pattern))
varPattern =
    nameLowercaseMapWithRange
        (\range var ->
            { comments = ropeEmpty
            , syntax = GrenSyntax.Node range (GrenSyntax.PatternVariable var)
            }
        )


numberPart : Parser (WithComments (GrenSyntax.Node GrenSyntax.Pattern))
numberPart =
    ParserFast.integerDecimalOrHexadecimalMapWithRange
        (\range n -> { comments = ropeEmpty, syntax = GrenSyntax.Node range (GrenSyntax.PatternInt n) })
        (\range n -> { comments = ropeEmpty, syntax = GrenSyntax.Node range (GrenSyntax.PatternHex n) })


charPattern : Parser (WithComments (GrenSyntax.Node GrenSyntax.Pattern))
charPattern =
    characterLiteralMapWithRange
        (\range char ->
            { comments = ropeEmpty, syntax = GrenSyntax.Node range (GrenSyntax.PatternChar char) }
        )


listPattern : Parser (WithComments (GrenSyntax.Node GrenSyntax.Pattern))
listPattern =
    ParserFast.map2WithRange
        (\range commentsBeforeElements maybeElements ->
            case maybeElements of
                Nothing ->
                    { comments = commentsBeforeElements
                    , syntax = GrenSyntax.Node range patternListEmpty
                    }

                Just elements ->
                    { comments = commentsBeforeElements |> ropePrependTo elements.comments
                    , syntax = GrenSyntax.Node range (GrenSyntax.PatternListExact elements.syntax)
                    }
        )
        (ParserFast.symbolFollowedBy "[" whitespaceAndComments)
        (ParserFast.oneOf2
            (ParserFast.symbol "]" Nothing)
            (ParserFast.map4
                (\commentsBeforeHead head commentsAfterHead tail ->
                    Just
                        { comments =
                            commentsBeforeHead
                                |> ropePrependTo head.comments
                                |> ropePrependTo tail.comments
                                |> ropePrependTo commentsAfterHead
                        , syntax = head.syntax :: tail.syntax
                        }
                )
                (ParserFast.orSucceed
                    (ParserFast.symbolFollowedBy "," whitespaceAndComments)
                    ropeEmpty
                )
                pattern
                whitespaceAndComments
                (manyWithComments
                    (ParserFast.symbolFollowedBy ","
                        (ParserFast.map4
                            (\commentsBefore commentsWithExtraComma v commentsAfter ->
                                { comments =
                                    commentsBefore
                                        |> ropePrependTo commentsWithExtraComma
                                        |> ropePrependTo v.comments
                                        |> ropePrependTo commentsAfter
                                , syntax = v.syntax
                                }
                            )
                            whitespaceAndComments
                            (ParserFast.orSucceed
                                (ParserFast.symbolFollowedBy "," whitespaceAndComments)
                                ropeEmpty
                            )
                            pattern
                            whitespaceAndComments
                        )
                    )
                )
                |> ParserFast.followedBySymbol "]"
            )
        )


patternListEmpty : GrenSyntax.Pattern
patternListEmpty =
    GrenSyntax.PatternListExact []


composablePattern : Parser (WithComments (GrenSyntax.Node GrenSyntax.Pattern))
composablePattern =
    ParserFast.oneOf9
        varPattern
        qualifiedPatternWithConsumeArgs
        allPattern
        patternParenthesizedOrOldUnit
        recordPattern
        stringPattern
        listPattern
        numberPart
        charPattern


patternNotSpaceSeparated : Parser (WithComments (GrenSyntax.Node GrenSyntax.Pattern))
patternNotSpaceSeparated =
    ParserFast.oneOf9
        varPattern
        qualifiedPatternWithoutConsumeArgs
        allPattern
        patternParenthesizedOrOldUnit
        recordPattern
        stringPattern
        listPattern
        numberPart
        charPattern


allPattern : Parser (WithComments (GrenSyntax.Node GrenSyntax.Pattern))
allPattern =
    ParserFast.symbolWithRange "_"
        (\range ->
            { comments = ropeEmpty
            , syntax = GrenSyntax.Node range GrenSyntax.PatternIgnored
            }
        )


stringPattern : Parser (WithComments (GrenSyntax.Node GrenSyntax.Pattern))
stringPattern =
    singleOrTripleQuotedStringLiteralMapWithRange
        (\range string ->
            { comments = ropeEmpty
            , syntax = GrenSyntax.Node range (GrenSyntax.PatternString string)
            }
        )


qualifiedPatternWithConsumeArgs : Parser (WithComments (GrenSyntax.Node GrenSyntax.Pattern))
qualifiedPatternWithConsumeArgs =
    ParserFast.map3
        (\(GrenSyntax.Node nameRange name) afterStartName argsReverse ->
            let
                range : GrenSyntax.Range
                range =
                    case argsReverse.syntax of
                        [] ->
                            nameRange

                        (GrenSyntax.Node lastArgRange _) :: _ ->
                            { start = nameRange.start, end = lastArgRange.end }
            in
            { comments = afterStartName |> ropePrependTo argsReverse.comments
            , syntax =
                GrenSyntax.Node range
                    (GrenSyntax.PatternVariant
                        name
                        (List.reverse argsReverse.syntax)
                    )
            }
        )
        qualifiedNameRefNode
        whitespaceAndComments
        (manyWithCommentsReverse
            (ParserFast.map2
                (\arg commentsAfterArg ->
                    { comments = arg.comments |> ropePrependTo commentsAfterArg
                    , syntax = arg.syntax
                    }
                )
                patternNotSpaceSeparated
                whitespaceAndComments
            )
        )


qualifiedNameRefNode : Parser (GrenSyntax.Node GrenSyntax.QualifiedNameRef)
qualifiedNameRefNode =
    ParserFast.map2WithRange
        (\range firstName after ->
            GrenSyntax.Node range
                (case after of
                    Nothing ->
                        { moduleName = [], name = firstName }

                    Just ( qualificationAfter, unqualified ) ->
                        { moduleName = firstName :: qualificationAfter, name = unqualified }
                )
        )
        nameUppercase
        maybeDotNamesUppercaseTuple


qualifiedPatternWithoutConsumeArgs : Parser (WithComments (GrenSyntax.Node GrenSyntax.Pattern))
qualifiedPatternWithoutConsumeArgs =
    ParserFast.map2WithRange
        (\range firstName after ->
            { comments = ropeEmpty
            , syntax =
                GrenSyntax.Node range
                    (GrenSyntax.PatternVariant
                        (case after of
                            Nothing ->
                                { moduleName = [], name = firstName }

                            Just ( qualificationAfter, unqualified ) ->
                                { moduleName = firstName :: qualificationAfter, name = unqualified }
                        )
                        []
                    )
            }
        )
        nameUppercase
        maybeDotNamesUppercaseTuple


recordPattern : Parser (WithComments (GrenSyntax.Node GrenSyntax.Pattern))
recordPattern =
    ParserFast.map2WithRange
        (\range commentsBeforeElements elements ->
            { comments = commentsBeforeElements |> ropePrependTo elements.comments
            , syntax =
                GrenSyntax.Node range (GrenSyntax.PatternRecord elements.syntax)
            }
        )
        (ParserFast.symbolFollowedBy "{" whitespaceAndComments)
        (ParserFast.oneOf2
            (ParserFast.symbol "}" { comments = ropeEmpty, syntax = [] })
            (ParserFast.map4
                (\commentsBeforeHead head commentsAfterHead tail ->
                    { comments =
                        commentsBeforeHead
                            |> ropePrependTo commentsAfterHead
                            |> ropePrependTo tail.comments
                    , syntax = head :: tail.syntax
                    }
                )
                (ParserFast.orSucceed
                    (ParserFast.symbolFollowedBy "," whitespaceAndComments)
                    ropeEmpty
                )
                nameLowercaseNodeUnderscoreSuffixingKeywords
                whitespaceAndComments
                (manyWithComments
                    (ParserFast.symbolFollowedBy ","
                        (ParserFast.map4
                            (\commentsBeforeName commentsWithExtraComma name afterName ->
                                { comments =
                                    commentsBeforeName
                                        |> ropePrependTo commentsWithExtraComma
                                        |> ropePrependTo afterName
                                , syntax = name
                                }
                            )
                            whitespaceAndComments
                            (ParserFast.orSucceed
                                (ParserFast.symbolFollowedBy "," whitespaceAndComments)
                                ropeEmpty
                            )
                            nameLowercaseNodeUnderscoreSuffixingKeywords
                            whitespaceAndComments
                        )
                    )
                )
                |> ParserFast.followedBySymbol "}"
            )
        )


isNotReserved : String -> Bool
isNotReserved name =
    case name of
        "module" ->
            False

        "exposing" ->
            False

        "import" ->
            False

        "as" ->
            False

        "if" ->
            False

        "then" ->
            False

        "else" ->
            False

        "let" ->
            False

        "in" ->
            False

        "case" ->
            False

        "of" ->
            False

        "port" ->
            False

        --"infixr"
        --"infixl"
        "type" ->
            False

        -- "infix" Apparently this is not a reserved keyword
        -- "alias" Apparently this is not a reserved keyword
        "where" ->
            False

        _ ->
            True


ifKeywordUnderscoreSuffix : String -> String
ifKeywordUnderscoreSuffix name =
    case name of
        "module" ->
            "module_"

        "exposing" ->
            "exposing_"

        "import" ->
            "import_"

        "as" ->
            "as_"

        "if" ->
            "if_"

        "then" ->
            "then_"

        "else" ->
            "else_"

        "let" ->
            "let_"

        "in" ->
            "in_"

        "case" ->
            "case_"

        "of" ->
            "of_"

        "port" ->
            "port_"

        --"infixr"
        --"infixl"
        "type" ->
            "type_"

        -- "infix" Apparently this is not a reserved keyword
        -- "alias" Apparently this is not a reserved keyword
        "where" ->
            "where_"

        _ ->
            name


escapedCharValueMap : (Char -> res) -> Parser res
escapedCharValueMap charToRes =
    ParserFast.oneOf7
        (ParserFast.symbol "'" (charToRes '\''))
        (ParserFast.symbol "\"" (charToRes '"'))
        (ParserFast.symbol "n" (charToRes '\n'))
        (ParserFast.symbol "t" (charToRes '\t'))
        -- even though gren-format will change \r to a unicode version. When you don't use gren-format, this will not happen.
        (ParserFast.symbol "r" (charToRes '\u{000D}'))
        (ParserFast.symbol "\\" (charToRes '\\'))
        (ParserFast.symbolFollowedBy "u{"
            (ParserFast.ifFollowedByWhileMapWithoutLinebreak
                (\hex ->
                    charToRes (Char.fromCode (hexStringToInt hex))
                )
                Char.isHexDigit
                Char.isHexDigit
                |> ParserFast.followedBySymbol "}"
            )
        )


hexStringToInt : String -> Int
hexStringToInt string =
    String.foldr
        (\c soFar ->
            { exponent = soFar.exponent + 1
            , result = soFar.result + 16 ^ soFar.exponent * charToHex c
            }
        )
        { exponent = 0, result = 0 }
        string
        |> .result


charToHex : Char -> Int
charToHex c =
    case c of
        '0' ->
            0

        '1' ->
            1

        '2' ->
            2

        '3' ->
            3

        '4' ->
            4

        '5' ->
            5

        '6' ->
            6

        '7' ->
            7

        '8' ->
            8

        '9' ->
            9

        'a' ->
            10

        'b' ->
            11

        'c' ->
            12

        'd' ->
            13

        'e' ->
            14

        'f' ->
            15

        'A' ->
            10

        'B' ->
            11

        'C' ->
            12

        'D' ->
            13

        'E' ->
            14

        -- 'F'
        _ ->
            15


characterLiteralMapWithRange : (GrenSyntax.Range -> Char -> res) -> Parser res
characterLiteralMapWithRange rangeAndCharToRes =
    ParserFast.symbolFollowedBy "'"
        (ParserFast.oneOf2MapWithStartRowColumnAndEndRowColumn
            (\startRow startColumn char endRow endColumn ->
                rangeAndCharToRes
                    { start = { row = startRow, column = startColumn - 1 }
                    , end = { row = endRow, column = endColumn + 1 }
                    }
                    char
            )
            (ParserFast.symbolFollowedBy "\\" (escapedCharValueMap identity))
            (\startRow startColumn char endRow endColumn ->
                rangeAndCharToRes
                    { start = { row = startRow, column = startColumn - 1 }
                    , end = { row = endRow, column = endColumn + 1 }
                    }
                    char
            )
            ParserFast.anyChar
            |> ParserFast.followedBySymbol "'"
        )


singleOrTripleQuotedStringLiteralMapWithRange : (GrenSyntax.Range -> String -> res) -> Parser res
singleOrTripleQuotedStringLiteralMapWithRange rangeAndStringToRes =
    ParserFast.symbolFollowedBy "\""
        (ParserFast.oneOf2MapWithStartRowColumnAndEndRowColumn
            (\startRow startColumn string endRow endColumn ->
                rangeAndStringToRes
                    { start = { row = startRow, column = startColumn - 1 }
                    , end = { row = endRow, column = endColumn }
                    }
                    string
            )
            (ParserFast.symbolFollowedBy "\"\""
                tripleQuotedStringLiteralOfterTripleDoubleQuote
            )
            (\startRow startColumn string endRow endColumn ->
                rangeAndStringToRes
                    { start = { row = startRow, column = startColumn - 1 }
                    , end = { row = endRow, column = endColumn }
                    }
                    string
            )
            singleQuotedStringLiteralAfterDoubleQuote
        )


singleQuotedStringLiteralAfterDoubleQuote : Parser String
singleQuotedStringLiteralAfterDoubleQuote =
    ParserFast.loopUntil (ParserFast.symbol "\"" ())
        (ParserFast.oneOf2
            (ParserFast.whileAtLeast1WithoutLinebreak
                (\c ->
                    case c of
                        '"' ->
                            False

                        '\\' ->
                            False

                        _ ->
                            not (Char.Extra.isUtf16Surrogate c)
                )
            )
            (ParserFast.symbolFollowedBy "\\" (escapedCharValueMap String.fromChar))
        )
        ""
        (\extension soFar ->
            soFar ++ extension ++ ""
        )
        identity


tripleQuotedStringLiteralOfterTripleDoubleQuote : Parser String
tripleQuotedStringLiteralOfterTripleDoubleQuote =
    ParserFast.loopUntil (ParserFast.symbol "\"\"\"" ())
        (ParserFast.oneOf3
            (ParserFast.symbol "\"" "\"")
            (ParserFast.symbolFollowedBy "\\" (escapedCharValueMap String.fromChar))
            (ParserFast.while
                (\c ->
                    case c of
                        '"' ->
                            False

                        '\\' ->
                            False

                        _ ->
                            not (Char.Extra.isUtf16Surrogate c)
                )
            )
        )
        ""
        (\extension soFar ->
            soFar ++ extension ++ ""
        )
        identity


{-| [`Parser`](#Parser) for a name used for
record field names and unqualified function/value references
-}
nameLowercase : Parser String
nameLowercase =
    ParserFast.ifFollowedByWhileValidateWithoutLinebreak
        Char.Extra.unicodeIsLowerFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast
        isNotReserved


nameLowercaseUnderscoreSuffixingKeywords : Parser String
nameLowercaseUnderscoreSuffixingKeywords =
    ParserFast.ifFollowedByWhileMapWithoutLinebreak
        ifKeywordUnderscoreSuffix
        Char.Extra.unicodeIsLowerFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast


nameLowercaseNode : Parser (GrenSyntax.Node String)
nameLowercaseNode =
    ParserFast.ifFollowedByWhileValidateMapWithRangeWithoutLinebreak GrenSyntax.Node
        Char.Extra.unicodeIsLowerFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast
        isNotReserved


nameLowercaseNodeUnderscoreSuffixingKeywords : Parser (GrenSyntax.Node String)
nameLowercaseNodeUnderscoreSuffixingKeywords =
    ParserFast.ifFollowedByWhileMapWithRangeWithoutLinebreak
        (\range name ->
            GrenSyntax.Node range
                (name |> ifKeywordUnderscoreSuffix)
        )
        Char.Extra.unicodeIsLowerFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast


nameLowercaseMapWithRange : (GrenSyntax.Range -> String -> res) -> Parser res
nameLowercaseMapWithRange rangeAndNameToResult =
    ParserFast.ifFollowedByWhileValidateMapWithRangeWithoutLinebreak
        rangeAndNameToResult
        Char.Extra.unicodeIsLowerFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast
        isNotReserved


functionNameNotInfixNode : Parser (GrenSyntax.Node String)
functionNameNotInfixNode =
    ParserFast.ifFollowedByWhileValidateMapWithRangeWithoutLinebreak GrenSyntax.Node
        Char.Extra.unicodeIsLowerFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast
        (\name ->
            case name of
                "infix" ->
                    False

                nameNotInfix ->
                    nameNotInfix |> isNotReserved
        )


{-| [`Parser`](#Parser) for a name used for
type names, variant names, record type alias constructor function names and module names
-}
nameUppercase : Parser String
nameUppercase =
    ParserFast.ifFollowedByWhileWithoutLinebreak
        Char.Extra.unicodeIsUpperFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast


nameUppercaseMapWithRange : (GrenSyntax.Range -> String -> res) -> Parser res
nameUppercaseMapWithRange rangeAndNameToRes =
    ParserFast.ifFollowedByWhileMapWithRangeWithoutLinebreak rangeAndNameToRes
        Char.Extra.unicodeIsUpperFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast


nameUppercaseNode : Parser (GrenSyntax.Node String)
nameUppercaseNode =
    ParserFast.ifFollowedByWhileMapWithRangeWithoutLinebreak GrenSyntax.Node
        Char.Extra.unicodeIsUpperFast
        Char.Extra.unicodeIsAlphaNumOrUnderscoreFast


isAllowedOperatorToken : String -> Bool
isAllowedOperatorToken operatorCandidateToValidate =
    case operatorCandidateToValidate of
        "==" ->
            True

        "/=" ->
            True

        "::" ->
            True

        "++" ->
            True

        "+" ->
            True

        "*" ->
            True

        "<|" ->
            True

        "|>" ->
            True

        "||" ->
            True

        "<=" ->
            True

        ">=" ->
            True

        "|=" ->
            True

        "|." ->
            True

        "//" ->
            True

        "</>" ->
            True

        "<?>" ->
            True

        "^" ->
            True

        "<<" ->
            True

        ">>" ->
            True

        "<" ->
            True

        ">" ->
            True

        "/" ->
            True

        "&&" ->
            True

        "-" ->
            True

        _ ->
            False


isOperatorSymbolCharAsString : String -> Bool
isOperatorSymbolCharAsString c =
    case c of
        "|" ->
            True

        "+" ->
            True

        "<" ->
            True

        ">" ->
            True

        "=" ->
            True

        "*" ->
            True

        ":" ->
            True

        "-" ->
            True

        "/" ->
            True

        "&" ->
            True

        "." ->
            True

        "?" ->
            True

        "^" ->
            True

        -- only for != to /= conversion
        "!" ->
            True

        _ ->
            False


{-| [`Parser`](#Parser) for a `--...` comment
-}
singleLineComment : Parser (GrenSyntax.Node String)
singleLineComment =
    ParserFast.symbolFollowedBy "--"
        (ParserFast.whileMapWithRange
            (\c ->
                case c of
                    '\u{000D}' ->
                        False

                    '\n' ->
                        False

                    _ ->
                        not (Char.Extra.isUtf16Surrogate c)
            )
            (\range content ->
                GrenSyntax.Node
                    { start = { row = range.start.row, column = range.start.column - 2 }
                    , end =
                        { row = range.start.row
                        , column = range.end.column
                        }
                    }
                    ("--" ++ content)
            )
        )


{-| [`Parser`](#Parser) for a `{-...-}` comment,
also verifying that it itself isn't a documentation comment
-}
multiLineComment : Parser (GrenSyntax.Node String)
multiLineComment =
    ParserFast.offsetSourceAndThen
        (\offset source ->
            case String.slice (offset + 2) (offset + 3) source of
                "|" ->
                    ParserFast.problem

                _ ->
                    multiLineCommentNoCheck
        )


multiLineCommentNoCheck : Parser (GrenSyntax.Node String)
multiLineCommentNoCheck =
    ParserFast.nestableMultiCommentMapWithRange GrenSyntax.Node
        ( '{', "-" )
        ( '-', "}" )


{-| [`Parser`](#Parser) for the space between syntax tokens
which can contain spaces, linebreaks, [`multiLineComment`](#multiLineComment)s
and [`singleLineComment`](#singleLineComment)s
-}
whitespaceAndComments : Parser Comments
whitespaceAndComments =
    ParserFast.skipWhileWhitespaceBacktrackableFollowedBy
        -- whitespace can't be followed by more whitespace
        --
        -- since comments are comparatively rare
        -- but expensive to check for, we allow shortcutting
        (ParserFast.offsetSourceAndThenOrSucceed
            (\offset source ->
                case source |> String.slice offset (offset + 2) of
                    "--" ->
                        -- this will always succeed from here, so no need to fall back to empty
                        Just fromSingleLineCommentNode

                    "{-" ->
                        Just fromMultilineCommentNodeOrEmptyOnProblem

                    _ ->
                        Nothing
            )
            ropeEmpty
        )


fromMultilineCommentNodeOrEmptyOnProblem : Parser Comments
fromMultilineCommentNodeOrEmptyOnProblem =
    ParserFast.map2OrSucceed
        (\comment commentsAfter ->
            ropeOne comment |> ropeFilledPrependTo commentsAfter
        )
        (multiLineComment
            |> ParserFast.followedBySkipWhileWhitespace
        )
        whitespaceAndCommentsOrEmptyLoop
        ropeEmpty


fromSingleLineCommentNode : Parser Comments
fromSingleLineCommentNode =
    ParserFast.map2
        (\content commentsAfter ->
            ropeOne content |> ropeFilledPrependTo commentsAfter
        )
        (singleLineComment
            |> ParserFast.followedBySkipWhileWhitespace
        )
        whitespaceAndCommentsOrEmptyLoop


whitespaceAndCommentsOrEmptyLoop : Parser Comments
whitespaceAndCommentsOrEmptyLoop =
    ParserFast.loopWhileSucceeds
        (ParserFast.oneOf2
            singleLineComment
            multiLineComment
            |> ParserFast.followedBySkipWhileWhitespace
        )
        ropeEmpty
        (\right soFar -> soFar |> ropePrependToFilled (ropeOne right))
        identity


positivelyIndentedFollowedBy : Parser a -> Parser a
positivelyIndentedFollowedBy nextParser =
    ParserFast.columnIndentAndThen
        (\column indent ->
            if
                (column > 1)
                    && (indent |> List.all (\nestedIndent -> column /= nestedIndent))
            then
                nextParser

            else
                ParserFast.problem
        )


whitespaceAndCommentsEndsTopIndentedFollowedByComments : Parser Comments -> Parser Comments
whitespaceAndCommentsEndsTopIndentedFollowedByComments nextParser =
    ParserFast.map2
        (\commentsBefore afterComments ->
            commentsBefore |> ropePrependTo afterComments
        )
        whitespaceAndComments
        (topIndentedFollowedBy nextParser)


whitespaceAndCommentsEndsTopIndentedFollowedByWithComments : Parser (WithComments syntax) -> Parser (WithComments syntax)
whitespaceAndCommentsEndsTopIndentedFollowedByWithComments nextParser =
    ParserFast.map2
        (\commentsBefore after ->
            { comments = commentsBefore |> ropePrependTo after.comments
            , syntax = after.syntax
            }
        )
        whitespaceAndComments
        (topIndentedFollowedBy nextParser)


whitespaceAndCommentsEndsTopIndentedFollowedBy : Parser syntax -> Parser (WithComments syntax)
whitespaceAndCommentsEndsTopIndentedFollowedBy nextParser =
    ParserFast.map2
        (\commentsBefore after ->
            { comments = commentsBefore, syntax = after }
        )
        whitespaceAndComments
        (topIndentedFollowedBy nextParser)


whitespaceAndCommentsEndsTopIndented : Parser Comments
whitespaceAndCommentsEndsTopIndented =
    whitespaceAndComments |> endsTopIndented


endsTopIndented : Parser a -> Parser a
endsTopIndented parser =
    ParserFast.validateEndColumnIndentation
        (\column indent ->
            case indent of
                [] ->
                    column == 1

                highestIndent :: _ ->
                    column - highestIndent == 0
        )
        parser


topIndentedFollowedBy : Parser a -> Parser a
topIndentedFollowedBy nextParser =
    ParserFast.columnIndentAndThen
        (\column indent ->
            case indent of
                [] ->
                    if column == 1 then
                        nextParser

                    else
                        ParserFast.problem

                highestIndent :: _ ->
                    if column - highestIndent == 0 then
                        nextParser

                    else
                        ParserFast.problem
        )


type alias WithComments res =
    { comments : Comments, syntax : res }


{-| A bag of comment nodes.
Each comment string contains the `{-`, `-}` or `--`.

Access with [`commentsToList`](#commentsToList)

-}
type alias Comments =
    Maybe (RopeFilled (GrenSyntax.Node String))


{-| Extract a list of comment nodes from parse result [`Comments`](#Comments)
-}
commentsToList : Comments -> List (GrenSyntax.Node String)
commentsToList comments =
    ropeToList comments


untilWithComments : ParserFast.Parser () -> ParserFast.Parser (WithComments a) -> ParserFast.Parser (WithComments (List a))
untilWithComments end element =
    ParserFast.loopUntil
        end
        element
        ( ropeEmpty, [] )
        (\pResult ( commentsSoFar, itemsSoFar ) ->
            ( commentsSoFar |> ropePrependTo pResult.comments
            , pResult.syntax :: itemsSoFar
            )
        )
        (\( commentsSoFar, itemsSoFar ) ->
            { comments = commentsSoFar
            , syntax = List.reverse itemsSoFar
            }
        )


manyWithComments : ParserFast.Parser (WithComments a) -> ParserFast.Parser (WithComments (List a))
manyWithComments p =
    ParserFast.loopWhileSucceeds p
        ( ropeEmpty, [] )
        (\pResult ( commentsSoFar, itemsSoFar ) ->
            ( commentsSoFar |> ropePrependTo pResult.comments
            , pResult.syntax :: itemsSoFar
            )
        )
        (\( commentsSoFar, itemsSoFar ) ->
            { comments = commentsSoFar
            , syntax = List.reverse itemsSoFar
            }
        )


{-| Same as `manyWithComments` except that it doesn't reverse the list.
This can be useful if you need to access the range of the last item.

Mind you the comments will be reversed either way

-}
manyWithCommentsReverse : ParserFast.Parser (WithComments a) -> ParserFast.Parser (WithComments (List a))
manyWithCommentsReverse p =
    ParserFast.loopWhileSucceeds p
        { comments = ropeEmpty, syntax = [] }
        (\pResult soFar ->
            { comments = soFar.comments |> ropePrependTo pResult.comments
            , syntax = pResult.syntax :: soFar.syntax
            }
        )
        (\result -> result)


type alias Rope a =
    Maybe (RopeFilled a)


{-| Constantly appending lists of comments when combining parse can get expensive,
so we summarize everything in this temporary structure
and only convert to a list when we're done.

Inspired by [miniBill/gren-rope](https://gren-lang.org/packages/miniBill/gren-rope/latest/)

-}
type RopeFilled a
    = RopeLeaf a ()
    | RopeBranch2 (RopeFilled a) (RopeFilled a)


ropeEmpty : Rope a_
ropeEmpty =
    Nothing


ropeOne : a -> RopeFilled a
ropeOne onlyElement =
    RopeLeaf onlyElement ()


ropeFilledPrependTo : Rope a -> RopeFilled a -> Rope a
ropeFilledPrependTo right leftLikelyFilled =
    Just
        (case right of
            Nothing ->
                leftLikelyFilled

            Just rightLikelyFilled ->
                RopeBranch2 leftLikelyFilled rightLikelyFilled
        )


ropePrependToFilled : RopeFilled a -> Rope a -> Rope a
ropePrependToFilled rightLikelyFilled left =
    Just
        (case left of
            Nothing ->
                rightLikelyFilled

            Just leftLikelyFilled ->
                RopeBranch2 leftLikelyFilled rightLikelyFilled
        )


ropePrependTo : Rope a -> Rope a -> Rope a
ropePrependTo right left =
    case left of
        Nothing ->
            right

        Just leftLikelyFilled ->
            case right of
                Nothing ->
                    left

                Just rightLikelyFilled ->
                    Just (RopeBranch2 leftLikelyFilled rightLikelyFilled)


ropeToList : Rope a -> List a
ropeToList rope =
    case rope of
        Nothing ->
            []

        Just ropeLikelyFilled ->
            ropeLikelyFilledToListInto [] ropeLikelyFilled


ropeLikelyFilledToListInto : List a -> RopeFilled a -> List a
ropeLikelyFilledToListInto initialAcc ropeLikelyFilled =
    -- IGNORE TCO
    case ropeLikelyFilled of
        RopeLeaf onlyElement () ->
            onlyElement :: initialAcc

        RopeBranch2 left right ->
            ropeLikelyFilledToListInto
                (ropeLikelyFilledToListInto
                    initialAcc
                    right
                )
                left
