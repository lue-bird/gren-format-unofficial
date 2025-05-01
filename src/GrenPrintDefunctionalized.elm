module GrenPrintDefunctionalized exposing
    ( module_
    , moduleHeader, moduleExposing, expose, imports, import_, importExposing, moduleLevelComments, comments, comment
    , declarations, declaration, declarationChoiceType, declarationSignature, declarationExpression, declarationInfix, declarationPort, declarationTypeAlias
    , expressionNotParenthesized, case_, patternNotParenthesized, typeNotParenthesized
    , moduleName, qualifiedReference
    )

{-|

@docs module_

@docs moduleHeader, moduleExposing, expose, imports, import_, importExposing, moduleLevelComments, comments, comment
@docs declarations, declaration, declarationChoiceType, declarationSignature, declarationExpression, declarationInfix, declarationPort, declarationTypeAlias
@docs expressionNotParenthesized, case_, patternNotParenthesized, typeNotParenthesized
@docs moduleName, qualifiedReference

-}

import Bitwise
import GrenSyntax
import Print exposing (Print)
import Unicode


{-| Print an [`GrenSyntax.File`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-File#File)
-}
module_ : GrenSyntax.File -> Print
module_ syntaxModule =
    let
        maybeModuleDocumentation : Maybe (GrenSyntax.Node String)
        maybeModuleDocumentation =
            moduleDocumentation syntaxModule

        commentsAndPortDocumentationComments :
            { portDocumentationComments : List (GrenSyntax.Node String)
            , remainingComments : List (GrenSyntax.Node String)
            }
        commentsAndPortDocumentationComments =
            (case maybeModuleDocumentation of
                Nothing ->
                    syntaxModule.comments

                Just syntaxModuleDocumentation ->
                    syntaxModule.comments
                        |> List.filter (\c -> c /= syntaxModuleDocumentation)
            )
                |> splitOffPortDocumentationComments

        maybeModuleDocumentationParsed :
            Maybe
                { whileAtDocsLines : List { rawBefore : String, atDocsLine : List String }
                , rawAfterAtDocsLines : String
                }
        maybeModuleDocumentationParsed =
            case maybeModuleDocumentation of
                Nothing ->
                    Nothing

                Just syntaxModuleDocumentationNode ->
                    Just
                        (syntaxModuleDocumentationNode.value
                            |> moduleDocumentationParse
                        )

        atDocsLines : List (List String)
        atDocsLines =
            case maybeModuleDocumentationParsed of
                Nothing ->
                    []

                Just moduleDocumentationParsed ->
                    moduleDocumentationParsed.whileAtDocsLines
                        |> List.map .atDocsLine

        lastSyntaxLocationBeforeDeclarations : GrenSyntax.Location
        lastSyntaxLocationBeforeDeclarations =
            case syntaxModule.imports of
                firstImport :: _ ->
                    firstImport.range.end

                [] ->
                    syntaxModule.moduleDefinition
                        |> GrenSyntax.nodeRange
                        |> .end

        commentsBeforeDeclarations : List String
        commentsBeforeDeclarations =
            case syntaxModule.declarations of
                [] ->
                    -- invalid syntax
                    []

                declaration0 :: _ ->
                    commentsInRange
                        { start = lastSyntaxLocationBeforeDeclarations
                        , end = declaration0.range.start
                        }
                        commentsAndPortDocumentationComments.remainingComments
    in
    syntaxModule.moduleDefinition
        |> GrenSyntax.nodeValue
        |> moduleHeader { atDocsLines = atDocsLines, comments = commentsAndPortDocumentationComments.remainingComments }
        |> Print.followedBy
            (case maybeModuleDocumentationParsed of
                Nothing ->
                    Print.empty

                Just moduleDocumentationParsed ->
                    printLinebreakLinebreak
                        |> Print.followedBy (printModuleDocumentation moduleDocumentationParsed)
            )
        |> Print.followedBy
            (case syntaxModule.imports of
                [] ->
                    case maybeModuleDocumentation of
                        Nothing ->
                            printLinebreakLinebreak

                        Just _ ->
                            case commentsBeforeDeclarations of
                                [] ->
                                    printLinebreakLinebreak

                                _ :: _ ->
                                    Print.empty

                import0Node :: import1Up ->
                    (case
                        commentsInRange
                            { start =
                                syntaxModule.moduleDefinition
                                    |> GrenSyntax.nodeRange
                                    |> .end
                            , end = import0Node.range.start
                            }
                            commentsAndPortDocumentationComments.remainingComments
                     of
                        [] ->
                            Print.linebreak

                        comment0 :: comment1Up ->
                            printLinebreakLinebreak
                                |> Print.followedBy
                                    (moduleLevelComments (comment0 :: comment1Up))
                    )
                        |> Print.followedBy Print.linebreak
                        |> Print.followedBy
                            ((import0Node :: import1Up)
                                |> imports commentsAndPortDocumentationComments.remainingComments
                            )
                        |> Print.followedBy printLinebreakLinebreak
            )
        |> Print.followedBy Print.linebreak
        |> Print.followedBy
            (case commentsBeforeDeclarations of
                [] ->
                    Print.empty

                comment0 :: comment1Up ->
                    moduleLevelCommentsBeforeDeclaration
                        { comment0 = comment0, comment1Up = comment1Up }
            )
        |> Print.followedBy
            (syntaxModule.declarations
                |> declarations
                    { comments = commentsAndPortDocumentationComments.remainingComments
                    , portDocumentationComments = commentsAndPortDocumentationComments.portDocumentationComments
                    , previousEnd = lastSyntaxLocationBeforeDeclarations
                    }
            )
        |> Print.followedBy Print.linebreak
        |> Print.followedBy
            (case syntaxModule.declarations of
                [] ->
                    -- invalid syntax
                    Print.empty

                declaration0 :: declaration1Up ->
                    case
                        commentsAfter
                            (listFilledLast declaration0 declaration1Up
                                |> GrenSyntax.nodeRange
                                |> .end
                            )
                            commentsAndPortDocumentationComments.remainingComments
                    of
                        [] ->
                            Print.empty

                        comment0 :: comment1Up ->
                            printLinebreakLinebreakLinebreak
                                |> Print.followedBy
                                    (moduleLevelComments (comment0 :: comment1Up))
            )


printModuleDocumentation :
    { whileAtDocsLines : List { rawBefore : String, atDocsLine : List String }
    , rawAfterAtDocsLines : String
    }
    -> Print
printModuleDocumentation moduleDocumentationBlocks =
    let
        content : String
        content =
            ((moduleDocumentationBlocks.whileAtDocsLines
                |> listMapAndFlattenToString
                    (\atDocsLineAndBefore ->
                        atDocsLineAndBefore.rawBefore
                            ++ (case atDocsLineAndBefore.atDocsLine of
                                    [] ->
                                        ""

                                    atDocsExpose0 :: atDocsExpose1Up ->
                                        "@docs "
                                            ++ ((atDocsExpose0 :: atDocsExpose1Up)
                                                    |> String.join ", "
                                               )
                                            ++ "\n"
                               )
                    )
             )
                ++ moduleDocumentationBlocks.rawAfterAtDocsLines
            )
                |> String.trimRight
    in
    Print.exactly
        ("{-|"
            ++ (if content |> String.startsWith "@docs " then
                    "\n\n"

                else
                    " "
               )
            ++ content
            ++ (if content |> String.contains "\n" then
                    "\n\n-}"

                else
                    "\n-}"
               )
        )


{-| Resulting lists are sorted by range
-}
splitOffPortDocumentationComments :
    List (GrenSyntax.Node String)
    ->
        { portDocumentationComments : List (GrenSyntax.Node String)
        , remainingComments : List (GrenSyntax.Node String)
        }
splitOffPortDocumentationComments commentsAndPortDocumentationComments =
    commentsAndPortDocumentationComments
        |> List.foldr
            (\commentOrPortDocumentationComments soFar ->
                if commentOrPortDocumentationComments |> GrenSyntax.nodeValue |> String.startsWith "{-|" then
                    { remainingComments = soFar.remainingComments
                    , portDocumentationComments = commentOrPortDocumentationComments :: soFar.portDocumentationComments
                    }

                else
                    { remainingComments = commentOrPortDocumentationComments :: soFar.remainingComments
                    , portDocumentationComments = soFar.portDocumentationComments
                    }
            )
            commentsEmptyPortDocumentationRemainingCommentsEmpty


commentsEmptyPortDocumentationRemainingCommentsEmpty : { remainingComments : List a_, portDocumentationComments : List b_ }
commentsEmptyPortDocumentationRemainingCommentsEmpty =
    { remainingComments = [], portDocumentationComments = [] }


moduleDocumentation : GrenSyntax.File -> Maybe (GrenSyntax.Node String)
moduleDocumentation ast =
    let
        cutOffLine : Int
        cutOffLine =
            case ast.imports of
                firstImportNode :: _ ->
                    firstImportNode.range.start.row

                [] ->
                    case ast.declarations of
                        firstDeclarationNode :: _ ->
                            firstDeclarationNode.range.start.row

                        [] ->
                            -- Should not happen, as every module should have at least one declaration
                            0
    in
    moduleDocumentationBeforeCutOffLine cutOffLine ast.comments


moduleDocumentationBeforeCutOffLine : Int -> List (GrenSyntax.Node String) -> Maybe (GrenSyntax.Node String)
moduleDocumentationBeforeCutOffLine cutOffLine allComments =
    case allComments of
        [] ->
            Nothing

        headCommentNode :: restOfComments ->
            if headCommentNode.range.start.row > cutOffLine then
                Nothing

            else if String.startsWith "{-|" headCommentNode.value then
                Just headCommentNode

            else
                moduleDocumentationBeforeCutOffLine cutOffLine restOfComments


moduleDocumentationParse :
    String
    ->
        { whileAtDocsLines : List { rawBefore : String, atDocsLine : List String }
        , rawAfterAtDocsLines : String
        }
moduleDocumentationParse moduleDocumentationContent =
    let
        parsed :
            { rawSinceAtDocs : String
            , finishedBlocks : List { rawBefore : String, atDocsLine : List String }
            }
        parsed =
            moduleDocumentationContent
                |> String.dropLeft
                    -- String.length "{-|"
                    3
                |> String.trimLeft
                |> String.dropRight
                    -- String.length "-}"
                    2
                |> String.lines
                |> List.foldl
                    (\line soFar ->
                        if line |> String.startsWith "@docs " then
                            { rawSinceAtDocs = ""
                            , finishedBlocks =
                                { rawBefore = soFar.rawSinceAtDocs
                                , atDocsLine =
                                    String.slice 6 (line |> String.length) line
                                        |> String.split ","
                                        |> List.map String.trim
                                }
                                    :: soFar.finishedBlocks
                            }

                        else
                            case line of
                                "@docs" ->
                                    { rawSinceAtDocs = ""
                                    , finishedBlocks =
                                        { rawBefore = soFar.rawSinceAtDocs
                                        , atDocsLine = []
                                        }
                                            :: soFar.finishedBlocks
                                    }

                                _ ->
                                    { rawSinceAtDocs = soFar.rawSinceAtDocs ++ line ++ "\n"
                                    , finishedBlocks = soFar.finishedBlocks
                                    }
                    )
                    rawSinceAtDocsEmptyFinishedBlocksEmpty
    in
    { whileAtDocsLines = parsed.finishedBlocks |> List.reverse
    , rawAfterAtDocsLines = parsed.rawSinceAtDocs
    }


rawSinceAtDocsEmptyFinishedBlocksEmpty : { rawSinceAtDocs : String, finishedBlocks : List a_ }
rawSinceAtDocsEmptyFinishedBlocksEmpty =
    { rawSinceAtDocs = "", finishedBlocks = [] }


commentsAfter : GrenSyntax.Location -> List (GrenSyntax.Node String) -> List String
commentsAfter end sortedComments =
    case sortedComments of
        [] ->
            []

        headCommentNode :: tailComments ->
            case locationCompareFast headCommentNode.range.start end of
                LT ->
                    commentsAfter end tailComments

                GT ->
                    headCommentNode.value :: (tailComments |> List.map GrenSyntax.nodeValue)

                EQ ->
                    headCommentNode.value :: (tailComments |> List.map GrenSyntax.nodeValue)


moduleLevelCommentsBeforeDeclaration : { comment0 : String, comment1Up : List String } -> Print
moduleLevelCommentsBeforeDeclaration syntaxComments =
    Print.linebreak
        |> Print.followedBy
            (moduleLevelComments (syntaxComments.comment0 :: syntaxComments.comment1Up))
        |> Print.followedBy
            (case listFilledLast syntaxComments.comment0 syntaxComments.comment1Up of
                "{--}" ->
                    -- rudiment from elm-format
                    Print.empty

                _ ->
                    printLinebreakLinebreak
            )


commentNodesInRange : GrenSyntax.Range -> List (GrenSyntax.Node String) -> List (GrenSyntax.Node String)
commentNodesInRange range sortedComments =
    case sortedComments of
        [] ->
            []

        headCommentNode :: tailComments ->
            case locationCompareFast headCommentNode.range.start range.start of
                LT ->
                    commentNodesInRange range tailComments

                EQ ->
                    headCommentNode :: commentNodesInRange range tailComments

                GT ->
                    case locationCompareFast headCommentNode.range.end range.end of
                        GT ->
                            []

                        LT ->
                            headCommentNode :: commentNodesInRange range tailComments

                        EQ ->
                            headCommentNode :: commentNodesInRange range tailComments


commentsInRange : GrenSyntax.Range -> List (GrenSyntax.Node String) -> List String
commentsInRange range sortedComments =
    case sortedComments of
        [] ->
            []

        headCommentNode :: tailComments ->
            case locationCompareFast headCommentNode.range.start range.start of
                LT ->
                    commentsInRange range tailComments

                EQ ->
                    headCommentNode.value :: commentsInRange range tailComments

                GT ->
                    case locationCompareFast headCommentNode.range.end range.end of
                        GT ->
                            []

                        LT ->
                            headCommentNode.value :: commentsInRange range tailComments

                        EQ ->
                            headCommentNode.value :: commentsInRange range tailComments


lineSpreadInRange : GrenSyntax.Range -> Print.LineSpread
lineSpreadInRange range =
    if range.end.row - range.start.row == 0 then
        Print.SingleLine

    else
        Print.MultipleLines


exposingMulti :
    List (GrenSyntax.Node String)
    ->
        { fullRange : GrenSyntax.Range
        , expose0 : GrenSyntax.Node GrenSyntax.TopLevelExpose
        , expose1Up : List (GrenSyntax.Node GrenSyntax.TopLevelExpose)
        }
    -> Print
exposingMulti syntaxComments syntaxExposing =
    let
        containedComments : List String
        containedComments =
            commentsInRange syntaxExposing.fullRange syntaxComments

        lineSpread : Print.LineSpread
        lineSpread =
            case containedComments of
                _ :: _ ->
                    Print.MultipleLines

                [] ->
                    lineSpreadInRange syntaxExposing.fullRange
    in
    (case lineSpread of
        Print.SingleLine ->
            printExactlyParensOpening

        Print.MultipleLines ->
            printExactlyParensOpeningSpace
    )
        |> Print.followedBy
            ((syntaxExposing.expose0 :: syntaxExposing.expose1Up)
                |> Print.listMapAndIntersperseAndFlatten
                    (\syntaxExposeNode ->
                        Print.exactly (expose syntaxExposeNode.value)
                    )
                    (Print.emptyOrLinebreakIndented lineSpread
                        |> Print.followedBy printExactlyCommaSpace
                    )
            )
        |> Print.followedBy (Print.emptyOrLinebreakIndented lineSpread)
        |> Print.followedBy
            (case containedComments of
                [] ->
                    printExactlyParensClosing

                comment0 :: comment1Up ->
                    comments (comment0 :: comment1Up)
                        |> Print.followedBy (Print.emptyOrLinebreakIndented lineSpread)
                        |> Print.followedBy printExactlyParensClosing
            )


listMapAndFlattenToString : (a -> String) -> List a -> String
listMapAndFlattenToString elementToString elements =
    elements
        |> List.foldl
            (\next soFar -> soFar ++ elementToString next ++ "")
            ""


listMapAndIntersperseAndFlattenToString : (a -> String) -> String -> List a -> String
listMapAndIntersperseAndFlattenToString elementToString betweenElements elements =
    case elements of
        [] ->
            ""

        head :: tail ->
            tail
                |> List.foldl
                    (\next soFar -> soFar ++ betweenElements ++ elementToString next ++ "")
                    (elementToString head)


exposeCompare : GrenSyntax.TopLevelExpose -> GrenSyntax.TopLevelExpose -> Basics.Order
exposeCompare a b =
    case a of
        GrenSyntax.InfixExpose aOperatorSymbol ->
            case b of
                GrenSyntax.InfixExpose bOperatorSymbol ->
                    compare aOperatorSymbol bOperatorSymbol

                GrenSyntax.FunctionExpose _ ->
                    LT

                GrenSyntax.TypeOrAliasExpose _ ->
                    LT

                GrenSyntax.TypeExpose _ ->
                    LT

        GrenSyntax.FunctionExpose aName ->
            case b of
                GrenSyntax.InfixExpose _ ->
                    GT

                GrenSyntax.FunctionExpose bName ->
                    compare aName bName

                GrenSyntax.TypeOrAliasExpose _ ->
                    GT

                GrenSyntax.TypeExpose _ ->
                    GT

        GrenSyntax.TypeOrAliasExpose aName ->
            case b of
                GrenSyntax.InfixExpose _ ->
                    GT

                GrenSyntax.FunctionExpose _ ->
                    LT

                GrenSyntax.TypeOrAliasExpose bName ->
                    compare aName bName

                GrenSyntax.TypeExpose bTypeExpose ->
                    compare aName bTypeExpose.name

        GrenSyntax.TypeExpose aTypeExpose ->
            case b of
                GrenSyntax.InfixExpose _ ->
                    GT

                GrenSyntax.FunctionExpose _ ->
                    LT

                GrenSyntax.TypeOrAliasExpose bName ->
                    compare aTypeExpose.name bName

                GrenSyntax.TypeExpose bTypeExpose ->
                    compare aTypeExpose.name bTypeExpose.name


exposeListToNormal :
    List (GrenSyntax.Node GrenSyntax.TopLevelExpose)
    -> List (GrenSyntax.Node GrenSyntax.TopLevelExpose)
exposeListToNormal syntaxExposeList =
    syntaxExposeList
        |> List.sortWith
            (\a b ->
                exposeCompare a.value b.value
            )
        |> exposesCombine


exposesCombine :
    List (GrenSyntax.Node GrenSyntax.TopLevelExpose)
    -> List (GrenSyntax.Node GrenSyntax.TopLevelExpose)
exposesCombine syntaxExposes =
    case syntaxExposes of
        [] ->
            []

        [ _ ] as onlyExposeList ->
            onlyExposeList

        expose0Node :: (expose1Node :: expose2Up) ->
            case exposeCompare expose0Node.value expose1Node.value of
                EQ ->
                    exposesCombine
                        ({ range = expose0Node.range, value = exposeMerge expose0Node.value expose1Node.value }
                            :: expose2Up
                        )

                LT ->
                    expose0Node :: exposesCombine (expose1Node :: expose2Up)

                GT ->
                    expose0Node :: exposesCombine (expose1Node :: expose2Up)


exposeMerge : GrenSyntax.TopLevelExpose -> GrenSyntax.TopLevelExpose -> GrenSyntax.TopLevelExpose
exposeMerge a b =
    case a of
        GrenSyntax.TypeExpose aTypeExpose ->
            case b of
                GrenSyntax.TypeExpose bTypeExpose ->
                    GrenSyntax.TypeExpose
                        { name = aTypeExpose.name
                        , open =
                            case aTypeExpose.open of
                                Just openRange ->
                                    Just openRange

                                Nothing ->
                                    bTypeExpose.open
                        }

                GrenSyntax.InfixExpose _ ->
                    GrenSyntax.TypeExpose aTypeExpose

                GrenSyntax.FunctionExpose _ ->
                    GrenSyntax.TypeExpose aTypeExpose

                GrenSyntax.TypeOrAliasExpose _ ->
                    GrenSyntax.TypeExpose aTypeExpose

        GrenSyntax.InfixExpose _ ->
            b

        GrenSyntax.FunctionExpose _ ->
            b

        GrenSyntax.TypeOrAliasExpose _ ->
            b


{-| Print the stuff after `exposing` in a module header.
For import exposing: [`importExposing`](#importExposing)
-}
moduleExposing :
    { atDocsLines : List (List String), comments : List (GrenSyntax.Node String) }
    -> GrenSyntax.Node GrenSyntax.Exposing
    -> Print
moduleExposing context syntaxExposingNode =
    case syntaxExposingNode.value of
        GrenSyntax.All _ ->
            printExactlyParensOpeningDotDotParensClosing

        GrenSyntax.Explicit exposingSet ->
            case exposingSet |> exposeListToNormal of
                [] ->
                    printExactlyCurlyBraceOpeningCurlyBraceClosing

                [ onlySyntaxExposeNode ] ->
                    let
                        containedComments : List String
                        containedComments =
                            commentsInRange syntaxExposingNode.range context.comments

                        lineSpread : Print.LineSpread
                        lineSpread =
                            case containedComments of
                                _ :: _ ->
                                    Print.MultipleLines

                                [] ->
                                    Print.SingleLine
                    in
                    Print.exactly
                        ((case lineSpread of
                            Print.SingleLine ->
                                "("

                            Print.MultipleLines ->
                                "( "
                         )
                            ++ expose onlySyntaxExposeNode.value
                            ++ ""
                        )
                        |> Print.followedBy (Print.emptyOrLinebreakIndented lineSpread)
                        |> Print.followedBy
                            (case containedComments of
                                [] ->
                                    printExactlyParensClosing

                                comment0 :: comment1Up ->
                                    comments (comment0 :: comment1Up)
                                        |> Print.followedBy (Print.emptyOrLinebreakIndented lineSpread)
                                        |> Print.followedBy printExactlyParensClosing
                            )

                expose0 :: expose1 :: expose2Up ->
                    case context.atDocsLines of
                        atDocsLine0 :: atDocsLine1Up ->
                            let
                                atDocsExposeLines :
                                    { remainingExposes : List GrenSyntax.TopLevelExpose
                                    , atDocsExposeLines : List (List GrenSyntax.TopLevelExpose)
                                    }
                                atDocsExposeLines =
                                    (atDocsLine0 :: atDocsLine1Up)
                                        |> List.foldr
                                            (\atDocsLine soFar ->
                                                let
                                                    atDocsExposeLine :
                                                        { remaining : List GrenSyntax.TopLevelExpose
                                                        , exposes : List GrenSyntax.TopLevelExpose
                                                        }
                                                    atDocsExposeLine =
                                                        atDocsLineToExposesAndRemaining atDocsLine soFar.remainingExposes
                                                in
                                                { atDocsExposeLines =
                                                    atDocsExposeLine.exposes :: soFar.atDocsExposeLines
                                                , remainingExposes = atDocsExposeLine.remaining
                                                }
                                            )
                                            { remainingExposes =
                                                (expose0 :: expose1 :: expose2Up)
                                                    |> List.map GrenSyntax.nodeValue
                                            , atDocsExposeLines = []
                                            }
                            in
                            case
                                atDocsExposeLines.atDocsExposeLines
                                    |> List.filter
                                        (\line ->
                                            case line of
                                                [] ->
                                                    False

                                                _ :: _ ->
                                                    True
                                        )
                            of
                                [] ->
                                    exposingMulti context.comments
                                        { fullRange = syntaxExposingNode.range
                                        , expose0 = expose0
                                        , expose1Up = expose1 :: expose2Up
                                        }

                                atDocsExposeLine0 :: atDocsExposeLine1Up ->
                                    printExactlyParensOpeningSpace
                                        |> Print.followedBy
                                            ((atDocsExposeLine0 :: atDocsExposeLine1Up)
                                                |> Print.listMapAndIntersperseAndFlatten
                                                    (\atDocsLine ->
                                                        Print.exactly
                                                            (atDocsLine
                                                                |> listMapAndIntersperseAndFlattenToString
                                                                    expose
                                                                    ", "
                                                            )
                                                    )
                                                    printLinebreakIndentedCommaSpace
                                            )
                                        |> Print.followedBy Print.linebreakIndented
                                        |> Print.followedBy
                                            (case atDocsExposeLines.remainingExposes of
                                                [] ->
                                                    printExactlyParensClosing

                                                remainingExpose0 :: remainingExpose1Up ->
                                                    Print.exactly
                                                        (", "
                                                            ++ ((remainingExpose0 :: remainingExpose1Up)
                                                                    |> listMapAndIntersperseAndFlattenToString
                                                                        expose
                                                                        ", "
                                                               )
                                                        )
                                                        |> Print.followedBy Print.linebreakIndented
                                                        |> Print.followedBy printExactlyParensClosing
                                            )

                        [] ->
                            exposingMulti context.comments
                                { fullRange = syntaxExposingNode.range
                                , expose0 = expose0
                                , expose1Up = expose1 :: expose2Up
                                }


atDocsLineToExposesAndRemaining :
    List String
    -> List GrenSyntax.TopLevelExpose
    ->
        { remaining : List GrenSyntax.TopLevelExpose
        , exposes : List GrenSyntax.TopLevelExpose
        }
atDocsLineToExposesAndRemaining atDocsLine remainingExposes =
    atDocsLine
        |> List.foldr
            (\exposeAsAtDocsString soFar ->
                let
                    toExposeReferencedByAtDocsString : GrenSyntax.TopLevelExpose -> Maybe GrenSyntax.TopLevelExpose
                    toExposeReferencedByAtDocsString ex =
                        if (ex |> exposeToAtDocsString) == exposeAsAtDocsString then
                            Just ex

                        else
                            Nothing
                in
                case soFar.remaining |> listFirstJustMap toExposeReferencedByAtDocsString of
                    Nothing ->
                        soFar

                    Just exposeReferencedByAtDocsString ->
                        { remaining =
                            soFar.remaining
                                |> List.filter (\ex -> ex /= exposeReferencedByAtDocsString)
                        , exposes = exposeReferencedByAtDocsString :: soFar.exposes
                        }
            )
            { remaining = remainingExposes
            , exposes = []
            }


exposeToAtDocsString : GrenSyntax.TopLevelExpose -> String
exposeToAtDocsString syntaxExpose =
    case syntaxExpose of
        GrenSyntax.InfixExpose operatorSymbol ->
            "(" ++ operatorSymbol ++ ")"

        GrenSyntax.FunctionExpose name ->
            name

        GrenSyntax.TypeOrAliasExpose name ->
            name

        GrenSyntax.TypeExpose choiceTypeExpose ->
            choiceTypeExpose.name


listFirstJustMap : (a -> Maybe b) -> List a -> Maybe b
listFirstJustMap elementToMaybe list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            case elementToMaybe head of
                Nothing ->
                    listFirstJustMap elementToMaybe tail

                Just b ->
                    Just b


{-| Print an [`GrenSyntax.Module`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Module#Module)
(confusingly, that's their name for only the `module X exposing (Y)` lines)
-}
moduleHeader :
    { atDocsLines : List (List String), comments : List (GrenSyntax.Node String) }
    -> GrenSyntax.Module
    -> Print
moduleHeader context syntaxModuleHeader =
    case syntaxModuleHeader of
        GrenSyntax.NormalModule defaultModuleData ->
            let
                exposingPrint : Print
                exposingPrint =
                    defaultModuleData.exposingList
                        |> moduleExposing context

                lineSpread : Print.LineSpread
                lineSpread =
                    exposingPrint |> Print.lineSpread
            in
            Print.exactly
                ("module "
                    ++ moduleName (defaultModuleData.moduleName |> GrenSyntax.nodeValue)
                    ++ " exposing"
                )
                |> Print.followedBy
                    (Print.withIndentAtNextMultipleOf4
                        (Print.spaceOrLinebreakIndented lineSpread
                            |> Print.followedBy exposingPrint
                        )
                    )

        GrenSyntax.PortModule defaultModuleData ->
            let
                exposingPrint : Print
                exposingPrint =
                    defaultModuleData.exposingList
                        |> moduleExposing context

                lineSpread : Print.LineSpread
                lineSpread =
                    exposingPrint |> Print.lineSpread
            in
            Print.exactly
                ("port module "
                    ++ moduleName
                        (defaultModuleData.moduleName |> GrenSyntax.nodeValue)
                    ++ " exposing"
                )
                |> Print.followedBy
                    (Print.withIndentAtNextMultipleOf4
                        (Print.spaceOrLinebreakIndented lineSpread
                            |> Print.followedBy exposingPrint
                        )
                    )

        GrenSyntax.EffectModule effectModuleData ->
            let
                exposingPrint : Print
                exposingPrint =
                    effectModuleData.exposingList
                        |> moduleExposing context

                lineSpread : Print.LineSpread
                lineSpread =
                    exposingPrint |> Print.lineSpread
            in
            Print.exactly
                ("effect module "
                    ++ moduleName
                        (effectModuleData.moduleName |> GrenSyntax.nodeValue)
                    ++ " where { "
                    ++ ([ case effectModuleData.command of
                            Nothing ->
                                Nothing

                            Just nameNode ->
                                Just ("command = " ++ nameNode.value)
                        , case effectModuleData.subscription of
                            Nothing ->
                                Nothing

                            Just nameNode ->
                                Just ("subscription = " ++ nameNode.value)
                        ]
                            |> List.filterMap identity
                            |> String.join ", "
                       )
                    ++ " } exposing"
                )
                |> Print.followedBy
                    (Print.withIndentAtNextMultipleOf4
                        (Print.spaceOrLinebreakIndented lineSpread
                            |> Print.followedBy exposingPrint
                        )
                    )


{-| Print a set of [`GrenSyntax.Import`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Import#Import)s
-}
imports :
    List (GrenSyntax.Node String)
    -> List (GrenSyntax.Node GrenSyntax.Import)
    -> Print
imports syntaxComments syntaxImports =
    case syntaxImports of
        [] ->
            Print.empty

        import0Node :: imports1Up ->
            let
                commentsBetweenImports : List String
                commentsBetweenImports =
                    (import0Node :: imports1Up)
                        |> List.foldl
                            (\importNode soFar ->
                                { previousImportRange = importNode.range
                                , commentsBetweenImports =
                                    soFar.commentsBetweenImports
                                        ++ commentsInRange
                                            { start = soFar.previousImportRange.end
                                            , end = importNode.range.start
                                            }
                                            syntaxComments
                                }
                            )
                            { previousImportRange = import0Node.range
                            , commentsBetweenImports = []
                            }
                        |> .commentsBetweenImports
            in
            (case commentsBetweenImports of
                [] ->
                    Print.empty

                comment0 :: comment1Up ->
                    moduleLevelComments (comment0 :: comment1Up)
                        |> Print.followedBy Print.linebreak
            )
                |> Print.followedBy
                    ((import0Node :: imports1Up)
                        |> List.sortWith
                            (\a b ->
                                compare
                                    (a.value.moduleName |> GrenSyntax.nodeValue)
                                    (b.value.moduleName |> GrenSyntax.nodeValue)
                            )
                        |> importsCombine
                        |> Print.listMapAndIntersperseAndFlatten
                            (\syntaxImport -> import_ syntaxComments syntaxImport)
                            Print.linebreak
                    )


importsCombine :
    List (GrenSyntax.Node GrenSyntax.Import)
    -> List (GrenSyntax.Node GrenSyntax.Import)
importsCombine syntaxImports =
    case syntaxImports of
        [] ->
            []

        [ onlyImport ] ->
            [ onlyImport |> GrenSyntax.nodeMap importToNormal ]

        import0Node :: import1Node :: import2Up ->
            if
                (import0Node.value.moduleName |> GrenSyntax.nodeValue)
                    == (import1Node.value.moduleName |> GrenSyntax.nodeValue)
            then
                importsCombine
                    ({ range = import1Node.range, value = importsMerge import0Node.value import1Node.value }
                        :: import2Up
                    )

            else
                { range = import0Node.range, value = import0Node.value |> importToNormal }
                    :: importsCombine (import1Node :: import2Up)


importToNormal : GrenSyntax.Import -> GrenSyntax.Import
importToNormal syntaxImport =
    { moduleName = syntaxImport.moduleName
    , moduleAlias = syntaxImport.moduleAlias
    , exposingList =
        case syntaxImport.exposingList of
            Nothing ->
                Nothing

            Just syntaxExposingNode ->
                Just
                    { range = syntaxExposingNode.range, value = syntaxExposingNode.value |> exposingToNormal }
    }


exposingToNormal : GrenSyntax.Exposing -> GrenSyntax.Exposing
exposingToNormal syntaxExposing =
    case syntaxExposing of
        GrenSyntax.All allRange ->
            GrenSyntax.All allRange

        GrenSyntax.Explicit exposeSet ->
            GrenSyntax.Explicit (exposeSet |> exposeListToNormal)


importsMerge : GrenSyntax.Import -> GrenSyntax.Import -> GrenSyntax.Import
importsMerge earlier later =
    { moduleName = later.moduleName
    , moduleAlias =
        case earlier.moduleAlias of
            Just alias ->
                alias |> Just

            Nothing ->
                later.moduleAlias
    , exposingList =
        exposingCombine earlier.exposingList later.exposingList
    }


exposingCombine :
    Maybe (GrenSyntax.Node GrenSyntax.Exposing)
    -> Maybe (GrenSyntax.Node GrenSyntax.Exposing)
    -> Maybe (GrenSyntax.Node GrenSyntax.Exposing)
exposingCombine a b =
    case a of
        Just aExposingNode ->
            case aExposingNode.value of
                GrenSyntax.All allRange ->
                    Just
                        { range = aExposingNode.range, value = GrenSyntax.All allRange }

                GrenSyntax.Explicit earlierExposeSet ->
                    Just
                        (case b of
                            Just bExposingNode ->
                                case bExposingNode.value of
                                    GrenSyntax.All allRange ->
                                        { range = bExposingNode.range, value = GrenSyntax.All allRange }

                                    GrenSyntax.Explicit laterExposeSet ->
                                        { range =
                                            case lineSpreadInRange aExposingNode.range of
                                                Print.MultipleLines ->
                                                    aExposingNode.range

                                                Print.SingleLine ->
                                                    bExposingNode.range
                                        , value = GrenSyntax.Explicit (earlierExposeSet ++ laterExposeSet |> exposeListToNormal)
                                        }

                            Nothing ->
                                { range = aExposingNode.range, value = GrenSyntax.Explicit earlierExposeSet }
                        )

        Nothing ->
            b


{-| Print a single [`GrenSyntax.Import`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Import#Import)
-}
import_ :
    List (GrenSyntax.Node String)
    -> GrenSyntax.Node GrenSyntax.Import
    -> Print
import_ syntaxComments syntaxImportNode =
    let
        importRange : GrenSyntax.Range
        importRange =
            case syntaxImportNode.value.exposingList of
                Nothing ->
                    syntaxImportNode.range

                Just syntaxExposingNode ->
                    { start = syntaxImportNode.range.start
                    , end = syntaxExposingNode.range.end
                    }
    in
    printExactImport
        |> Print.followedBy
            (case syntaxImportNode.value.moduleAlias of
                Nothing ->
                    case commentsInRange { start = importRange.start, end = syntaxImportNode.value.moduleName.range.start } syntaxComments of
                        [] ->
                            Print.exactly
                                (" "
                                    ++ moduleName syntaxImportNode.value.moduleName.value
                                )

                        comment0 :: comment1Up ->
                            Print.withIndentAtNextMultipleOf4
                                (Print.linebreakIndented
                                    |> Print.followedBy (comments (comment0 :: comment1Up))
                                    |> Print.followedBy Print.linebreakIndented
                                    |> Print.followedBy
                                        (Print.exactly (moduleName syntaxImportNode.value.moduleName.value))
                                )

                Just moduleAliasNode ->
                    case
                        commentsInRange { start = syntaxImportNode.value.moduleName.range.end, end = moduleAliasNode.range.start }
                            syntaxComments
                    of
                        [] ->
                            (case commentsInRange { start = importRange.start, end = syntaxImportNode.value.moduleName.range.start } syntaxComments of
                                [] ->
                                    Print.exactly
                                        (" "
                                            ++ moduleName syntaxImportNode.value.moduleName.value
                                        )

                                moduleNameComment0 :: moduleNameComment1Up ->
                                    Print.withIndentAtNextMultipleOf4
                                        (Print.linebreakIndented
                                            |> Print.followedBy (comments (moduleNameComment0 :: moduleNameComment1Up))
                                            |> Print.followedBy Print.linebreakIndented
                                            |> Print.followedBy (Print.exactly (moduleName syntaxImportNode.value.moduleName.value))
                                        )
                            )
                                |> Print.followedBy (Print.exactly (" as " ++ moduleName moduleAliasNode.value))

                        aliasComment0 :: aliasComment1Up ->
                            Print.withIndentAtNextMultipleOf4
                                (Print.linebreakIndented
                                    |> Print.followedBy
                                        (case commentsInRange { start = importRange.start, end = syntaxImportNode.value.moduleName.range.start } syntaxComments of
                                            [] ->
                                                Print.exactly (moduleName syntaxImportNode.value.moduleName.value)

                                            moduleNameComment0 :: moduleNameComment1Up ->
                                                comments (moduleNameComment0 :: moduleNameComment1Up)
                                                    |> Print.followedBy Print.linebreakIndented
                                                    |> Print.followedBy (Print.exactly (moduleName syntaxImportNode.value.moduleName.value))
                                        )
                                    |> Print.followedBy
                                        (Print.withIndentAtNextMultipleOf4
                                            (printLinebreakIndentedAs
                                                |> Print.followedBy
                                                    (Print.withIndentAtNextMultipleOf4
                                                        (Print.linebreakIndented
                                                            |> Print.followedBy (comments (aliasComment0 :: aliasComment1Up))
                                                            |> Print.followedBy Print.linebreakIndented
                                                            |> Print.followedBy
                                                                (Print.exactly (moduleName moduleAliasNode.value))
                                                        )
                                                    )
                                            )
                                        )
                                )
            )
        |> Print.followedBy
            (case syntaxImportNode.value.exposingList of
                Nothing ->
                    Print.empty

                Just syntaxExposing ->
                    let
                        exposingPrint : Print
                        exposingPrint =
                            importExposing syntaxComments syntaxExposing

                        exposingPartStart : GrenSyntax.Location
                        exposingPartStart =
                            case syntaxImportNode.value.moduleAlias of
                                Nothing ->
                                    syntaxImportNode.value.moduleName.range.end

                                Just moduleAliasNode ->
                                    moduleAliasNode.range.end
                    in
                    case commentsInRange { start = exposingPartStart, end = importRange.end } syntaxComments of
                        [] ->
                            let
                                lineSpread : Print.LineSpread
                                lineSpread =
                                    Print.lineSpread exposingPrint
                            in
                            Print.withIndentAtNextMultipleOf4
                                (Print.spaceOrLinebreakIndented lineSpread
                                    |> Print.followedBy printExactlyExposing
                                    |> Print.followedBy
                                        (Print.withIndentAtNextMultipleOf4
                                            (Print.spaceOrLinebreakIndented lineSpread
                                                |> Print.followedBy exposingPrint
                                            )
                                        )
                                )

                        exposingComment0 :: exposingComment1Up ->
                            Print.withIndentAtNextMultipleOf4
                                (printLinebreakIndentedExposing
                                    |> Print.followedBy
                                        (Print.withIndentAtNextMultipleOf4
                                            (Print.linebreakIndented
                                                |> Print.followedBy (comments (exposingComment0 :: exposingComment1Up))
                                                |> Print.followedBy Print.linebreakIndented
                                                |> Print.followedBy exposingPrint
                                            )
                                        )
                                )
            )


{-| Print the stuff after `exposing` in an import.
For module header exposing: [`moduleExposing`](#moduleExposing)
-}
importExposing :
    List (GrenSyntax.Node String)
    -> GrenSyntax.Node GrenSyntax.Exposing
    -> Print
importExposing syntaxComments syntaxExposingNode =
    case syntaxExposingNode.value of
        GrenSyntax.All _ ->
            printExactlyParensOpeningDotDotParensClosing

        GrenSyntax.Explicit exposingSet ->
            case exposingSet of
                -- invalid syntax
                [] ->
                    printExactlyParensOpeningDotDotParensClosing

                expose0 :: expose1Up ->
                    exposingMulti syntaxComments
                        { fullRange = syntaxExposingNode.range
                        , expose0 = expose0
                        , expose1Up = expose1Up
                        }


{-| Print `--` or `{- -}` comments placed _within a declaration_.
For top-level comments: [`moduleLevelComments`](#moduleLevelComments)
-}
comments : List String -> Print
comments syntaxComments =
    syntaxComments
        |> Print.listMapAndIntersperseAndFlatten
            comment
            Print.linebreakIndented


collapsibleComments : List String -> { print : Print, lineSpread : Print.LineSpread }
collapsibleComments commentsToPrint =
    case commentsToPrint of
        [] ->
            printEmptyLineSpreadSingleLine

        comment0 :: comment1Up ->
            let
                commentPrints : List Print
                commentPrints =
                    (comment0 :: comment1Up) |> List.map comment
            in
            if
                commentPrints
                    |> List.all
                        (\commentPrint ->
                            commentPrint |> Print.toString |> commentCanBePartOfCollapsible
                        )
            then
                { print =
                    commentPrints
                        |> Print.listIntersperseAndFlatten printExactlySpace
                , lineSpread = Print.SingleLine
                }

            else
                { print = comments (comment0 :: comment1Up)
                , lineSpread = Print.MultipleLines
                }


printEmptyLineSpreadSingleLine : { print : Print.Print, lineSpread : Print.LineSpread }
printEmptyLineSpreadSingleLine =
    { print = Print.empty, lineSpread = Print.SingleLine }


commentCanBePartOfCollapsible : String -> Bool
commentCanBePartOfCollapsible syntaxComment =
    case syntaxComment of
        "{--}" ->
            False

        commentNotDirectlyClosed ->
            (commentNotDirectlyClosed |> String.startsWith "{-")
                && Basics.not (commentNotDirectlyClosed |> String.contains "\n")


{-| Print `--` or `{- -}` comments placed outside of declarations at the top level.
For comments within a declaration: [`comments`](#comments)
-}
moduleLevelComments : List String -> Print
moduleLevelComments syntaxComments =
    case syntaxComments of
        [] ->
            Print.empty

        comment0 :: comment1Up ->
            comment comment0
                |> Print.followedBy Print.linebreak
                |> Print.followedBy
                    (comment1Up
                        |> Print.listMapAndFlatten
                            (\syntaxComment ->
                                case syntaxComment of
                                    "{--}" ->
                                        moduleLevelMultiLineCommentWithoutWhitespace

                                    notEmptyMultiLineComment ->
                                        comment notEmptyMultiLineComment
                                            |> Print.followedBy Print.linebreak
                            )
                    )


moduleLevelMultiLineCommentWithoutWhitespace : Print.Print
moduleLevelMultiLineCommentWithoutWhitespace =
    printLinebreakLinebreak
        |> Print.followedBy printExactlyCurlyOpeningDotDotCurlyClosing
        |> Print.followedBy Print.linebreak


{-| Print a single `--` or `{- -}` comment.
-}
comment : String -> Print
comment syntaxComment =
    case syntaxComment of
        "{--}" ->
            printExactlyCurlyOpeningDotDotCurlyClosing

        nonDirectlyClosingMultiLineComment ->
            if nonDirectlyClosingMultiLineComment |> String.startsWith "--" then
                Print.exactly (nonDirectlyClosingMultiLineComment |> String.trimRight)

            else
                -- comment starts with {-
                let
                    commentContentLines : List String
                    commentContentLines =
                        nonDirectlyClosingMultiLineComment
                            |> -- {-
                               String.dropLeft 2
                            |> -- -}
                               String.dropRight 2
                            |> String.lines

                    commentContentNormal : List String
                    commentContentNormal =
                        case commentContentLines of
                            [] ->
                                []

                            commentContentLine0 :: commentContentLine1Up ->
                                (commentContentLine0 |> String.trimLeft)
                                    :: (commentContentLine1Up
                                            |> listDropLastIfIs
                                                (\line ->
                                                    case line |> String.trim of
                                                        "" ->
                                                            True

                                                        _ ->
                                                            False
                                                )
                                            |> unindent
                                       )
                                    |> List.map String.trimRight
                in
                printExactlyCurlyOpeningMinus
                    |> Print.followedBy
                        -- if original commentContent contains >= 2 lines, keep but
                        (case commentContentNormal of
                            -- only spaces
                            [] ->
                                printExactlySpaceSpace

                            [ singleLine ] ->
                                Print.exactly
                                    (" " ++ singleLine ++ " ")

                            firstLine :: secondLine :: thirdLineUp ->
                                (case firstLine of
                                    "" ->
                                        Print.linebreakIndented

                                    lineNotEmpty ->
                                        Print.exactly (" " ++ lineNotEmpty)
                                            |> Print.followedBy Print.linebreakIndented
                                )
                                    |> Print.followedBy
                                        ((secondLine :: thirdLineUp)
                                            |> Print.listMapAndFlatten
                                                (\line ->
                                                    case line of
                                                        "" ->
                                                            Print.linebreakIndented

                                                        lineNotEmpty ->
                                                            Print.exactly ("   " ++ lineNotEmpty)
                                                                |> Print.followedBy
                                                                    Print.linebreakIndented
                                                )
                                        )
                        )
                    |> Print.followedBy printExactlyMinusCurlyClosing


unindent : List String -> List String
unindent lines =
    let
        nonBlankLines : List String
        nonBlankLines =
            lines
                |> List.filterMap
                    (\line ->
                        case line |> String.trim of
                            "" ->
                                Nothing

                            _ ->
                                Just line
                    )
    in
    case nonBlankLines |> List.map lineIndentation |> List.minimum of
        Nothing ->
            lines

        Just minimumIndentation ->
            lines
                |> List.map (\line -> line |> String.dropLeft minimumIndentation)


lineIndentation : String -> Int
lineIndentation line =
    line
        |> String.foldl
            (\char soFar ->
                if soFar.onlySpaces then
                    case char of
                        ' ' ->
                            { spaceCount = soFar.spaceCount + 1, onlySpaces = True }

                        _ ->
                            { spaceCount = soFar.spaceCount, onlySpaces = False }

                else
                    soFar
            )
            spaceCount0OnlySpacesTrue
        |> .spaceCount


spaceCount0OnlySpacesTrue : { spaceCount : Int, onlySpaces : Bool }
spaceCount0OnlySpacesTrue =
    { spaceCount = 0, onlySpaces = True }


listDropLastIfIs : (a -> Bool) -> List a -> List a
listDropLastIfIs lastElementShouldBeRemoved list =
    case list of
        [] ->
            []

        [ onlyElement ] ->
            if onlyElement |> lastElementShouldBeRemoved then
                []

            else
                [ onlyElement ]

        element0 :: element1 :: element2Up ->
            element0 :: listDropLastIfIs lastElementShouldBeRemoved (element1 :: element2Up)


{-| Print an [`GrenSyntax.ModuleName`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-ModuleName#ModuleName)
-}
moduleName : GrenSyntax.ModuleName -> String
moduleName syntaxModuleName =
    syntaxModuleName |> String.join "."


{-| Print a single [`GrenSyntax.TopLevelExpose`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Exposing#TopLevelExpose)
-}
expose : GrenSyntax.TopLevelExpose -> String
expose syntaxExpose =
    case syntaxExpose of
        GrenSyntax.InfixExpose operatorSymbol ->
            "(" ++ operatorSymbol ++ ")"

        GrenSyntax.FunctionExpose name ->
            name

        GrenSyntax.TypeOrAliasExpose name ->
            name

        GrenSyntax.TypeExpose syntaxExposeType ->
            case syntaxExposeType.open of
                Nothing ->
                    syntaxExposeType.name

                Just _ ->
                    syntaxExposeType.name ++ "(..)"


patternParenthesizedIfSpaceSeparated :
    List (GrenSyntax.Node String)
    -> GrenSyntax.Node GrenSyntax.Pattern
    -> Print
patternParenthesizedIfSpaceSeparated syntaxComments syntaxPattern =
    if patternIsSpaceSeparated (syntaxPattern |> GrenSyntax.nodeValue) then
        patternParenthesized syntaxComments syntaxPattern

    else
        patternNotParenthesized syntaxComments syntaxPattern


patternParenthesized :
    List (GrenSyntax.Node String)
    -> GrenSyntax.Node GrenSyntax.Pattern
    -> Print
patternParenthesized syntaxComments patternNode =
    parenthesized patternNotParenthesized
        { fullRange = patternNode |> GrenSyntax.nodeRange
        , notParenthesized = patternNode |> patternToNotParenthesized
        }
        syntaxComments


patternIsSpaceSeparated : GrenSyntax.Pattern -> Bool
patternIsSpaceSeparated syntaxPattern =
    case syntaxPattern of
        GrenSyntax.PatternIgnored _ ->
            False

        GrenSyntax.PatternUnit ->
            False

        GrenSyntax.PatternVariable _ ->
            False

        GrenSyntax.PatternChar _ ->
            False

        GrenSyntax.PatternString _ ->
            False

        GrenSyntax.PatternInt _ ->
            False

        GrenSyntax.PatternHex _ ->
            False

        GrenSyntax.PatternParenthesized inParensNode ->
            patternIsSpaceSeparated inParensNode.value

        GrenSyntax.PatternRecord _ ->
            False

        GrenSyntax.PatternListCons _ ->
            True

        GrenSyntax.PatternListExact _ ->
            False

        GrenSyntax.PatternVariant syntaxPatternVariant ->
            case syntaxPatternVariant.value of
                Nothing ->
                    False

                Just _ ->
                    True

        GrenSyntax.PatternAs _ ->
            True


stringLiteral : { content : String, quotingStyle : GrenSyntax.StringQuotingStyle } -> Print
stringLiteral string =
    case string.quotingStyle of
        GrenSyntax.StringTripleQuoted ->
            printExactlyDoubleQuoteDoubleQuoteDoubleQuote
                |> Print.followedBy
                    (string.content
                        |> String.foldl
                            (\contentChar soFar ->
                                soFar ++ tripleDoubleQuotedStringCharToEscaped contentChar ++ ""
                            )
                            ""
                        |> tripleDoubleQuotedStringEscapeDoubleQuotes
                        |> String.lines
                        |> Print.listMapAndIntersperseAndFlatten
                            Print.exactly
                            Print.linebreak
                    )
                |> Print.followedBy printExactlyDoubleQuoteDoubleQuoteDoubleQuote

        GrenSyntax.StringSingleQuoted ->
            let
                singleDoubleQuotedStringContentEscaped : String
                singleDoubleQuotedStringContentEscaped =
                    string.content
                        |> String.foldl
                            (\contentChar soFar ->
                                soFar ++ singleDoubleQuotedStringCharToEscaped contentChar ++ ""
                            )
                            ""
            in
            Print.exactly ("\"" ++ singleDoubleQuotedStringContentEscaped ++ "\"")


tripleDoubleQuotedStringEscapeDoubleQuotes : String -> String
tripleDoubleQuotedStringEscapeDoubleQuotes string =
    let
        beforeLastCharEscaped : { consecutiveDoubleQuoteCount : Int, result : String }
        beforeLastCharEscaped =
            -- escape continuous double quotes if combined length >= 3
            string
                |> String.foldl
                    (\char soFar ->
                        case char of
                            '"' ->
                                { consecutiveDoubleQuoteCount =
                                    soFar.consecutiveDoubleQuoteCount + 1
                                , result = soFar.result
                                }

                            firstCharNotDoubleQuote ->
                                { consecutiveDoubleQuoteCount = 0
                                , result =
                                    soFar.result
                                        ++ (case soFar.consecutiveDoubleQuoteCount of
                                                0 ->
                                                    ""

                                                1 ->
                                                    "\""

                                                2 ->
                                                    "\"\""

                                                atLeast3ConsecutiveDoubleQuoteCount ->
                                                    String.repeat atLeast3ConsecutiveDoubleQuoteCount "\\\""
                                           )
                                        ++ String.fromChar firstCharNotDoubleQuote
                                        ++ ""
                                }
                    )
                    { consecutiveDoubleQuoteCount = 0
                    , result = ""
                    }
    in
    beforeLastCharEscaped.result
        ++ (-- escape preceding continuous double quotes if they connect to the last char double quote
            String.repeat beforeLastCharEscaped.consecutiveDoubleQuoteCount "\\\""
           )
        ++ ""


singleDoubleQuotedStringCharToEscaped : Char -> String
singleDoubleQuotedStringCharToEscaped character =
    case character of
        '"' ->
            "\\\""

        '\\' ->
            "\\\\"

        '\t' ->
            "\\t"

        '\n' ->
            "\\n"

        '\u{000D}' ->
            "\\u{000D}"

        otherCharacter ->
            if characterIsNotPrint otherCharacter then
                "\\u{" ++ characterHex otherCharacter ++ "}"

            else
                String.fromChar otherCharacter


tripleDoubleQuotedStringCharToEscaped : Char -> String
tripleDoubleQuotedStringCharToEscaped character =
    case character of
        '"' ->
            -- edge cases handled in a later step
            "\""

        '\\' ->
            "\\\\"

        '\t' ->
            "\\t"

        '\n' ->
            "\n"

        '\u{000D}' ->
            "\u{000D}"

        otherCharacter ->
            if characterIsNotPrint otherCharacter then
                "\\u{" ++ characterHex otherCharacter ++ "}"

            else
                String.fromChar otherCharacter


intToHexString : Int -> String
intToHexString int =
    if int < 16 then
        unsafeHexDigitIntToString int

    else
        intToHexString (int // 16)
            ++ unsafeHexDigitIntToString (int |> Basics.remainderBy 16)
            ++ ""


unsafeHexDigitIntToString : Int -> String
unsafeHexDigitIntToString int =
    case int of
        0 ->
            "0"

        1 ->
            "1"

        2 ->
            "2"

        3 ->
            "3"

        4 ->
            "4"

        5 ->
            "5"

        6 ->
            "6"

        7 ->
            "7"

        8 ->
            "8"

        9 ->
            "9"

        10 ->
            "A"

        11 ->
            "B"

        12 ->
            "C"

        13 ->
            "D"

        14 ->
            "E"

        -- 15
        _ ->
            "F"


characterHex : Char -> String
characterHex character =
    let
        charCode : Int
        charCode =
            Char.toCode character
    in
    String.toUpper
        (unsafeHexDigitIntToString
            (charCode
                |> Bitwise.and 0xF000
                |> Bitwise.shiftRightBy 12
            )
            ++ unsafeHexDigitIntToString
                (charCode
                    |> Bitwise.and 0x0F00
                    |> Bitwise.shiftRightBy 8
                )
            ++ unsafeHexDigitIntToString
                (charCode
                    |> Bitwise.and 0xF0
                    |> Bitwise.shiftRightBy 4
                )
            ++ unsafeHexDigitIntToString (charCode |> Bitwise.and 0x0F)
            ++ ""
        )


characterIsNotPrint : Char -> Bool
characterIsNotPrint character =
    if
        -- Unicode.getCategory is very expensive so we shortcut if at all possible
        Unicode.isLatinAlphaNumOrUnderscoreFast character
            || (case character of
                    ' ' ->
                        True

                    '.' ->
                        True

                    '!' ->
                        True

                    '?' ->
                        True

                    '-' ->
                        True

                    ':' ->
                        True

                    _ ->
                        False
               )
    then
        False

    else
        case Unicode.getCategory character of
            Nothing ->
                True

            Just category ->
                case category of
                    Unicode.SeparatorLine ->
                        True

                    Unicode.SeparatorParagraph ->
                        True

                    Unicode.OtherControl ->
                        True

                    Unicode.OtherFormat ->
                        True

                    Unicode.OtherSurrogate ->
                        True

                    Unicode.OtherPrivateUse ->
                        True

                    Unicode.LetterUppercase ->
                        False

                    Unicode.LetterLowercase ->
                        False

                    Unicode.LetterTitlecase ->
                        False

                    Unicode.MarkNonSpacing ->
                        False

                    Unicode.MarkSpacingCombining ->
                        False

                    Unicode.MarkEnclosing ->
                        False

                    Unicode.NumberDecimalDigit ->
                        False

                    Unicode.NumberLetter ->
                        False

                    Unicode.NumberOther ->
                        False

                    Unicode.SeparatorSpace ->
                        True

                    Unicode.LetterModifier ->
                        False

                    Unicode.LetterOther ->
                        False

                    Unicode.PunctuationConnector ->
                        False

                    Unicode.PunctuationDash ->
                        False

                    Unicode.PunctuationOpen ->
                        False

                    Unicode.PunctuationClose ->
                        False

                    Unicode.PunctuationInitialQuote ->
                        False

                    Unicode.PunctuationFinalQuote ->
                        False

                    Unicode.PunctuationOther ->
                        False

                    Unicode.SymbolMath ->
                        False

                    Unicode.SymbolCurrency ->
                        False

                    Unicode.SymbolModifier ->
                        False

                    Unicode.SymbolOther ->
                        False


charLiteral : Char -> String
charLiteral charContent =
    "'"
        ++ quotedCharToEscaped charContent
        ++ "'"


quotedCharToEscaped : Char -> String
quotedCharToEscaped character =
    case character of
        '\'' ->
            "\\'"

        '\\' ->
            "\\\\"

        '\t' ->
            "\\t"

        '\n' ->
            "\\n"

        '\u{000D}' ->
            "\\u{000D}"

        otherCharacter ->
            if characterIsNotPrint otherCharacter then
                "\\u{" ++ characterHex otherCharacter ++ "}"

            else
                String.fromChar otherCharacter


intLiteral : Int -> String
intLiteral =
    String.fromInt


hexLiteral : Int -> String
hexLiteral int =
    let
        maybeSignPrint : String
        maybeSignPrint =
            if int < 0 then
                "-"

            else
                ""

        intAbs : Int
        intAbs =
            int |> Basics.abs

        digitCountToPrint : Int
        digitCountToPrint =
            if intAbs <= 0xFF then
                2

            else if intAbs <= 0xFFFF then
                4

            else if intAbs <= 0xFFFFFFFF then
                8

            else
                16
    in
    maybeSignPrint
        ++ "0x"
        ++ (intToHexString int
                |> stringResizePadLeftWith0s digitCountToPrint
           )


stringResizePadLeftWith0s : Int -> String -> String
stringResizePadLeftWith0s length unpaddedString =
    if length < (unpaddedString |> String.length) then
        String.left length unpaddedString

    else
        String.repeat (length - (unpaddedString |> String.length)) "0"
            ++ unpaddedString
            ++ ""


patternToNotParenthesized :
    GrenSyntax.Node GrenSyntax.Pattern
    -> GrenSyntax.Node GrenSyntax.Pattern
patternToNotParenthesized syntaxPatternNode =
    -- IGNORE TCO
    case syntaxPatternNode.value of
        GrenSyntax.PatternParenthesized inParens ->
            inParens |> patternToNotParenthesized

        GrenSyntax.PatternIgnored maybeName ->
            { range = syntaxPatternNode.range, value = GrenSyntax.PatternIgnored maybeName }

        GrenSyntax.PatternUnit ->
            { range = syntaxPatternNode.range, value = GrenSyntax.PatternUnit }

        GrenSyntax.PatternVariable name ->
            { range = syntaxPatternNode.range, value = GrenSyntax.PatternVariable name }

        GrenSyntax.PatternChar char ->
            { range = syntaxPatternNode.range, value = GrenSyntax.PatternChar char }

        GrenSyntax.PatternString string ->
            { range = syntaxPatternNode.range, value = GrenSyntax.PatternString string }

        GrenSyntax.PatternInt int ->
            { range = syntaxPatternNode.range, value = GrenSyntax.PatternInt int }

        GrenSyntax.PatternHex int ->
            { range = syntaxPatternNode.range, value = GrenSyntax.PatternHex int }

        GrenSyntax.PatternRecord fields ->
            { range = syntaxPatternNode.range, value = GrenSyntax.PatternRecord fields }

        GrenSyntax.PatternListCons listCons ->
            { range = syntaxPatternNode.range, value = GrenSyntax.PatternListCons listCons }

        GrenSyntax.PatternListExact elementPatterns ->
            { range = syntaxPatternNode.range, value = GrenSyntax.PatternListExact elementPatterns }

        GrenSyntax.PatternVariant syntaxPatternVariant ->
            { range = syntaxPatternNode.range, value = GrenSyntax.PatternVariant syntaxPatternVariant }

        GrenSyntax.PatternAs syntaxPatternAs ->
            { range = syntaxPatternNode.range, value = GrenSyntax.PatternAs syntaxPatternAs }


{-| Print an [`GrenSyntax.Pattern`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Pattern#Pattern)
-}
patternNotParenthesized :
    List (GrenSyntax.Node String)
    -> GrenSyntax.Node GrenSyntax.Pattern
    -> Print
patternNotParenthesized syntaxComments syntaxPatternNode =
    -- IGNORE TCO
    case syntaxPatternNode.value of
        GrenSyntax.PatternIgnored maybeName ->
            case maybeName of
                Nothing ->
                    printExactlyUnderscore

                Just name ->
                    Print.exactly ("_" ++ name)

        GrenSyntax.PatternUnit ->
            printExactlyCurlyBraceOpeningCurlyBraceClosing

        GrenSyntax.PatternVariable name ->
            Print.exactly name

        GrenSyntax.PatternChar char ->
            Print.exactly (charLiteral char)

        GrenSyntax.PatternString string ->
            stringLiteral string

        GrenSyntax.PatternInt int ->
            Print.exactly (intLiteral int)

        GrenSyntax.PatternHex int ->
            Print.exactly (hexLiteral int)

        GrenSyntax.PatternParenthesized inParens ->
            let
                commentsBeforeInParens : List String
                commentsBeforeInParens =
                    commentsInRange
                        { start = syntaxPatternNode.range.start
                        , end = inParens |> GrenSyntax.nodeRange |> .start
                        }
                        syntaxComments

                commentsAfterInParens : List String
                commentsAfterInParens =
                    commentsInRange
                        { start = inParens |> GrenSyntax.nodeRange |> .end
                        , end = syntaxPatternNode.range.end
                        }
                        syntaxComments
            in
            case ( commentsBeforeInParens, commentsAfterInParens ) of
                ( [], [] ) ->
                    patternNotParenthesized syntaxComments inParens

                _ ->
                    parenthesized patternNotParenthesized
                        { notParenthesized = inParens |> patternToNotParenthesized
                        , fullRange = syntaxPatternNode.range
                        }
                        syntaxComments

        GrenSyntax.PatternRecord fields ->
            patternRecord syntaxComments
                { fullRange = syntaxPatternNode.range, fields = fields }

        GrenSyntax.PatternListCons listCons ->
            patternCons syntaxComments listCons

        GrenSyntax.PatternListExact elementPatterns ->
            patternList syntaxComments
                { fullRange = syntaxPatternNode.range, elements = elementPatterns }

        GrenSyntax.PatternVariant syntaxPatternVariant ->
            construct
                { printArgumentParenthesizedIfSpaceSeparated =
                    patternParenthesizedIfSpaceSeparated
                , lineSpreadMinimum = Print.SingleLine
                }
                syntaxComments
                { fullRange = syntaxPatternNode.range
                , start =
                    qualifiedReference
                        { qualification = syntaxPatternVariant.qualification
                        , name = syntaxPatternVariant.name
                        }
                , arguments =
                    case syntaxPatternVariant.value of
                        Nothing ->
                            []

                        Just value ->
                            [ value ]
                }

        GrenSyntax.PatternAs syntaxPatternAs ->
            patternAs syntaxComments syntaxPatternAs


patternList :
    List (GrenSyntax.Node String)
    ->
        { elements : List (GrenSyntax.Node GrenSyntax.Pattern)
        , fullRange : GrenSyntax.Range
        }
    -> Print
patternList syntaxComments syntaxList =
    case syntaxList.elements of
        [] ->
            printExactlySquareOpening
                |> Print.followedBy
                    (case commentsInRange syntaxList.fullRange syntaxComments of
                        [] ->
                            printExactlySquareClosing

                        comment0 :: comment1Up ->
                            let
                                commentsCollapsed : { print : Print, lineSpread : Print.LineSpread }
                                commentsCollapsed =
                                    collapsibleComments (comment0 :: comment1Up)
                            in
                            Print.withIndentIncreasedBy 1
                                commentsCollapsed.print
                                |> Print.followedBy
                                    (Print.emptyOrLinebreakIndented
                                        commentsCollapsed.lineSpread
                                    )
                                |> Print.followedBy printExactlySquareClosing
                    )

        element0 :: element1Up ->
            let
                elementPrintsWithCommentsBefore :
                    { endLocation : GrenSyntax.Location
                    , reverse : List Print
                    }
                elementPrintsWithCommentsBefore =
                    (element0 :: element1Up)
                        |> List.foldl
                            (\elementNode soFar ->
                                let
                                    elementPrint : Print
                                    elementPrint =
                                        patternNotParenthesized syntaxComments
                                            elementNode
                                in
                                { endLocation = elementNode.range.end
                                , reverse =
                                    (case
                                        commentsInRange
                                            { start = soFar.endLocation
                                            , end = elementNode.range.start
                                            }
                                            syntaxComments
                                     of
                                        [] ->
                                            elementPrint

                                        comment0 :: comment1Up ->
                                            let
                                                commentsBeforeElement : { print : Print, lineSpread : Print.LineSpread }
                                                commentsBeforeElement =
                                                    collapsibleComments (comment0 :: comment1Up)
                                            in
                                            commentsBeforeElement.print
                                                |> Print.followedBy
                                                    (Print.spaceOrLinebreakIndented
                                                        (commentsBeforeElement.lineSpread
                                                            |> Print.lineSpreadMergeWith
                                                                (\() -> elementPrint |> Print.lineSpread)
                                                        )
                                                    )
                                                |> Print.followedBy elementPrint
                                    )
                                        :: soFar.reverse
                                }
                            )
                            { endLocation = syntaxList.fullRange.start
                            , reverse = []
                            }

                commentsAfterElements : Maybe { print : Print, lineSpread : Print.LineSpread }
                commentsAfterElements =
                    case
                        commentsInRange { start = elementPrintsWithCommentsBefore.endLocation, end = syntaxList.fullRange.end }
                            syntaxComments
                    of
                        [] ->
                            Nothing

                        comment0 :: comment1Up ->
                            Just (collapsibleComments (comment0 :: comment1Up))

                lineSpread : Print.LineSpread
                lineSpread =
                    elementPrintsWithCommentsBefore.reverse
                        |> Print.lineSpreadListMapAndCombine Print.lineSpread
                        |> Print.lineSpreadMergeWith
                            (\() -> commentsAfterElements |> maybeLineSpread .lineSpread)
            in
            printExactlySquareOpeningSpace
                |> Print.followedBy
                    (elementPrintsWithCommentsBefore.reverse
                        |> Print.listReverseAndMapAndIntersperseAndFlatten
                            (\elementPrintWithCommentsBefore ->
                                Print.withIndentIncreasedBy 2
                                    elementPrintWithCommentsBefore
                            )
                            (Print.emptyOrLinebreakIndented lineSpread
                                |> Print.followedBy printExactlyCommaSpace
                            )
                    )
                |> Print.followedBy
                    (case commentsAfterElements of
                        Nothing ->
                            Print.spaceOrLinebreakIndented lineSpread

                        Just commentsCollapsibleAfterElements ->
                            Print.withIndentIncreasedBy 2
                                (Print.spaceOrLinebreakIndented lineSpread
                                    |> Print.followedBy commentsCollapsibleAfterElements.print
                                )
                                |> Print.followedBy (Print.spaceOrLinebreakIndented lineSpread)
                    )
                |> Print.followedBy printExactlySquareClosing


maybeLineSpread : (a -> Print.LineSpread) -> Maybe a -> Print.LineSpread
maybeLineSpread valueToLineSpread maybe =
    case maybe of
        Nothing ->
            Print.SingleLine

        Just value ->
            value |> valueToLineSpread


patternCons :
    List (GrenSyntax.Node String)
    ->
        { head : GrenSyntax.Node GrenSyntax.Pattern
        , tail : GrenSyntax.Node GrenSyntax.Pattern
        }
    -> Print
patternCons syntaxComments syntaxCons =
    let
        headPrint : Print
        headPrint =
            patternParenthesizedIfSpaceSeparated syntaxComments syntaxCons.head

        tailPatterns : List (GrenSyntax.Node GrenSyntax.Pattern)
        tailPatterns =
            syntaxCons.tail |> patternConsExpand

        tailPatternPrintsAndCommentsBeforeReverse : List Print
        tailPatternPrintsAndCommentsBeforeReverse =
            tailPatterns
                |> List.foldl
                    (\tailPatternNode soFar ->
                        let
                            print : Print
                            print =
                                patternParenthesizedIfSpaceSeparated syntaxComments
                                    tailPatternNode
                        in
                        { reverse =
                            (case
                                commentsInRange
                                    { start = soFar.endLocation
                                    , end = tailPatternNode.range.start
                                    }
                                    syntaxComments
                             of
                                [] ->
                                    print

                                comment0 :: comment1Up ->
                                    let
                                        commentsBefore : { print : Print, lineSpread : Print.LineSpread }
                                        commentsBefore =
                                            collapsibleComments (comment0 :: comment1Up)
                                    in
                                    commentsBefore.print
                                        |> Print.followedBy
                                            (Print.spaceOrLinebreakIndented
                                                (commentsBefore.lineSpread
                                                    |> Print.lineSpreadMergeWith
                                                        (\() -> print |> Print.lineSpread)
                                                )
                                            )
                                        |> Print.followedBy print
                            )
                                :: soFar.reverse
                        , endLocation = tailPatternNode.range.end
                        }
                    )
                    { reverse = []
                    , endLocation = syntaxCons.head |> GrenSyntax.nodeRange |> .end
                    }
                |> .reverse

        lineSpread : Print.LineSpread
        lineSpread =
            headPrint
                |> Print.lineSpread
                |> Print.lineSpreadMergeWith
                    (\() ->
                        tailPatternPrintsAndCommentsBeforeReverse
                            |> Print.lineSpreadListMapAndCombine Print.lineSpread
                    )
    in
    headPrint
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.spaceOrLinebreakIndented lineSpread
                    |> Print.followedBy
                        (tailPatternPrintsAndCommentsBeforeReverse
                            |> Print.listReverseAndMapAndIntersperseAndFlatten
                                (\tailPatternElementPrintWithCommentsBefore ->
                                    printExactlyColonColonSpace
                                        |> Print.followedBy
                                            (Print.withIndentIncreasedBy 3
                                                tailPatternElementPrintWithCommentsBefore
                                            )
                                )
                                (Print.spaceOrLinebreakIndented lineSpread)
                        )
                )
            )


patternConsExpand :
    GrenSyntax.Node GrenSyntax.Pattern
    -> List (GrenSyntax.Node GrenSyntax.Pattern)
patternConsExpand syntaxPatternNode =
    case syntaxPatternNode.value of
        GrenSyntax.PatternListCons listCons ->
            listCons.head :: patternConsExpand listCons.tail

        GrenSyntax.PatternIgnored maybeName ->
            [ { range = syntaxPatternNode.range, value = GrenSyntax.PatternIgnored maybeName } ]

        GrenSyntax.PatternUnit ->
            [ { range = syntaxPatternNode.range, value = GrenSyntax.PatternUnit } ]

        GrenSyntax.PatternChar char ->
            [ { range = syntaxPatternNode.range, value = GrenSyntax.PatternChar char } ]

        GrenSyntax.PatternString string ->
            [ { range = syntaxPatternNode.range, value = GrenSyntax.PatternString string } ]

        GrenSyntax.PatternInt int ->
            [ { range = syntaxPatternNode.range, value = GrenSyntax.PatternInt int } ]

        GrenSyntax.PatternHex int ->
            [ { range = syntaxPatternNode.range, value = GrenSyntax.PatternHex int } ]

        GrenSyntax.PatternRecord fields ->
            [ { range = syntaxPatternNode.range, value = GrenSyntax.PatternRecord fields } ]

        GrenSyntax.PatternListExact elements ->
            [ { range = syntaxPatternNode.range, value = GrenSyntax.PatternListExact elements } ]

        GrenSyntax.PatternVariable variableName ->
            [ { range = syntaxPatternNode.range, value = GrenSyntax.PatternVariable variableName } ]

        GrenSyntax.PatternVariant syntaxPatternVariant ->
            [ { range = syntaxPatternNode.range, value = GrenSyntax.PatternVariant syntaxPatternVariant } ]

        GrenSyntax.PatternAs syntaxPatternAs ->
            [ { range = syntaxPatternNode.range, value = GrenSyntax.PatternAs syntaxPatternAs } ]

        GrenSyntax.PatternParenthesized inParens ->
            [ { range = syntaxPatternNode.range, value = GrenSyntax.PatternParenthesized inParens } ]


patternAs :
    List (GrenSyntax.Node String)
    ->
        { variable : GrenSyntax.Node String
        , pattern : GrenSyntax.Node GrenSyntax.Pattern
        }
    -> Print
patternAs syntaxComments syntaxAs =
    let
        aliasedPatternPrint : Print
        aliasedPatternPrint =
            patternParenthesizedIfSpaceSeparated syntaxComments syntaxAs.pattern

        commentsBeforeAliasName : List String
        commentsBeforeAliasName =
            commentsInRange
                { start = syntaxAs.pattern |> GrenSyntax.nodeRange |> .end
                , end = syntaxAs.variable |> GrenSyntax.nodeRange |> .start
                }
                syntaxComments

        commentsCollapsibleBeforeAliasName : { print : Print, lineSpread : Print.LineSpread }
        commentsCollapsibleBeforeAliasName =
            collapsibleComments commentsBeforeAliasName

        lineSpread : Print.LineSpread
        lineSpread =
            commentsCollapsibleBeforeAliasName.lineSpread
                |> Print.lineSpreadMergeWith
                    (\() -> aliasedPatternPrint |> Print.lineSpread)

        namePrint : Print
        namePrint =
            Print.exactly (syntaxAs.variable |> GrenSyntax.nodeValue)
    in
    aliasedPatternPrint
        |> Print.followedBy (Print.spaceOrLinebreakIndented lineSpread)
        |> Print.followedBy printExactlyAs
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.spaceOrLinebreakIndented lineSpread
                    |> Print.followedBy
                        (case commentsBeforeAliasName of
                            [] ->
                                namePrint

                            _ :: _ ->
                                commentsCollapsibleBeforeAliasName.print
                                    |> Print.followedBy
                                        (Print.spaceOrLinebreakIndented
                                            commentsCollapsibleBeforeAliasName.lineSpread
                                        )
                                    |> Print.followedBy namePrint
                        )
                )
            )


patternRecord :
    List (GrenSyntax.Node String)
    ->
        { fields :
            List
                { name : GrenSyntax.Node String
                , value : Maybe (GrenSyntax.Node GrenSyntax.Pattern)
                }
        , fullRange : GrenSyntax.Range
        }
    -> Print
patternRecord syntaxComments syntaxRecord =
    case syntaxRecord.fields of
        [] ->
            printExactlyCurlyOpening
                |> Print.followedBy
                    (case commentsInRange syntaxRecord.fullRange syntaxComments of
                        [] ->
                            printExactlyCurlyClosing

                        comment0 :: comment1Up ->
                            let
                                commentsCollapsed : { print : Print, lineSpread : Print.LineSpread }
                                commentsCollapsed =
                                    collapsibleComments (comment0 :: comment1Up)
                            in
                            Print.withIndentIncreasedBy 1
                                commentsCollapsed.print
                                |> Print.followedBy
                                    (Print.emptyOrLinebreakIndented
                                        commentsCollapsed.lineSpread
                                    )
                                |> Print.followedBy printExactlyCurlyClosing
                    )

        field0 :: field1Up ->
            let
                fieldPrintsWithCommentsBefore :
                    { endLocation : GrenSyntax.Location
                    , reverse : List Print
                    }
                fieldPrintsWithCommentsBefore =
                    (field0 :: field1Up)
                        |> List.foldl
                            (\field soFar ->
                                case field.value of
                                    Nothing ->
                                        { endLocation =
                                            field.name |> GrenSyntax.nodeRange |> .end
                                        , reverse =
                                            (case commentsInRange { start = soFar.endLocation, end = field.name |> GrenSyntax.nodeRange |> .start } syntaxComments of
                                                [] ->
                                                    Print.exactly (field.name |> GrenSyntax.nodeValue)

                                                comment0 :: comment1Up ->
                                                    let
                                                        commentsBefore : { print : Print, lineSpread : Print.LineSpread }
                                                        commentsBefore =
                                                            collapsibleComments (comment0 :: comment1Up)
                                                    in
                                                    commentsBefore.print
                                                        |> Print.followedBy
                                                            (Print.spaceOrLinebreakIndented commentsBefore.lineSpread)
                                                        |> Print.followedBy (Print.exactly (field.name |> GrenSyntax.nodeValue))
                                            )
                                                :: soFar.reverse
                                        }

                                    Just fieldValueNode ->
                                        let
                                            fieldValuePrint : Print
                                            fieldValuePrint =
                                                fieldValueNode
                                                    |> patternNotParenthesized syntaxComments
                                        in
                                        { endLocation = fieldValueNode.range.end
                                        , reverse =
                                            (case commentsInRange { start = soFar.endLocation, end = fieldValueNode.range.start } syntaxComments of
                                                [] ->
                                                    Print.exactly ((field.name |> GrenSyntax.nodeValue) ++ " =")
                                                        |> Print.followedBy
                                                            (Print.withIndentAtNextMultipleOf4
                                                                (Print.spaceOrLinebreakIndented (fieldValuePrint |> Print.lineSpread)
                                                                    |> Print.followedBy fieldValuePrint
                                                                )
                                                            )

                                                comment0 :: comment1Up ->
                                                    let
                                                        commentsBefore : { print : Print, lineSpread : Print.LineSpread }
                                                        commentsBefore =
                                                            collapsibleComments (comment0 :: comment1Up)

                                                        fieldLineSpread : Print.LineSpread
                                                        fieldLineSpread =
                                                            commentsBefore.lineSpread
                                                                |> Print.lineSpreadMergeWith
                                                                    (\() -> fieldValuePrint |> Print.lineSpread)
                                                    in
                                                    Print.exactly ((field.name |> GrenSyntax.nodeValue) ++ " =")
                                                        |> Print.followedBy
                                                            (Print.withIndentAtNextMultipleOf4
                                                                (Print.spaceOrLinebreakIndented fieldLineSpread
                                                                    |> Print.followedBy commentsBefore.print
                                                                    |> Print.followedBy
                                                                        (Print.spaceOrLinebreakIndented fieldLineSpread)
                                                                    |> Print.followedBy fieldValuePrint
                                                                )
                                                            )
                                            )
                                                :: soFar.reverse
                                        }
                            )
                            { endLocation = syntaxRecord.fullRange.start
                            , reverse = []
                            }

                maybeCommentsAfterFields : Maybe { print : Print, lineSpread : Print.LineSpread }
                maybeCommentsAfterFields =
                    case
                        commentsInRange
                            { start = fieldPrintsWithCommentsBefore.endLocation
                            , end = syntaxRecord.fullRange.end
                            }
                            syntaxComments
                    of
                        [] ->
                            Nothing

                        comment0 :: comment1Up ->
                            -- yes, in record patterns trailing comments
                            -- are indented and not separated with an extra linebreak
                            Just (collapsibleComments (comment0 :: comment1Up))

                lineSpread : Print.LineSpread
                lineSpread =
                    fieldPrintsWithCommentsBefore.reverse
                        |> Print.lineSpreadListMapAndCombine Print.lineSpread
                        |> Print.lineSpreadMergeWith
                            (\() -> maybeCommentsAfterFields |> maybeLineSpread .lineSpread)
            in
            printExactlyCurlyOpeningSpace
                |> Print.followedBy
                    (fieldPrintsWithCommentsBefore.reverse
                        |> Print.listReverseAndMapAndIntersperseAndFlatten
                            (\fieldPrintWithComments ->
                                Print.withIndentIncreasedBy 2
                                    fieldPrintWithComments
                            )
                            (Print.emptyOrLinebreakIndented lineSpread
                                |> Print.followedBy printExactlyCommaSpace
                            )
                    )
                |> Print.followedBy
                    (case maybeCommentsAfterFields of
                        Nothing ->
                            Print.spaceOrLinebreakIndented lineSpread

                        Just commentsAfterFields ->
                            -- yes, in record patterns trailing comments
                            -- are indented and not separated with an extra linebreak
                            Print.withIndentIncreasedBy 2
                                (Print.spaceOrLinebreakIndented lineSpread
                                    |> Print.followedBy commentsAfterFields.print
                                )
                                |> Print.followedBy (Print.spaceOrLinebreakIndented lineSpread)
                    )
                |> Print.followedBy printExactlyCurlyClosing


typeRecordExtension :
    List (GrenSyntax.Node String)
    ->
        { fullRange : GrenSyntax.Range
        , recordVariable : GrenSyntax.Node String
        , fields :
            List
                (GrenSyntax.Node
                    { name : GrenSyntax.Node String
                    , value : GrenSyntax.Node GrenSyntax.TypeAnnotation
                    }
                )
        }
    -> Print
typeRecordExtension syntaxComments syntaxRecordExtension =
    let
        commentsBeforeRecordVariable : List String
        commentsBeforeRecordVariable =
            commentsInRange
                { start = syntaxRecordExtension.fullRange.start
                , end = syntaxRecordExtension.recordVariable |> GrenSyntax.nodeRange |> .start
                }
                syntaxComments

        commentsCollapsibleBeforeRecordVariable : { print : Print, lineSpread : Print.LineSpread }
        commentsCollapsibleBeforeRecordVariable =
            collapsibleComments commentsBeforeRecordVariable

        fieldPrintsAndComments :
            { endLocation : GrenSyntax.Location
            , reverse :
                List
                    { syntax :
                        { name : GrenSyntax.Node String
                        , value : GrenSyntax.Node GrenSyntax.TypeAnnotation
                        }
                    , valuePrint : Print
                    , maybeCommentsBeforeName : Maybe { print : Print, lineSpread : Print.LineSpread }
                    , maybeCommentsBetweenNameAndValue : Maybe { print : Print, lineSpread : Print.LineSpread }
                    }
            }
        fieldPrintsAndComments =
            syntaxRecordExtension.fields
                |> List.foldl
                    (\fieldNode soFar ->
                        let
                            commentsBeforeName : List String
                            commentsBeforeName =
                                commentsInRange
                                    { start = soFar.endLocation, end = fieldNode.value.name.range.start }
                                    syntaxComments

                            commentsBetweenNameAndValue : List String
                            commentsBetweenNameAndValue =
                                commentsInRange
                                    { start = fieldNode.value.name.range.start
                                    , end = fieldNode.value.value.range.start
                                    }
                                    syntaxComments
                        in
                        { endLocation = fieldNode.value.value.range.end
                        , reverse =
                            { syntax = fieldNode.value
                            , valuePrint = typeNotParenthesized syntaxComments fieldNode.value.value
                            , maybeCommentsBeforeName =
                                case commentsBeforeName of
                                    [] ->
                                        Nothing

                                    comment0 :: comment1Up ->
                                        Just (collapsibleComments (comment0 :: comment1Up))
                            , maybeCommentsBetweenNameAndValue =
                                case commentsBetweenNameAndValue of
                                    [] ->
                                        Nothing

                                    comment0 :: comment1Up ->
                                        Just (collapsibleComments (comment0 :: comment1Up))
                            }
                                :: soFar.reverse
                        }
                    )
                    { endLocation =
                        syntaxRecordExtension.recordVariable
                            |> GrenSyntax.nodeRange
                            |> .end
                    , reverse = []
                    }

        commentsAfterFields : List String
        commentsAfterFields =
            commentsInRange
                { start = fieldPrintsAndComments.endLocation
                , end = syntaxRecordExtension.fullRange.end
                }
                syntaxComments

        lineSpread : Print.LineSpread
        lineSpread =
            lineSpreadInRange syntaxRecordExtension.fullRange
                |> Print.lineSpreadMergeWithStrict
                    commentsCollapsibleBeforeRecordVariable.lineSpread
                |> Print.lineSpreadMergeWith
                    (\() ->
                        fieldPrintsAndComments.reverse
                            |> Print.lineSpreadListMapAndCombine
                                (\field ->
                                    field.valuePrint
                                        |> Print.lineSpread
                                        |> Print.lineSpreadMergeWith
                                            (\() ->
                                                case field.maybeCommentsBeforeName of
                                                    Nothing ->
                                                        Print.SingleLine

                                                    Just commentsBeforeName ->
                                                        commentsBeforeName.lineSpread
                                            )
                                        |> Print.lineSpreadMergeWith
                                            (\() ->
                                                case field.maybeCommentsBetweenNameAndValue of
                                                    Nothing ->
                                                        Print.SingleLine

                                                    Just commentsBetweenNameAndValue ->
                                                        commentsBetweenNameAndValue.lineSpread
                                            )
                                )
                    )
                |> Print.lineSpreadMergeWith
                    (\() ->
                        case commentsAfterFields of
                            [] ->
                                Print.SingleLine

                            _ :: _ ->
                                Print.MultipleLines
                    )

        recordVariablePrint : Print
        recordVariablePrint =
            Print.exactly
                (syntaxRecordExtension.recordVariable |> GrenSyntax.nodeValue)
    in
    printExactlyCurlyOpeningSpace
        |> Print.followedBy
            (Print.withIndentIncreasedBy 2
                (case commentsBeforeRecordVariable of
                    [] ->
                        recordVariablePrint

                    _ :: _ ->
                        commentsCollapsibleBeforeRecordVariable.print
                            |> Print.followedBy
                                (Print.spaceOrLinebreakIndented
                                    commentsCollapsibleBeforeRecordVariable.lineSpread
                                )
                            |> Print.followedBy recordVariablePrint
                )
            )
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.spaceOrLinebreakIndented lineSpread
                    |> Print.followedBy printExactlyVerticalBarSpace
                    |> Print.followedBy
                        (fieldPrintsAndComments.reverse
                            |> Print.listReverseAndMapAndIntersperseAndFlatten
                                (\field ->
                                    let
                                        lineSpreadBetweenNameAndValueNotConsideringComments : () -> Print.LineSpread
                                        lineSpreadBetweenNameAndValueNotConsideringComments () =
                                            lineSpreadInRange
                                                { start = field.syntax.name.range.start
                                                , end = field.syntax.value |> GrenSyntax.nodeRange |> .end
                                                }
                                                |> Print.lineSpreadMergeWith
                                                    (\() -> field.valuePrint |> Print.lineSpread)
                                    in
                                    Print.withIndentIncreasedBy 2
                                        ((case field.maybeCommentsBeforeName of
                                            Nothing ->
                                                Print.exactly (field.syntax.name.value ++ " :")

                                            Just commentsBeforeName ->
                                                commentsBeforeName.print
                                                    |> Print.followedBy
                                                        (Print.spaceOrLinebreakIndented
                                                            commentsBeforeName.lineSpread
                                                        )
                                                    |> Print.followedBy
                                                        (Print.exactly
                                                            (field.syntax.name.value ++ " :")
                                                        )
                                         )
                                            |> Print.followedBy
                                                (Print.withIndentAtNextMultipleOf4
                                                    ((case field.maybeCommentsBetweenNameAndValue of
                                                        Nothing ->
                                                            Print.spaceOrLinebreakIndented
                                                                (lineSpreadBetweenNameAndValueNotConsideringComments ())

                                                        Just commentsBetweenNameAndValue ->
                                                            Print.spaceOrLinebreakIndented
                                                                (commentsBetweenNameAndValue.lineSpread
                                                                    |> Print.lineSpreadMergeWith
                                                                        lineSpreadBetweenNameAndValueNotConsideringComments
                                                                )
                                                                |> Print.followedBy commentsBetweenNameAndValue.print
                                                                |> Print.followedBy
                                                                    (Print.spaceOrLinebreakIndented
                                                                        (commentsBetweenNameAndValue.lineSpread
                                                                            |> Print.lineSpreadMergeWith
                                                                                (\() -> field.valuePrint |> Print.lineSpread)
                                                                        )
                                                                    )
                                                     )
                                                        |> Print.followedBy field.valuePrint
                                                    )
                                                )
                                        )
                                )
                                (Print.emptyOrLinebreakIndented lineSpread
                                    |> Print.followedBy printExactlyCommaSpace
                                )
                        )
                    |> -- indenting trailing comments is a rudiment from elm-format
                       Print.followedBy
                        (case commentsAfterFields of
                            [] ->
                                Print.empty

                            comment0 :: comment1Up ->
                                Print.linebreak
                                    |> Print.followedBy (Print.spaceOrLinebreakIndented lineSpread)
                                    |> Print.followedBy (comments (comment0 :: comment1Up))
                        )
                )
            )
        |> Print.followedBy (Print.spaceOrLinebreakIndented lineSpread)
        |> Print.followedBy printExactlyCurlyClosing


construct :
    { printArgumentParenthesizedIfSpaceSeparated :
        List (GrenSyntax.Node String) -> GrenSyntax.Node a -> Print
    , lineSpreadMinimum : Print.LineSpread
    }
    -> List (GrenSyntax.Node String)
    ->
        { arguments : List (GrenSyntax.Node a)
        , fullRange : GrenSyntax.Range
        , start : String
        }
    -> Print
construct specific syntaxComments syntaxConstruct =
    let
        argumentPrintsAndCommentsBeforeReverse : List Print
        argumentPrintsAndCommentsBeforeReverse =
            syntaxConstruct.arguments
                |> List.foldl
                    (\argument soFar ->
                        let
                            print : Print
                            print =
                                specific.printArgumentParenthesizedIfSpaceSeparated syntaxComments
                                    argument
                        in
                        { reverse =
                            (case
                                commentsInRange
                                    { start = soFar.endLocation
                                    , end = argument |> GrenSyntax.nodeRange |> .start
                                    }
                                    syntaxComments
                             of
                                [] ->
                                    print

                                comment0 :: comment1Up ->
                                    let
                                        commentsBeforeArgument : { print : Print, lineSpread : Print.LineSpread }
                                        commentsBeforeArgument =
                                            collapsibleComments (comment0 :: comment1Up)
                                    in
                                    commentsBeforeArgument.print
                                        |> Print.followedBy
                                            (Print.spaceOrLinebreakIndented
                                                (commentsBeforeArgument.lineSpread
                                                    |> Print.lineSpreadMergeWith
                                                        (\() -> print |> Print.lineSpread)
                                                )
                                            )
                                        |> Print.followedBy print
                            )
                                :: soFar.reverse
                        , endLocation = argument |> GrenSyntax.nodeRange |> .end
                        }
                    )
                    { reverse = []
                    , endLocation = syntaxConstruct.fullRange.start
                    }
                |> .reverse

        lineSpread : Print.LineSpread
        lineSpread =
            specific.lineSpreadMinimum
                |> Print.lineSpreadMergeWith
                    (\() ->
                        argumentPrintsAndCommentsBeforeReverse
                            |> Print.lineSpreadListMapAndCombine
                                Print.lineSpread
                    )
    in
    Print.exactly syntaxConstruct.start
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (argumentPrintsAndCommentsBeforeReverse
                    |> Print.listReverseAndMapAndFlatten
                        (\argumentPrintWithCommentsBefore ->
                            Print.spaceOrLinebreakIndented lineSpread
                                |> Print.followedBy
                                    argumentPrintWithCommentsBefore
                        )
                )
            )


recordLiteral :
    { nameValueSeparator : String
    , printValueNotParenthesized :
        List (GrenSyntax.Node String)
        -> GrenSyntax.Node fieldValue
        -> Print
    }
    -> List (GrenSyntax.Node String)
    ->
        { fields :
            List
                (GrenSyntax.Node
                    { name : GrenSyntax.Node String
                    , value : GrenSyntax.Node fieldValue
                    }
                )
        , fullRange : GrenSyntax.Range
        }
    -> Print
recordLiteral fieldSpecific syntaxComments syntaxRecord =
    case syntaxRecord.fields of
        [] ->
            printExactlyCurlyOpening
                |> Print.followedBy
                    (case commentsInRange syntaxRecord.fullRange syntaxComments of
                        [] ->
                            printExactlyCurlyClosing

                        comment0 :: comment1Up ->
                            let
                                commentsCollapsed : { print : Print, lineSpread : Print.LineSpread }
                                commentsCollapsed =
                                    collapsibleComments (comment0 :: comment1Up)
                            in
                            Print.withIndentIncreasedBy 1
                                commentsCollapsed.print
                                |> Print.followedBy
                                    (Print.emptyOrLinebreakIndented
                                        commentsCollapsed.lineSpread
                                    )
                                |> Print.followedBy printExactlyCurlyClosing
                    )

        field0 :: field1Up ->
            let
                fieldPrintsAndComments :
                    { endLocation : GrenSyntax.Location
                    , reverse :
                        List
                            { syntax :
                                { name : GrenSyntax.Node String
                                , value : GrenSyntax.Node fieldValue
                                }
                            , valuePrint : Print
                            , maybeCommentsBeforeName : Maybe { print : Print, lineSpread : Print.LineSpread }
                            , maybeCommentsBetweenNameAndValue : Maybe { print : Print, lineSpread : Print.LineSpread }
                            }
                    }
                fieldPrintsAndComments =
                    (field0 :: field1Up)
                        |> List.foldl
                            (\fieldNode soFar ->
                                let
                                    commentsBeforeName : List String
                                    commentsBeforeName =
                                        commentsInRange
                                            { start = soFar.endLocation, end = fieldNode.value.name.range.start }
                                            syntaxComments

                                    commentsBetweenNameAndValue : List String
                                    commentsBetweenNameAndValue =
                                        commentsInRange
                                            { start = fieldNode.value.name.range.start, end = fieldNode.value.value.range.start }
                                            syntaxComments
                                in
                                { endLocation = fieldNode.value.value.range.end
                                , reverse =
                                    { syntax = fieldNode.value
                                    , valuePrint =
                                        fieldSpecific.printValueNotParenthesized syntaxComments
                                            fieldNode.value.value
                                    , maybeCommentsBeforeName =
                                        case commentsBeforeName of
                                            [] ->
                                                Nothing

                                            comment0 :: comment1Up ->
                                                Just (collapsibleComments (comment0 :: comment1Up))
                                    , maybeCommentsBetweenNameAndValue =
                                        case commentsBetweenNameAndValue of
                                            [] ->
                                                Nothing

                                            comment0 :: comment1Up ->
                                                Just (collapsibleComments (comment0 :: comment1Up))
                                    }
                                        :: soFar.reverse
                                }
                            )
                            { endLocation = syntaxRecord.fullRange.start
                            , reverse = []
                            }

                commentsAfterFields : List String
                commentsAfterFields =
                    commentsInRange
                        { start = fieldPrintsAndComments.endLocation
                        , end = syntaxRecord.fullRange.end
                        }
                        syntaxComments

                lineSpread : Print.LineSpread
                lineSpread =
                    lineSpreadInRange syntaxRecord.fullRange
                        |> Print.lineSpreadMergeWith
                            (\() ->
                                fieldPrintsAndComments.reverse
                                    |> Print.lineSpreadListMapAndCombine
                                        (\field ->
                                            field.valuePrint
                                                |> Print.lineSpread
                                                |> Print.lineSpreadMergeWith
                                                    (\() ->
                                                        case field.maybeCommentsBeforeName of
                                                            Nothing ->
                                                                Print.SingleLine

                                                            Just commentsBeforeName ->
                                                                commentsBeforeName.lineSpread
                                                    )
                                                |> Print.lineSpreadMergeWith
                                                    (\() ->
                                                        case field.maybeCommentsBetweenNameAndValue of
                                                            Nothing ->
                                                                Print.SingleLine

                                                            Just commentsBetweenNameAndValue ->
                                                                commentsBetweenNameAndValue.lineSpread
                                                    )
                                        )
                            )
                        |> Print.lineSpreadMergeWith
                            (\() ->
                                case commentsAfterFields of
                                    [] ->
                                        Print.SingleLine

                                    _ :: _ ->
                                        Print.MultipleLines
                            )
            in
            printExactlyCurlyOpeningSpace
                |> Print.followedBy
                    (fieldPrintsAndComments.reverse
                        |> Print.listReverseAndMapAndIntersperseAndFlatten
                            (\fieldPrintAndComment ->
                                let
                                    field =
                                        fieldPrintAndComment.syntax

                                    lineSpreadBetweenNameAndValueNotConsideringComments : () -> Print.LineSpread
                                    lineSpreadBetweenNameAndValueNotConsideringComments () =
                                        lineSpreadInRange
                                            { start = field.name.range.start
                                            , end = field.value |> GrenSyntax.nodeRange |> .end
                                            }
                                            |> Print.lineSpreadMergeWith
                                                (\() -> fieldPrintAndComment.valuePrint |> Print.lineSpread)

                                    nameSeparatorValuePrint : Print
                                    nameSeparatorValuePrint =
                                        Print.exactly (field.name.value ++ " " ++ fieldSpecific.nameValueSeparator)
                                            |> Print.followedBy
                                                (Print.withIndentAtNextMultipleOf4
                                                    ((case fieldPrintAndComment.maybeCommentsBetweenNameAndValue of
                                                        Nothing ->
                                                            Print.spaceOrLinebreakIndented
                                                                (lineSpreadBetweenNameAndValueNotConsideringComments ())

                                                        Just commentsBetweenNameAndValue ->
                                                            Print.spaceOrLinebreakIndented
                                                                (commentsBetweenNameAndValue.lineSpread
                                                                    |> Print.lineSpreadMergeWith
                                                                        lineSpreadBetweenNameAndValueNotConsideringComments
                                                                )
                                                                |> Print.followedBy commentsBetweenNameAndValue.print
                                                                |> Print.followedBy
                                                                    (Print.spaceOrLinebreakIndented
                                                                        (commentsBetweenNameAndValue.lineSpread
                                                                            |> Print.lineSpreadMergeWith
                                                                                (\() -> fieldPrintAndComment.valuePrint |> Print.lineSpread)
                                                                        )
                                                                    )
                                                     )
                                                        |> Print.followedBy fieldPrintAndComment.valuePrint
                                                    )
                                                )
                                in
                                Print.withIndentIncreasedBy 2
                                    (case fieldPrintAndComment.maybeCommentsBeforeName of
                                        Nothing ->
                                            nameSeparatorValuePrint

                                        Just commentsBeforeName ->
                                            commentsBeforeName.print
                                                |> Print.followedBy
                                                    (Print.spaceOrLinebreakIndented
                                                        commentsBeforeName.lineSpread
                                                    )
                                                |> Print.followedBy nameSeparatorValuePrint
                                    )
                            )
                            (Print.emptyOrLinebreakIndented lineSpread
                                |> Print.followedBy printExactlyCommaSpace
                            )
                    )
                |> Print.followedBy
                    (case commentsAfterFields of
                        [] ->
                            Print.spaceOrLinebreakIndented lineSpread

                        comment0 :: comment1Up ->
                            Print.linebreak
                                |> Print.followedBy (Print.spaceOrLinebreakIndented lineSpread)
                                |> Print.followedBy (comments (comment0 :: comment1Up))
                                |> Print.followedBy (Print.spaceOrLinebreakIndented lineSpread)
                    )
                |> Print.followedBy printExactlyCurlyClosing


{-| Print a name with its qualification (`[]` for no qualification)
-}
qualifiedReference : { qualification : List String, name : String } -> String
qualifiedReference syntaxReference =
    case syntaxReference.qualification of
        [] ->
            syntaxReference.name

        modulePartHead :: modulePartTail ->
            modulePartHead
                ++ (modulePartTail
                        |> listMapAndFlattenToString
                            (\modulePart -> "." ++ modulePart)
                   )
                ++ "."
                ++ syntaxReference.name


lineSpreadBetweenRanges : GrenSyntax.Range -> GrenSyntax.Range -> Print.LineSpread
lineSpreadBetweenRanges earlierRange laterRange =
    if laterRange.end.row - earlierRange.start.row == 0 then
        Print.SingleLine

    else
        Print.MultipleLines


lineSpreadBetweenNodes : GrenSyntax.Node a_ -> GrenSyntax.Node b_ -> Print.LineSpread
lineSpreadBetweenNodes earlier later =
    if later.range.end.row - earlier.range.start.row == 0 then
        Print.SingleLine

    else
        Print.MultipleLines


lineSpreadInNode : GrenSyntax.Node a_ -> Print.LineSpread
lineSpreadInNode node =
    lineSpreadInRange node.range


typeFunctionNotParenthesized :
    List (GrenSyntax.Node String)
    ->
        { fullRange : GrenSyntax.Range
        , input : GrenSyntax.Node GrenSyntax.TypeAnnotation
        , output : GrenSyntax.Node GrenSyntax.TypeAnnotation
        }
    -> Print
typeFunctionNotParenthesized syntaxComments function =
    let
        afterArrowTypes :
            { beforeRightest : List (GrenSyntax.Node GrenSyntax.TypeAnnotation)
            , rightest : GrenSyntax.Node GrenSyntax.TypeAnnotation
            }
        afterArrowTypes =
            typeFunctionExpand function.output

        afterArrowTypesBeforeRightestPrintsWithCommentsBefore :
            { endLocation : GrenSyntax.Location
            , reverse : List Print
            }
        afterArrowTypesBeforeRightestPrintsWithCommentsBefore =
            afterArrowTypes.beforeRightest
                |> List.foldl
                    (\afterArrowTypeNode soFar ->
                        let
                            print : Print
                            print =
                                typeParenthesizedIfFunction syntaxComments
                                    afterArrowTypeNode
                        in
                        { endLocation = afterArrowTypeNode.range.end
                        , reverse =
                            ((case
                                commentsInRange
                                    { start = soFar.endLocation, end = afterArrowTypeNode.range.start }
                                    syntaxComments
                              of
                                [] ->
                                    Print.spaceOrLinebreakIndented
                                        (print |> Print.lineSpread)

                                comment0 :: comment1Up ->
                                    let
                                        commentsBeforeAfterArrowType : { print : Print, lineSpread : Print.LineSpread }
                                        commentsBeforeAfterArrowType =
                                            collapsibleComments (comment0 :: comment1Up)

                                        lineSpread : Print.LineSpread
                                        lineSpread =
                                            commentsBeforeAfterArrowType.lineSpread
                                                |> Print.lineSpreadMergeWith
                                                    (\() -> print |> Print.lineSpread)
                                    in
                                    Print.spaceOrLinebreakIndented lineSpread
                                        |> Print.followedBy commentsBeforeAfterArrowType.print
                                        |> Print.followedBy
                                            (Print.spaceOrLinebreakIndented
                                                lineSpread
                                            )
                             )
                                |> Print.followedBy print
                            )
                                :: soFar.reverse
                        }
                    )
                    { endLocation = function.input |> GrenSyntax.nodeRange |> .end
                    , reverse = []
                    }

        commentsBeforeRightestAfterArrowType : List String
        commentsBeforeRightestAfterArrowType =
            commentsInRange
                { start = afterArrowTypesBeforeRightestPrintsWithCommentsBefore.endLocation
                , end = afterArrowTypes.rightest |> GrenSyntax.nodeRange |> .start
                }
                syntaxComments

        commentsCollapsibleBeforeRightestAfterArrowType : { print : Print, lineSpread : Print.LineSpread }
        commentsCollapsibleBeforeRightestAfterArrowType =
            collapsibleComments commentsBeforeRightestAfterArrowType

        inTypePrint : Print
        inTypePrint =
            typeParenthesizedIfFunction syntaxComments function.input

        rightestAfterArrowTypePrint : Print
        rightestAfterArrowTypePrint =
            typeParenthesizedIfParenthesizedFunction syntaxComments
                afterArrowTypes.rightest

        rightestAfterArrowTypeWithCommentsBeforePrint : Print.Print
        rightestAfterArrowTypeWithCommentsBeforePrint =
            (case commentsBeforeRightestAfterArrowType of
                [] ->
                    Print.spaceOrLinebreakIndented
                        (rightestAfterArrowTypePrint |> Print.lineSpread)

                comment0 :: comment1Up ->
                    let
                        commentsCollapsible : { print : Print, lineSpread : Print.LineSpread }
                        commentsCollapsible =
                            collapsibleComments (comment0 :: comment1Up)

                        lineSpread : Print.LineSpread
                        lineSpread =
                            commentsCollapsible.lineSpread
                                |> Print.lineSpreadMergeWith
                                    (\() -> rightestAfterArrowTypePrint |> Print.lineSpread)
                    in
                    Print.spaceOrLinebreakIndented lineSpread
                        |> Print.followedBy commentsCollapsible.print
                        |> Print.followedBy
                            (Print.spaceOrLinebreakIndented
                                lineSpread
                            )
            )
                |> Print.followedBy rightestAfterArrowTypePrint

        fullLineSpread : Print.LineSpread
        fullLineSpread =
            lineSpreadInRange function.fullRange
                |> Print.lineSpreadMergeWith
                    (\() -> inTypePrint |> Print.lineSpread)
                |> Print.lineSpreadMergeWith
                    (\() -> rightestAfterArrowTypeWithCommentsBeforePrint |> Print.lineSpread)
                |> Print.lineSpreadMergeWithStrict
                    commentsCollapsibleBeforeRightestAfterArrowType.lineSpread
                |> Print.lineSpreadMergeWith
                    (\() ->
                        afterArrowTypesBeforeRightestPrintsWithCommentsBefore.reverse
                            |> Print.lineSpreadListMapAndCombine Print.lineSpread
                    )
    in
    inTypePrint
        |> Print.followedBy
            (afterArrowTypesBeforeRightestPrintsWithCommentsBefore.reverse
                |> Print.listReverseAndMapAndFlatten
                    (\printWithCommentsBefore ->
                        Print.spaceOrLinebreakIndented fullLineSpread
                            |> Print.followedBy printExactlyMinusGreaterThan
                            |> Print.followedBy
                                (Print.withIndentAtNextMultipleOf4
                                    printWithCommentsBefore
                                )
                    )
            )
        |> Print.followedBy
            (Print.spaceOrLinebreakIndented fullLineSpread
                |> Print.followedBy printExactlyMinusGreaterThan
                |> Print.followedBy
                    (Print.withIndentAtNextMultipleOf4
                        rightestAfterArrowTypeWithCommentsBeforePrint
                    )
            )


typeParenthesizedIfParenthesizedFunction :
    List (GrenSyntax.Node String)
    -> GrenSyntax.Node GrenSyntax.TypeAnnotation
    -> Print
typeParenthesizedIfParenthesizedFunction syntaxComments typeNode =
    case typeNode |> GrenSyntax.nodeValue of
        GrenSyntax.TypeAnnotationParenthesized inParens ->
            inParens |> typeParenthesizedIfFunction syntaxComments

        _ ->
            typeNotParenthesized syntaxComments typeNode


typeParenthesizedIfFunction :
    List (GrenSyntax.Node String)
    -> GrenSyntax.Node GrenSyntax.TypeAnnotation
    -> Print
typeParenthesizedIfFunction syntaxComments typeNode =
    case typeNode |> typeToFunction of
        Just _ ->
            typeParenthesized syntaxComments typeNode

        Nothing ->
            typeNotParenthesized syntaxComments typeNode


typeToFunction :
    GrenSyntax.Node GrenSyntax.TypeAnnotation
    ->
        Maybe
            { input : GrenSyntax.Node GrenSyntax.TypeAnnotation
            , output : GrenSyntax.Node GrenSyntax.TypeAnnotation
            }
typeToFunction typeNode =
    case typeNode |> typeToNotParenthesized |> GrenSyntax.nodeValue of
        GrenSyntax.TypeAnnotationFunction typeFunction ->
            Just typeFunction

        GrenSyntax.TypeAnnotationVariable _ ->
            Nothing

        GrenSyntax.TypeAnnotationConstruct _ ->
            Nothing

        GrenSyntax.TypeAnnotationUnit ->
            Nothing

        GrenSyntax.TypeAnnotationParenthesized _ ->
            Nothing

        GrenSyntax.TypeAnnotationRecord _ ->
            Nothing

        GrenSyntax.TypeAnnotationRecordExtension _ ->
            Nothing


{-| Remove as many parens as possible
-}
typeToNotParenthesized :
    GrenSyntax.Node GrenSyntax.TypeAnnotation
    -> GrenSyntax.Node GrenSyntax.TypeAnnotation
typeToNotParenthesized syntaxTypeNode =
    case syntaxTypeNode.value of
        GrenSyntax.TypeAnnotationVariable name ->
            { range = syntaxTypeNode.range, value = GrenSyntax.TypeAnnotationVariable name }

        GrenSyntax.TypeAnnotationConstruct typeConstruct ->
            { range = syntaxTypeNode.range, value = GrenSyntax.TypeAnnotationConstruct typeConstruct }

        GrenSyntax.TypeAnnotationUnit ->
            { range = syntaxTypeNode.range, value = GrenSyntax.TypeAnnotationUnit }

        GrenSyntax.TypeAnnotationParenthesized inParens ->
            typeToNotParenthesized inParens

        GrenSyntax.TypeAnnotationRecord fields ->
            { range = syntaxTypeNode.range, value = GrenSyntax.TypeAnnotationRecord fields }

        GrenSyntax.TypeAnnotationRecordExtension extendedRecord ->
            { range = syntaxTypeNode.range
            , value = GrenSyntax.TypeAnnotationRecordExtension extendedRecord
            }

        GrenSyntax.TypeAnnotationFunction typeFunction ->
            { range = syntaxTypeNode.range, value = GrenSyntax.TypeAnnotationFunction typeFunction }


typeFunctionExpand :
    GrenSyntax.Node GrenSyntax.TypeAnnotation
    ->
        { beforeRightest :
            List (GrenSyntax.Node GrenSyntax.TypeAnnotation)
        , rightest : GrenSyntax.Node GrenSyntax.TypeAnnotation
        }
typeFunctionExpand typeNode =
    case typeNode.value of
        GrenSyntax.TypeAnnotationFunction typeFunction ->
            let
                outTypeExpanded :
                    { beforeRightest : List (GrenSyntax.Node GrenSyntax.TypeAnnotation)
                    , rightest : GrenSyntax.Node GrenSyntax.TypeAnnotation
                    }
                outTypeExpanded =
                    typeFunctionExpand typeFunction.output
            in
            { beforeRightest = typeFunction.input :: outTypeExpanded.beforeRightest
            , rightest = outTypeExpanded.rightest
            }

        _ ->
            { beforeRightest = [], rightest = typeNode }


typeParenthesizedIfSpaceSeparated :
    List (GrenSyntax.Node String)
    -> GrenSyntax.Node GrenSyntax.TypeAnnotation
    -> Print
typeParenthesizedIfSpaceSeparated syntaxComments typeNode =
    if typeIsSpaceSeparated (typeNode |> GrenSyntax.nodeValue) then
        typeParenthesized syntaxComments typeNode

    else
        typeNotParenthesized syntaxComments typeNode


typeParenthesized :
    List (GrenSyntax.Node String)
    -> GrenSyntax.Node GrenSyntax.TypeAnnotation
    -> Print
typeParenthesized syntaxComments typeNode =
    parenthesized typeNotParenthesized
        { notParenthesized = typeNode |> typeToNotParenthesized
        , fullRange = typeNode |> GrenSyntax.nodeRange
        }
        syntaxComments


parenthesized :
    (List (GrenSyntax.Node String) -> GrenSyntax.Node a -> Print)
    ->
        { notParenthesized : GrenSyntax.Node a
        , fullRange : GrenSyntax.Range
        }
    -> List (GrenSyntax.Node String)
    -> Print
parenthesized printNotParenthesized syntax syntaxComments =
    let
        commentsBeforeInner : List String
        commentsBeforeInner =
            commentsInRange
                { start = syntax.fullRange.start
                , end = syntax.notParenthesized |> GrenSyntax.nodeRange |> .start
                }
                syntaxComments

        commentsAfterInner : List String
        commentsAfterInner =
            commentsInRange
                { start = syntax.notParenthesized |> GrenSyntax.nodeRange |> .end
                , end = syntax.fullRange.end
                }
                syntaxComments

        notParenthesizedPrint : Print
        notParenthesizedPrint =
            printNotParenthesized syntaxComments syntax.notParenthesized

        commentsBeforeInnerCollapsible : { print : Print, lineSpread : Print.LineSpread }
        commentsBeforeInnerCollapsible =
            collapsibleComments commentsBeforeInner

        commentsAfterInnerCollapsible : { print : Print, lineSpread : Print.LineSpread }
        commentsAfterInnerCollapsible =
            collapsibleComments commentsAfterInner

        lineSpread : Print.LineSpread
        lineSpread =
            notParenthesizedPrint
                |> Print.lineSpread
                |> Print.lineSpreadMergeWithStrict
                    commentsBeforeInnerCollapsible.lineSpread
                |> Print.lineSpreadMergeWithStrict
                    commentsAfterInnerCollapsible.lineSpread
    in
    printExactlyParensOpening
        |> Print.followedBy
            (Print.withIndentIncreasedBy 1
                ((case commentsBeforeInner of
                    [] ->
                        notParenthesizedPrint

                    _ :: _ ->
                        commentsBeforeInnerCollapsible.print
                            |> Print.followedBy
                                (Print.spaceOrLinebreakIndented lineSpread)
                            |> Print.followedBy notParenthesizedPrint
                 )
                    |> Print.followedBy
                        (case commentsAfterInner of
                            [] ->
                                Print.empty

                            _ :: _ ->
                                Print.spaceOrLinebreakIndented lineSpread
                                    |> Print.followedBy
                                        commentsAfterInnerCollapsible.print
                        )
                )
            )
        |> Print.followedBy
            (Print.emptyOrLinebreakIndented lineSpread)
        |> Print.followedBy printExactlyParensClosing


typeIsSpaceSeparated : GrenSyntax.TypeAnnotation -> Bool
typeIsSpaceSeparated syntaxType =
    case syntaxType of
        GrenSyntax.TypeAnnotationVariable _ ->
            False

        GrenSyntax.TypeAnnotationConstruct typeConstruct ->
            case typeConstruct.arguments of
                [] ->
                    False

                _ :: _ ->
                    True

        GrenSyntax.TypeAnnotationUnit ->
            False

        GrenSyntax.TypeAnnotationParenthesized inParensNode ->
            typeIsSpaceSeparated inParensNode.value

        GrenSyntax.TypeAnnotationRecord _ ->
            False

        GrenSyntax.TypeAnnotationRecordExtension _ ->
            False

        GrenSyntax.TypeAnnotationFunction _ ->
            True


{-| Print an [`GrenSyntax.TypeAnnotation`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-TypeAnnotation#TypeAnnotation)
-}
typeNotParenthesized :
    List (GrenSyntax.Node String)
    -> GrenSyntax.Node GrenSyntax.TypeAnnotation
    -> Print
typeNotParenthesized syntaxComments syntaxTypeNode =
    -- IGNORE TCO
    case syntaxTypeNode.value of
        GrenSyntax.TypeAnnotationUnit ->
            printExactlyCurlyBraceOpeningCurlyBraceClosing

        GrenSyntax.TypeAnnotationVariable name ->
            Print.exactly name

        GrenSyntax.TypeAnnotationConstruct typeConstruct ->
            construct
                { printArgumentParenthesizedIfSpaceSeparated =
                    typeParenthesizedIfSpaceSeparated
                , lineSpreadMinimum = lineSpreadInRange syntaxTypeNode.range
                }
                syntaxComments
                { fullRange = syntaxTypeNode.range
                , start =
                    qualifiedReference typeConstruct.reference.value
                , arguments = typeConstruct.arguments
                }

        GrenSyntax.TypeAnnotationParenthesized inParens ->
            let
                commentsBeforeInParens : List String
                commentsBeforeInParens =
                    commentsInRange
                        { start = syntaxTypeNode.range.start
                        , end = inParens |> GrenSyntax.nodeRange |> .start
                        }
                        syntaxComments

                commentsAfterInParens : List String
                commentsAfterInParens =
                    commentsInRange
                        { start = inParens |> GrenSyntax.nodeRange |> .end
                        , end = syntaxTypeNode.range.end
                        }
                        syntaxComments
            in
            case ( commentsBeforeInParens, commentsAfterInParens ) of
                ( [], [] ) ->
                    typeNotParenthesized syntaxComments inParens

                _ ->
                    parenthesized typeNotParenthesized
                        { notParenthesized = inParens |> typeToNotParenthesized
                        , fullRange = syntaxTypeNode.range
                        }
                        syntaxComments

        GrenSyntax.TypeAnnotationRecord fields ->
            recordLiteral
                { printValueNotParenthesized = typeNotParenthesized
                , nameValueSeparator = ":"
                }
                syntaxComments
                { fullRange = syntaxTypeNode.range, fields = fields }

        GrenSyntax.TypeAnnotationRecordExtension extendedRecord ->
            typeRecordExtension syntaxComments
                { fullRange = syntaxTypeNode.range
                , recordVariable = extendedRecord.recordVariable
                , fields = extendedRecord.fields.value
                }

        GrenSyntax.TypeAnnotationFunction typeFunction ->
            { fullRange = syntaxTypeNode.range
            , input = typeFunction.input
            , output = typeFunction.output
            }
                |> typeFunctionNotParenthesized syntaxComments


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
    case syntaxDeclarations of
        [] ->
            -- invalid syntax
            Print.empty

        declaration0Node :: declarations1Up ->
            declaration
                { comments = context.comments
                , portDocumentationComment =
                    case declaration0Node.value of
                        GrenSyntax.PortDeclaration _ ->
                            firstCommentInRange
                                { start = context.previousEnd
                                , end = declaration0Node.range.start
                                }
                                context.portDocumentationComments

                        GrenSyntax.ValueOrFunctionDeclaration _ ->
                            Nothing

                        GrenSyntax.AliasDeclaration _ ->
                            Nothing

                        GrenSyntax.ChoiceTypeDeclaration _ ->
                            Nothing

                        GrenSyntax.InfixDeclaration _ ->
                            Nothing
                }
                declaration0Node.value
                |> Print.followedBy
                    (declarations1Up
                        |> List.foldl
                            (\syntaxDeclarationNode soFar ->
                                let
                                    maybeDeclarationPortDocumentationComment : Maybe (GrenSyntax.Node String)
                                    maybeDeclarationPortDocumentationComment =
                                        case syntaxDeclarationNode.value of
                                            GrenSyntax.PortDeclaration _ ->
                                                firstCommentInRange
                                                    { start = soFar.previousRange.end
                                                    , end = syntaxDeclarationNode.range.start
                                                    }
                                                    context.portDocumentationComments

                                            GrenSyntax.ValueOrFunctionDeclaration _ ->
                                                Nothing

                                            GrenSyntax.AliasDeclaration _ ->
                                                Nothing

                                            GrenSyntax.ChoiceTypeDeclaration _ ->
                                                Nothing

                                            GrenSyntax.InfixDeclaration _ ->
                                                Nothing
                                in
                                { print =
                                    soFar.print
                                        |> Print.followedBy
                                            (case
                                                commentsInRange
                                                    { start = soFar.previousRange.end
                                                    , end = syntaxDeclarationNode.range.start
                                                    }
                                                    context.comments
                                             of
                                                comment0 :: comment1Up ->
                                                    printLinebreakLinebreakLinebreak
                                                        |> Print.followedBy
                                                            (moduleLevelCommentsBeforeDeclaration
                                                                { comment0 = comment0, comment1Up = comment1Up }
                                                            )
                                                        |> Print.followedBy
                                                            (declaration
                                                                { comments =
                                                                    commentNodesInRange syntaxDeclarationNode.range
                                                                        context.comments
                                                                , portDocumentationComment = maybeDeclarationPortDocumentationComment
                                                                }
                                                                syntaxDeclarationNode.value
                                                            )

                                                [] ->
                                                    linebreaksFollowedByDeclaration
                                                        { comments =
                                                            commentNodesInRange syntaxDeclarationNode.range
                                                                context.comments
                                                        , portDocumentationComment = maybeDeclarationPortDocumentationComment
                                                        }
                                                        syntaxDeclarationNode.value
                                            )
                                , previousRange = syntaxDeclarationNode.range
                                }
                            )
                            { print = Print.empty
                            , previousRange = declaration0Node.range
                            }
                        |> .print
                    )


firstCommentInRange :
    GrenSyntax.Range
    -> List (GrenSyntax.Node String)
    -> Maybe (GrenSyntax.Node String)
firstCommentInRange range sortedComments =
    case sortedComments of
        [] ->
            Nothing

        headCommentNode :: tailComments ->
            case locationCompareFast headCommentNode.range.start range.start of
                LT ->
                    firstCommentInRange range tailComments

                EQ ->
                    Just headCommentNode

                GT ->
                    case locationCompareFast headCommentNode.range.end range.end of
                        GT ->
                            Nothing

                        LT ->
                            Just headCommentNode

                        EQ ->
                            Just headCommentNode


locationCompareFast : GrenSyntax.Location -> GrenSyntax.Location -> Basics.Order
locationCompareFast left right =
    if left.row - right.row < 0 then
        LT

    else if left.row - right.row > 0 then
        GT

    else
        Basics.compare left.column right.column


linebreaksFollowedByDeclaration :
    { portDocumentationComment : Maybe (GrenSyntax.Node String)
    , comments : List (GrenSyntax.Node String)
    }
    -> GrenSyntax.Declaration
    -> Print
linebreaksFollowedByDeclaration syntaxComments syntaxDeclaration =
    case syntaxDeclaration of
        GrenSyntax.ValueOrFunctionDeclaration syntaxExpressionDeclaration ->
            printLinebreakLinebreakLinebreak
                |> Print.followedBy (declarationExpression syntaxComments.comments syntaxExpressionDeclaration)

        GrenSyntax.AliasDeclaration syntaxTypeAliasDeclaration ->
            printLinebreakLinebreakLinebreak
                |> Print.followedBy (declarationTypeAlias syntaxComments.comments syntaxTypeAliasDeclaration)

        GrenSyntax.ChoiceTypeDeclaration syntaxChoiceTypeDeclaration ->
            printLinebreakLinebreakLinebreak
                |> Print.followedBy (declarationChoiceType syntaxComments.comments syntaxChoiceTypeDeclaration)

        GrenSyntax.PortDeclaration signature ->
            printLinebreakLinebreakLinebreak
                |> Print.followedBy
                    (declarationPort
                        { documentationComment = syntaxComments.portDocumentationComment
                        , comments = syntaxComments.comments
                        }
                        signature
                    )

        GrenSyntax.InfixDeclaration syntaxInfixDeclaration ->
            Print.linebreak
                |> Print.followedBy (declarationInfix syntaxInfixDeclaration)


listFilledLast : a -> List a -> a
listFilledLast head tail =
    case tail of
        [] ->
            head

        tailHead :: tailTail ->
            listFilledLast tailHead tailTail


{-| Print an [`GrenSyntax.Declaration`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Declaration#Declaration)
-}
declaration :
    { portDocumentationComment : Maybe (GrenSyntax.Node String)
    , comments : List (GrenSyntax.Node String)
    }
    -> GrenSyntax.Declaration
    -> Print
declaration syntaxComments syntaxDeclaration =
    case syntaxDeclaration of
        GrenSyntax.ValueOrFunctionDeclaration syntaxExpressionDeclaration ->
            declarationExpression syntaxComments.comments syntaxExpressionDeclaration

        GrenSyntax.AliasDeclaration syntaxTypeAliasDeclaration ->
            declarationTypeAlias syntaxComments.comments syntaxTypeAliasDeclaration

        GrenSyntax.ChoiceTypeDeclaration syntaxChoiceTypeDeclaration ->
            declarationChoiceType syntaxComments.comments syntaxChoiceTypeDeclaration

        GrenSyntax.PortDeclaration signature ->
            declarationPort
                { documentationComment = syntaxComments.portDocumentationComment
                , comments = syntaxComments.comments
                }
                signature

        GrenSyntax.InfixDeclaration syntaxInfixDeclaration ->
            declarationInfix syntaxInfixDeclaration


{-| Print a type signature
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
    let
        typePrint : Print
        typePrint =
            typeNotParenthesized syntaxComments signature.typeAnnotation

        rangeBetweenNameAndType : GrenSyntax.Range
        rangeBetweenNameAndType =
            { start = signature.name |> GrenSyntax.nodeRange |> .end
            , end = signature.typeAnnotation |> GrenSyntax.nodeRange |> .start
            }
    in
    Print.exactly
        ((signature.name |> GrenSyntax.nodeValue)
            ++ " :"
        )
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                ((case commentsInRange rangeBetweenNameAndType syntaxComments of
                    [] ->
                        Print.spaceOrLinebreakIndented (Print.lineSpread typePrint)

                    comment0 :: comment1Up ->
                        Print.linebreakIndented
                            |> Print.followedBy
                                (comments (comment0 :: comment1Up))
                            |> Print.followedBy Print.linebreakIndented
                 )
                    |> Print.followedBy typePrint
                )
            )


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
    (case syntaxComments.documentationComment of
        Nothing ->
            printExactlyPortSpace

        Just documentationNode ->
            Print.exactly documentationNode.value
                |> Print.followedBy Print.linebreak
                |> Print.followedBy
                    (commentsBetweenDocumentationAndDeclaration
                        (commentsInRange
                            { start = documentationNode.range.start
                            , end =
                                signature.name
                                    |> GrenSyntax.nodeRange
                                    |> .start
                            }
                            syntaxComments.comments
                        )
                    )
                |> Print.followedBy printExactlyPortSpace
    )
        |> Print.followedBy (declarationSignature syntaxComments.comments signature)


{-| Print an [`GrenSyntax.TypeAlias`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-TypeAlias#TypeAlias) declaration
-}
declarationTypeAlias :
    List (GrenSyntax.Node String)
    -> GrenSyntax.TypeAliasDeclarationInfo
    -> Print
declarationTypeAlias syntaxComments syntaxTypeAliasDeclaration =
    let
        parameterPrintsWithCommentsBeforeReverse : List Print
        parameterPrintsWithCommentsBeforeReverse =
            syntaxTypeAliasDeclaration.generics
                |> List.foldl
                    (\parameterName soFar ->
                        let
                            parameterPrintedRange : GrenSyntax.Range
                            parameterPrintedRange =
                                parameterName |> GrenSyntax.nodeRange

                            parameterNamePrint : Print
                            parameterNamePrint =
                                Print.exactly (parameterName |> GrenSyntax.nodeValue)
                        in
                        { reverse =
                            (case
                                commentsInRange
                                    { start = soFar.endLocation, end = parameterPrintedRange.start }
                                    syntaxComments
                             of
                                [] ->
                                    parameterNamePrint

                                comment0 :: comment1Up ->
                                    let
                                        commentsCollapsible : { print : Print, lineSpread : Print.LineSpread }
                                        commentsCollapsible =
                                            collapsibleComments (comment0 :: comment1Up)
                                    in
                                    commentsCollapsible.print
                                        |> Print.followedBy
                                            (Print.spaceOrLinebreakIndented
                                                commentsCollapsible.lineSpread
                                            )
                                        |> Print.followedBy parameterNamePrint
                            )
                                :: soFar.reverse
                        , endLocation = parameterPrintedRange.end
                        }
                    )
                    { reverse = []
                    , endLocation =
                        syntaxTypeAliasDeclaration.name
                            |> GrenSyntax.nodeRange
                            |> .end
                    }
                |> .reverse

        parametersLineSpread : Print.LineSpread
        parametersLineSpread =
            parameterPrintsWithCommentsBeforeReverse |> Print.lineSpreadListMapAndCombine Print.lineSpread

        rangeBetweenParametersAndType : GrenSyntax.Range
        rangeBetweenParametersAndType =
            case syntaxTypeAliasDeclaration.generics of
                [] ->
                    { start =
                        syntaxTypeAliasDeclaration.name
                            |> GrenSyntax.nodeRange
                            |> .end
                    , end = syntaxTypeAliasDeclaration.typeAnnotation |> GrenSyntax.nodeRange |> .start
                    }

                parameter0 :: parameter1Up ->
                    { start =
                        listFilledLast parameter0 parameter1Up
                            |> GrenSyntax.nodeRange
                            |> .end
                    , end = syntaxTypeAliasDeclaration.typeAnnotation |> GrenSyntax.nodeRange |> .start
                    }

        aliasedTypePrint : Print
        aliasedTypePrint =
            typeNotParenthesized syntaxComments
                syntaxTypeAliasDeclaration.typeAnnotation
    in
    (case syntaxTypeAliasDeclaration.documentation of
        Nothing ->
            printExactlyTypeSpaceAlias

        Just documentationNode ->
            Print.exactly documentationNode.value
                |> Print.followedBy Print.linebreak
                |> Print.followedBy
                    (commentsBetweenDocumentationAndDeclaration
                        (commentsInRange
                            { start = documentationNode.range.start
                            , end =
                                syntaxTypeAliasDeclaration.name
                                    |> GrenSyntax.nodeRange
                                    |> .start
                            }
                            syntaxComments
                        )
                    )
                |> Print.followedBy printExactlyTypeSpaceAlias
    )
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.spaceOrLinebreakIndented parametersLineSpread
                    |> Print.followedBy
                        (Print.exactly (syntaxTypeAliasDeclaration.name |> GrenSyntax.nodeValue))
                    |> Print.followedBy
                        (Print.withIndentAtNextMultipleOf4
                            (parameterPrintsWithCommentsBeforeReverse
                                |> Print.listReverseAndMapAndFlatten
                                    (\parameterPrint ->
                                        Print.spaceOrLinebreakIndented parametersLineSpread
                                            |> Print.followedBy parameterPrint
                                    )
                            )
                        )
                    |> Print.followedBy (Print.spaceOrLinebreakIndented parametersLineSpread)
                    |> Print.followedBy printExactlyEquals
                    |> Print.followedBy
                        (Print.linebreakIndented
                            |> Print.followedBy
                                (case commentsInRange rangeBetweenParametersAndType syntaxComments of
                                    [] ->
                                        aliasedTypePrint

                                    comment0 :: comment1Up ->
                                        comments (comment0 :: comment1Up)
                                            |> Print.followedBy Print.linebreakIndented
                                            |> Print.followedBy aliasedTypePrint
                                )
                        )
                )
            )


{-| Print an [`GrenSyntax.Type`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Type#Type) declaration
-}
declarationChoiceType :
    List (GrenSyntax.Node String)
    -> GrenSyntax.ChoiceTypeDeclarationInfo
    -> Print
declarationChoiceType syntaxComments syntaxChoiceTypeDeclaration =
    let
        parameterPrints :
            { endLocation : GrenSyntax.Location
            , reverse : List Print
            }
        parameterPrints =
            syntaxChoiceTypeDeclaration.generics
                |> List.foldl
                    (\parameterName soFar ->
                        let
                            parameterPrintedRange : GrenSyntax.Range
                            parameterPrintedRange =
                                parameterName |> GrenSyntax.nodeRange

                            parameterNamePrint : Print
                            parameterNamePrint =
                                Print.exactly (parameterName |> GrenSyntax.nodeValue)
                        in
                        { reverse =
                            (case
                                commentsInRange
                                    { start = soFar.endLocation, end = parameterPrintedRange.start }
                                    syntaxComments
                             of
                                [] ->
                                    parameterNamePrint

                                comment0 :: comment1Up ->
                                    let
                                        commentsCollapsible : { print : Print, lineSpread : Print.LineSpread }
                                        commentsCollapsible =
                                            collapsibleComments (comment0 :: comment1Up)
                                    in
                                    commentsCollapsible.print
                                        |> Print.followedBy
                                            (Print.spaceOrLinebreakIndented
                                                commentsCollapsible.lineSpread
                                            )
                                        |> Print.followedBy parameterNamePrint
                            )
                                :: soFar.reverse
                        , endLocation = parameterPrintedRange.end
                        }
                    )
                    { reverse = []
                    , endLocation =
                        syntaxChoiceTypeDeclaration.name
                            |> GrenSyntax.nodeRange
                            |> .end
                    }

        parametersLineSpread : Print.LineSpread
        parametersLineSpread =
            parameterPrints.reverse |> Print.lineSpreadListMapAndCombine Print.lineSpread

        variantPrintsWithCommentsBeforeReverse : List Print
        variantPrintsWithCommentsBeforeReverse =
            syntaxChoiceTypeDeclaration.constructors
                |> List.foldl
                    (\variantNode soFar ->
                        let
                            variantPrint : Print
                            variantPrint =
                                construct
                                    { printArgumentParenthesizedIfSpaceSeparated =
                                        typeParenthesizedIfSpaceSeparated
                                    , lineSpreadMinimum = Print.SingleLine
                                    }
                                    syntaxComments
                                    { start = variantNode.value.name |> GrenSyntax.nodeValue
                                    , fullRange = variantNode.range
                                    , arguments =
                                        case variantNode.value.value of
                                            Nothing ->
                                                []

                                            Just value ->
                                                [ value ]
                                    }

                            commentsVariantPrint : Print
                            commentsVariantPrint =
                                case
                                    commentsInRange
                                        { start = soFar.endLocation
                                        , end = variantNode.value.name |> GrenSyntax.nodeRange |> .start
                                        }
                                        syntaxComments
                                of
                                    [] ->
                                        variantPrint

                                    comment0 :: comment1Up ->
                                        let
                                            commentsCollapsible : { print : Print, lineSpread : Print.LineSpread }
                                            commentsCollapsible =
                                                collapsibleComments (comment0 :: comment1Up)
                                        in
                                        commentsCollapsible.print
                                            |> Print.followedBy
                                                (Print.spaceOrLinebreakIndented
                                                    (commentsCollapsible.lineSpread
                                                        |> Print.lineSpreadMergeWith
                                                            (\() -> variantPrint |> Print.lineSpread)
                                                    )
                                                )
                                            |> Print.followedBy variantPrint
                        in
                        { reverse = commentsVariantPrint :: soFar.reverse
                        , endLocation = variantNode.range.end
                        }
                    )
                    { reverse = []
                    , endLocation = parameterPrints.endLocation
                    }
                |> .reverse
    in
    (case syntaxChoiceTypeDeclaration.documentation of
        Nothing ->
            printExactlyType

        Just documentationNode ->
            Print.exactly documentationNode.value
                |> Print.followedBy Print.linebreak
                |> Print.followedBy
                    (commentsBetweenDocumentationAndDeclaration
                        (commentsInRange
                            { start = documentationNode.range.start
                            , end =
                                syntaxChoiceTypeDeclaration.name
                                    |> GrenSyntax.nodeRange
                                    |> .start
                            }
                            syntaxComments
                        )
                    )
                |> Print.followedBy printExactlyType
    )
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.spaceOrLinebreakIndented parametersLineSpread
                    |> Print.followedBy
                        (Print.exactly
                            (syntaxChoiceTypeDeclaration.name |> GrenSyntax.nodeValue)
                        )
                    |> Print.followedBy
                        (Print.withIndentAtNextMultipleOf4
                            (parameterPrints.reverse
                                |> Print.listReverseAndMapAndFlatten
                                    (\parameterPrint ->
                                        Print.spaceOrLinebreakIndented parametersLineSpread
                                            |> Print.followedBy parameterPrint
                                    )
                            )
                        )
                    |> Print.followedBy Print.linebreakIndented
                    |> Print.followedBy printExactlyEqualsSpace
                    |> Print.followedBy
                        (variantPrintsWithCommentsBeforeReverse
                            |> Print.listReverseAndMapAndIntersperseAndFlatten
                                (\variantPrint -> Print.withIndentIncreasedBy 2 variantPrint)
                                printLinebreakIndentedVerticalBarSpace
                        )
                )
            )


{-| Print an [`GrenSyntax.Infix`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Infix#Infix) declaration
-}
declarationInfix : GrenSyntax.InfixDeclarationInfo -> Print
declarationInfix syntaxInfixDeclaration =
    Print.exactly
        ("infix "
            ++ infixDirection (syntaxInfixDeclaration.direction |> GrenSyntax.nodeValue)
            ++ " "
            ++ String.fromInt (syntaxInfixDeclaration.precedence |> GrenSyntax.nodeValue)
            ++ " ("
            ++ (syntaxInfixDeclaration.operator |> GrenSyntax.nodeValue)
            ++ ") = "
            ++ (syntaxInfixDeclaration.function |> GrenSyntax.nodeValue)
        )


infixDirection : GrenSyntax.InfixDirection -> String
infixDirection syntaxInfixDirection =
    case syntaxInfixDirection of
        GrenSyntax.Left ->
            "left "

        GrenSyntax.Right ->
            "right"

        GrenSyntax.Non ->
            "non  "


declarationExpressionImplementation :
    List (GrenSyntax.Node String)
    ->
        { name : GrenSyntax.Node String
        , parameters : List (GrenSyntax.Node GrenSyntax.Pattern)
        , expression : GrenSyntax.Node GrenSyntax.Expression
        }
    -> Print
declarationExpressionImplementation syntaxComments implementation =
    let
        parameterPrintsWithCommentsBefore :
            { endLocation : GrenSyntax.Location
            , reverse : List Print
            }
        parameterPrintsWithCommentsBefore =
            implementation.parameters
                |> List.foldl
                    (\parameterPattern soFar ->
                        let
                            parameterRange : GrenSyntax.Range
                            parameterRange =
                                parameterPattern |> GrenSyntax.nodeRange

                            parameterPrint : Print
                            parameterPrint =
                                patternParenthesizedIfSpaceSeparated syntaxComments parameterPattern
                        in
                        { reverse =
                            (case
                                commentsInRange
                                    { start = soFar.endLocation, end = parameterRange.start }
                                    syntaxComments
                             of
                                [] ->
                                    parameterPrint

                                comment0 :: comment1Up ->
                                    let
                                        commentsBefore : { print : Print, lineSpread : Print.LineSpread }
                                        commentsBefore =
                                            collapsibleComments (comment0 :: comment1Up)
                                    in
                                    commentsBefore.print
                                        |> Print.followedBy
                                            (Print.spaceOrLinebreakIndented
                                                (commentsBefore.lineSpread
                                                    |> Print.lineSpreadMergeWith
                                                        (\() -> parameterPrint |> Print.lineSpread)
                                                )
                                            )
                                        |> Print.followedBy parameterPrint
                            )
                                :: soFar.reverse
                        , endLocation = parameterRange.end
                        }
                    )
                    { reverse = []
                    , endLocation =
                        implementation.name
                            |> GrenSyntax.nodeRange
                            |> .end
                    }

        parametersLineSpread : Print.LineSpread
        parametersLineSpread =
            parameterPrintsWithCommentsBefore.reverse
                |> Print.lineSpreadListMapAndCombine
                    Print.lineSpread

        commentsBetweenParametersAndResult : List String
        commentsBetweenParametersAndResult =
            commentsInRange
                { start = parameterPrintsWithCommentsBefore.endLocation
                , end =
                    implementation.expression
                        |> GrenSyntax.nodeRange
                        |> .start
                }
                syntaxComments

        expressionPrint : Print
        expressionPrint =
            expressionNotParenthesized syntaxComments
                implementation.expression
    in
    Print.exactly (implementation.name |> GrenSyntax.nodeValue)
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (parameterPrintsWithCommentsBefore.reverse
                    |> Print.listReverseAndMapAndFlatten
                        (\parameterPrintWithCommentsBefore ->
                            Print.spaceOrLinebreakIndented parametersLineSpread
                                |> Print.followedBy parameterPrintWithCommentsBefore
                        )
                    |> Print.followedBy (Print.spaceOrLinebreakIndented parametersLineSpread)
                    |> Print.followedBy printExactlyEquals
                    |> Print.followedBy
                        (Print.linebreakIndented
                            |> Print.followedBy
                                (case commentsBetweenParametersAndResult of
                                    [] ->
                                        expressionPrint

                                    comment0 :: comment1Up ->
                                        comments (comment0 :: comment1Up)
                                            |> Print.followedBy Print.linebreakIndented
                                            |> Print.followedBy expressionPrint
                                )
                        )
                )
            )


commentsBetweenDocumentationAndDeclaration : List String -> Print
commentsBetweenDocumentationAndDeclaration syntaxComments =
    case syntaxComments of
        [] ->
            Print.empty

        comment0 :: comment1Up ->
            printLinebreakLinebreakLinebreak
                |> Print.followedBy
                    (moduleLevelComments (comment0 :: comment1Up))
                |> Print.followedBy printLinebreakLinebreak


{-| Print an [`GrenSyntax.Function`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Expression#Function) declaration
-}
declarationExpression :
    List (GrenSyntax.Node String)
    -> GrenSyntax.ValueOrFunctionDeclarationInfo
    -> Print
declarationExpression syntaxComments syntaxExpressionDeclaration =
    let
        implementationPrint : Print
        implementationPrint =
            declarationExpressionImplementation
                syntaxComments
                (syntaxExpressionDeclaration.declaration |> GrenSyntax.nodeValue)

        withoutDocumentationPrint : Print
        withoutDocumentationPrint =
            case syntaxExpressionDeclaration.signature of
                Nothing ->
                    implementationPrint

                Just signatureNode ->
                    let
                        commentsBetweenSignatureAndImplementationName : List String
                        commentsBetweenSignatureAndImplementationName =
                            commentsInRange
                                { start = signatureNode.range.end
                                , end =
                                    syntaxExpressionDeclaration.declaration
                                        |> GrenSyntax.nodeRange
                                        |> .start
                                }
                                syntaxComments
                    in
                    declarationSignature syntaxComments signatureNode.value
                        |> Print.followedBy Print.linebreak
                        |> Print.followedBy
                            (case commentsBetweenSignatureAndImplementationName of
                                [] ->
                                    implementationPrint

                                comment0 :: comment1Up ->
                                    printLinebreakLinebreakLinebreak
                                        |> Print.followedBy
                                            (moduleLevelComments (comment0 :: comment1Up))
                                        |> Print.followedBy printLinebreakLinebreak
                                        |> Print.followedBy implementationPrint
                            )
    in
    case syntaxExpressionDeclaration.documentation of
        Nothing ->
            withoutDocumentationPrint

        Just documentationNode ->
            Print.exactly documentationNode.value
                |> Print.followedBy Print.linebreak
                |> Print.followedBy
                    (commentsBetweenDocumentationAndDeclaration
                        (commentsInRange
                            { start = documentationNode.range.start
                            , end =
                                case syntaxExpressionDeclaration.signature of
                                    Nothing ->
                                        syntaxExpressionDeclaration.declaration |> GrenSyntax.nodeRange |> .start

                                    Just signatureNode ->
                                        signatureNode.range.start
                            }
                            syntaxComments
                        )
                    )
                |> Print.followedBy withoutDocumentationPrint


expressionParenthesized :
    List (GrenSyntax.Node String)
    -> GrenSyntax.Node GrenSyntax.Expression
    -> Print
expressionParenthesized syntaxComments expressionNode =
    parenthesized expressionNotParenthesized
        { notParenthesized = expressionNode |> expressionToNotParenthesized
        , fullRange = expressionNode |> GrenSyntax.nodeRange
        }
        syntaxComments


expressionIsSpaceSeparated : GrenSyntax.Expression -> Bool
expressionIsSpaceSeparated syntaxExpression =
    case syntaxExpression of
        GrenSyntax.ExpressionUnit ->
            False

        GrenSyntax.ExpressionCall application ->
            case application of
                [] ->
                    -- invalid syntax
                    False

                [ notActuallyAppliedNode ] ->
                    -- invalid syntax
                    expressionIsSpaceSeparated notActuallyAppliedNode.value

                _ :: _ :: _ ->
                    True

        GrenSyntax.ExpressionInfixOperation _ ->
            True

        GrenSyntax.ExpressionReference _ ->
            False

        GrenSyntax.ExpressionIfThenElse _ ->
            True

        GrenSyntax.ExpressionOperatorFunction _ ->
            False

        GrenSyntax.ExpressionInteger _ ->
            False

        GrenSyntax.ExpressionHex _ ->
            False

        GrenSyntax.ExpressionFloat _ ->
            False

        GrenSyntax.ExpressionNegation _ ->
            False

        GrenSyntax.ExpressionString _ ->
            False

        GrenSyntax.ExpressionChar _ ->
            False

        GrenSyntax.ExpressionParenthesized inParensNode ->
            expressionIsSpaceSeparated inParensNode.value

        GrenSyntax.ExpressionLetIn _ ->
            True

        GrenSyntax.ExpressionCaseOf _ ->
            True

        GrenSyntax.ExpressionLambda _ ->
            True

        GrenSyntax.ExpressionRecord _ ->
            False

        GrenSyntax.ExpressionArray _ ->
            False

        GrenSyntax.ExpressionRecordAccess _ ->
            False

        GrenSyntax.ExpressionRecordAccessFunction _ ->
            False

        GrenSyntax.ExpressionRecordUpdate _ ->
            False


expressionParenthesizedIfSpaceSeparated :
    List (GrenSyntax.Node String)
    -> GrenSyntax.Node GrenSyntax.Expression
    -> Print
expressionParenthesizedIfSpaceSeparated syntaxComments expressionNode =
    if expressionIsSpaceSeparated (expressionNode |> GrenSyntax.nodeValue) then
        expressionParenthesized syntaxComments expressionNode

    else
        expressionNotParenthesized syntaxComments expressionNode


{-| Print an [`GrenSyntax.Expression`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Expression#Expression)
-}
expressionNotParenthesized :
    List (GrenSyntax.Node String)
    -> GrenSyntax.Node GrenSyntax.Expression
    -> Print
expressionNotParenthesized syntaxComments syntaxExpressionNode =
    -- IGNORE TCO
    case syntaxExpressionNode.value of
        GrenSyntax.ExpressionUnit ->
            printExactlyCurlyBraceOpeningCurlyBraceClosing

        GrenSyntax.ExpressionCall application ->
            case application of
                [] ->
                    -- invalid syntax
                    Print.empty

                [ notAppliedAfterAll ] ->
                    -- invalid syntax
                    expressionNotParenthesized syntaxComments notAppliedAfterAll

                applied :: argument0 :: argument1Up ->
                    expressionCall syntaxComments
                        { fullRange = syntaxExpressionNode.range
                        , applied = applied
                        , argument0 = argument0
                        , argument1Up = argument1Up
                        }

        GrenSyntax.ExpressionInfixOperation infixOperation ->
            expressionOperation syntaxComments
                { fullRange = syntaxExpressionNode.range
                , operator = infixOperation.operator
                , left = infixOperation.left
                , right = infixOperation.right
                }

        GrenSyntax.ExpressionReference reference ->
            Print.exactly
                (qualifiedReference reference)

        GrenSyntax.ExpressionIfThenElse ifThenElse ->
            expressionIfThenElse syntaxComments
                { fullRange = syntaxExpressionNode.range
                , condition = ifThenElse.condition
                , conditionLineSpreadMinimum = Print.SingleLine
                , onTrue = ifThenElse.onTrue
                , onFalse = ifThenElse.onFalse
                }

        GrenSyntax.ExpressionOperatorFunction operatorSymbol ->
            Print.exactly ("(" ++ operatorSymbol ++ ")")

        GrenSyntax.ExpressionInteger int ->
            Print.exactly (intLiteral int)

        GrenSyntax.ExpressionHex int ->
            Print.exactly (hexLiteral int)

        GrenSyntax.ExpressionFloat float ->
            Print.exactly (floatLiteral float)

        GrenSyntax.ExpressionNegation negated ->
            printExpressionNegation syntaxComments negated

        GrenSyntax.ExpressionString string ->
            stringLiteral string

        GrenSyntax.ExpressionChar char ->
            Print.exactly (charLiteral char)

        GrenSyntax.ExpressionParenthesized inParens ->
            let
                commentsBeforeInParens : List String
                commentsBeforeInParens =
                    commentsInRange
                        { start = syntaxExpressionNode.range.start
                        , end = inParens |> GrenSyntax.nodeRange |> .start
                        }
                        syntaxComments

                commentsAfterInParens : List String
                commentsAfterInParens =
                    commentsInRange
                        { start = inParens |> GrenSyntax.nodeRange |> .end
                        , end = syntaxExpressionNode.range.end
                        }
                        syntaxComments
            in
            case ( commentsBeforeInParens, commentsAfterInParens ) of
                ( [], [] ) ->
                    expressionNotParenthesized syntaxComments inParens

                _ ->
                    parenthesized expressionNotParenthesized
                        { notParenthesized = inParens |> expressionToNotParenthesized
                        , fullRange = syntaxExpressionNode.range
                        }
                        syntaxComments

        GrenSyntax.ExpressionLetIn syntaxLetIn ->
            case syntaxLetIn.declarations of
                [] ->
                    -- invalid syntax
                    expressionNotParenthesized syntaxComments syntaxLetIn.result

                letDeclaration0 :: letDeclaration1Up ->
                    expressionLetIn syntaxComments
                        { fullRange = syntaxExpressionNode.range
                        , letDeclaration0 = letDeclaration0
                        , letDeclaration1Up = letDeclaration1Up
                        , result = syntaxLetIn.result
                        }

        GrenSyntax.ExpressionCaseOf syntaxCaseOf ->
            expressionCaseOf syntaxComments
                { fullRange = syntaxExpressionNode.range
                , expression = syntaxCaseOf.expression
                , cases = syntaxCaseOf.cases
                }

        GrenSyntax.ExpressionLambda syntaxLambda ->
            expressionLambda syntaxComments
                { range = syntaxExpressionNode.range, value = syntaxLambda }

        GrenSyntax.ExpressionRecord fields ->
            recordLiteral
                { printValueNotParenthesized = expressionNotParenthesized
                , nameValueSeparator = "="
                }
                syntaxComments
                { fullRange = syntaxExpressionNode.range
                , fields = fields
                }

        GrenSyntax.ExpressionArray elements ->
            expressionList syntaxComments
                { fullRange = syntaxExpressionNode.range
                , elements = elements
                }

        GrenSyntax.ExpressionRecordAccess grenExpressionRecordAccess ->
            expressionParenthesizedIfSpaceSeparated syntaxComments
                grenExpressionRecordAccess.record
                |> Print.followedBy
                    (Print.exactly ("." ++ grenExpressionRecordAccess.field.value))

        GrenSyntax.ExpressionRecordAccessFunction dotFieldName ->
            Print.exactly ("." ++ (dotFieldName |> String.replace "." ""))

        GrenSyntax.ExpressionRecordUpdate grenExpressionRecordUpdate ->
            expressionRecordUpdate syntaxComments
                { fullRange = syntaxExpressionNode.range
                , record = grenExpressionRecordUpdate.record
                , fields = grenExpressionRecordUpdate.fields
                }


printExpressionNegation :
    List (GrenSyntax.Node String)
    -> GrenSyntax.Node GrenSyntax.Expression
    -> Print
printExpressionNegation syntaxComments negated =
    if negated |> expressionIsBase10Zero then
        printExactlyZero

    else if negated |> expressionIsBase16Zero then
        printExactlyZeroXZeroZero

    else
        let
            negatedNotParenthesized =
                negated |> expressionToNotParenthesized
        in
        case negatedNotParenthesized.value of
            GrenSyntax.ExpressionNegation doublyNegated ->
                printExactlyMinus
                    |> Print.followedBy
                        (expressionParenthesized syntaxComments
                            { range = negatedNotParenthesized.range, value = GrenSyntax.ExpressionNegation doublyNegated }
                        )

            _ ->
                printExactlyMinus
                    |> Print.followedBy
                        (expressionParenthesizedIfSpaceSeparated syntaxComments
                            negatedNotParenthesized
                        )


expressionIsBase10Zero : GrenSyntax.Node GrenSyntax.Expression -> Bool
expressionIsBase10Zero expression =
    case expression |> expressionToNotParenthesized |> GrenSyntax.nodeValue of
        GrenSyntax.ExpressionInteger 0 ->
            True

        GrenSyntax.ExpressionNegation doublyNegated ->
            expressionIsBase10Zero doublyNegated

        _ ->
            False


expressionIsBase16Zero : GrenSyntax.Node GrenSyntax.Expression -> Bool
expressionIsBase16Zero expression =
    case expression |> expressionToNotParenthesized |> GrenSyntax.nodeValue of
        GrenSyntax.ExpressionHex 0 ->
            True

        GrenSyntax.ExpressionNegation doublyNegated ->
            expressionIsBase16Zero doublyNegated

        _ ->
            False


floatLiteral : Float -> String
floatLiteral float =
    if (float |> Basics.truncate |> Basics.toFloat) == float then
        String.fromFloat float ++ ".0"

    else
        String.fromFloat float


expressionCall :
    List (GrenSyntax.Node String)
    ->
        { fullRange : GrenSyntax.Range
        , applied : GrenSyntax.Node GrenSyntax.Expression
        , argument0 : GrenSyntax.Node GrenSyntax.Expression
        , argument1Up : List (GrenSyntax.Node GrenSyntax.Expression)
        }
    -> Print
expressionCall syntaxComments syntaxCall =
    let
        appliedPrint : Print
        appliedPrint =
            expressionParenthesizedIfSpaceSeparated syntaxComments
                syntaxCall.applied

        commentsBeforeArgument0 : List String
        commentsBeforeArgument0 =
            commentsInRange
                { start = syntaxCall.applied |> GrenSyntax.nodeRange |> .end
                , end =
                    syntaxCall.argument0
                        |> GrenSyntax.nodeRange
                        |> .start
                }
                syntaxComments

        collapsibleCommentsBeforeArgument0 : { print : Print, lineSpread : Print.LineSpread }
        collapsibleCommentsBeforeArgument0 =
            commentsBeforeArgument0 |> collapsibleComments

        argument0Print : Print
        argument0Print =
            expressionParenthesizedIfSpaceSeparated syntaxComments
                syntaxCall.argument0

        argument1UpPrintsWithCommentsBeforeReverse : List Print
        argument1UpPrintsWithCommentsBeforeReverse =
            syntaxCall.argument1Up
                |> List.foldl
                    (\argument soFar ->
                        let
                            print : Print
                            print =
                                expressionParenthesizedIfSpaceSeparated syntaxComments argument
                        in
                        { reverse =
                            (case
                                commentsInRange
                                    { start = soFar.endLocation
                                    , end = argument |> GrenSyntax.nodeRange |> .start
                                    }
                                    syntaxComments
                             of
                                [] ->
                                    print

                                comment0 :: comment1Up ->
                                    let
                                        commentsBefore : { print : Print, lineSpread : Print.LineSpread }
                                        commentsBefore =
                                            collapsibleComments (comment0 :: comment1Up)
                                    in
                                    commentsBefore.print
                                        |> Print.followedBy
                                            (Print.spaceOrLinebreakIndented
                                                (commentsBefore.lineSpread
                                                    |> Print.lineSpreadMergeWith
                                                        (\() -> print |> Print.lineSpread)
                                                )
                                            )
                                        |> Print.followedBy print
                            )
                                :: soFar.reverse
                        , endLocation = argument |> GrenSyntax.nodeRange |> .end
                        }
                    )
                    { reverse = []
                    , endLocation =
                        syntaxCall.argument0
                            |> GrenSyntax.nodeRange
                            |> .end
                    }
                |> .reverse

        argument0LineSpread : Print.LineSpread
        argument0LineSpread =
            appliedPrint
                |> Print.lineSpread
                |> Print.lineSpreadMergeWith
                    (\() ->
                        lineSpreadBetweenNodes
                            syntaxCall.applied
                            syntaxCall.argument0
                    )
                |> Print.lineSpreadMergeWithStrict
                    collapsibleCommentsBeforeArgument0.lineSpread
                |> Print.lineSpreadMergeWith
                    (\() -> argument0Print |> Print.lineSpread)

        argument1UpLineSpread : Print.LineSpread
        argument1UpLineSpread =
            lineSpreadInRange syntaxCall.fullRange
                |> Print.lineSpreadMergeWithStrict argument0LineSpread
                |> Print.lineSpreadMergeWith
                    (\() ->
                        argument1UpPrintsWithCommentsBeforeReverse
                            |> Print.lineSpreadListMapAndCombine Print.lineSpread
                    )
    in
    appliedPrint
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.spaceOrLinebreakIndented argument0LineSpread
                    |> Print.followedBy
                        (case commentsBeforeArgument0 of
                            [] ->
                                argument0Print

                            _ :: _ ->
                                collapsibleCommentsBeforeArgument0.print
                                    |> Print.followedBy
                                        (Print.spaceOrLinebreakIndented
                                            (collapsibleCommentsBeforeArgument0.lineSpread
                                                |> Print.lineSpreadMergeWith
                                                    (\() -> argument0Print |> Print.lineSpread)
                                            )
                                        )
                                    |> Print.followedBy argument0Print
                        )
                    |> Print.followedBy
                        (argument1UpPrintsWithCommentsBeforeReverse
                            |> Print.listReverseAndMapAndFlatten
                                (\argumentPrintWithCommentsBefore ->
                                    Print.spaceOrLinebreakIndented argument1UpLineSpread
                                        |> Print.followedBy
                                            argumentPrintWithCommentsBefore
                                )
                        )
                )
            )


expressionOperation :
    List (GrenSyntax.Node String)
    ->
        { fullRange : GrenSyntax.Range
        , left : GrenSyntax.Node GrenSyntax.Expression
        , operator : String
        , right : GrenSyntax.Node GrenSyntax.Expression
        }
    -> Print
expressionOperation syntaxComments syntaxOperation =
    let
        operationExpanded :
            { leftest : GrenSyntax.Node GrenSyntax.Expression
            , beforeRightestOperatorExpressionChain :
                List
                    { operator : String
                    , expression : GrenSyntax.Node GrenSyntax.Expression
                    }
            , rightestOperator : String
            , rightestExpression : GrenSyntax.Node GrenSyntax.Expression
            }
        operationExpanded =
            expressionOperationExpand syntaxOperation.left
                syntaxOperation.operator
                syntaxOperation.right

        beforeRightestPrintsAndComments :
            { reverse :
                List
                    { operator : String
                    , expression : GrenSyntax.Node GrenSyntax.Expression
                    , maybeCommentsBeforeExpression :
                        Maybe { print : Print, lineSpread : Print.LineSpread }
                    }
            , endLocation : GrenSyntax.Location
            }
        beforeRightestPrintsAndComments =
            operationExpanded.beforeRightestOperatorExpressionChain
                |> List.foldl
                    (\operatorAndExpressionBeforeRightest soFar ->
                        let
                            expressionRange : GrenSyntax.Range
                            expressionRange =
                                operatorAndExpressionBeforeRightest.expression
                                    |> GrenSyntax.nodeRange

                            commentsBefore : List String
                            commentsBefore =
                                commentsInRange
                                    { start = soFar.endLocation, end = expressionRange.start }
                                    syntaxComments
                        in
                        { endLocation = expressionRange.end
                        , reverse =
                            { operator = operatorAndExpressionBeforeRightest.operator
                            , expression = operatorAndExpressionBeforeRightest.expression
                            , maybeCommentsBeforeExpression =
                                case commentsBefore of
                                    [] ->
                                        Nothing

                                    comment0 :: comment1Up ->
                                        Just (collapsibleComments (comment0 :: comment1Up))
                            }
                                :: soFar.reverse
                        }
                    )
                    { endLocation =
                        operationExpanded.leftest |> GrenSyntax.nodeRange |> .end
                    , reverse = []
                    }

        commentsBeforeRightestExpression : List String
        commentsBeforeRightestExpression =
            commentsInRange
                { start = beforeRightestPrintsAndComments.endLocation
                , end =
                    operationExpanded.rightestExpression
                        |> GrenSyntax.nodeRange
                        |> .start
                }
                syntaxComments

        commentsCollapsibleBeforeRightestExpression : { print : Print, lineSpread : Print.LineSpread }
        commentsCollapsibleBeforeRightestExpression =
            collapsibleComments commentsBeforeRightestExpression

        leftestPrint : Print
        leftestPrint =
            expressionParenthesizedIfSpaceSeparatedExceptApplication syntaxComments
                operationExpanded.leftest

        lineSpread : Print.LineSpread
        lineSpread =
            lineSpreadInRange syntaxOperation.fullRange
                |> Print.lineSpreadMergeWith
                    (\() ->
                        beforeRightestPrintsAndComments.reverse
                            |> Print.lineSpreadListMapAndCombine
                                (\c ->
                                    c.maybeCommentsBeforeExpression
                                        |> maybeLineSpread .lineSpread
                                )
                    )
                |> Print.lineSpreadMergeWithStrict
                    commentsCollapsibleBeforeRightestExpression.lineSpread

        beforeRightestOperatorExpressionChainWithPreviousLineSpread :
            { previousLineSpread : Print.LineSpread
            , rightToLeft :
                List
                    { operator : String
                    , expression : GrenSyntax.Node GrenSyntax.Expression
                    , expressionPrint : Print
                    , maybeCommentsBeforeExpression : Maybe { print : Print, lineSpread : Print.LineSpread }
                    , previousLineSpread : Print.LineSpread
                    }
            }
        beforeRightestOperatorExpressionChainWithPreviousLineSpread =
            beforeRightestPrintsAndComments.reverse
                |> List.foldr
                    (\operatorExpression soFar ->
                        let
                            expressionPrint : Print
                            expressionPrint =
                                expressionParenthesizedIfSpaceSeparatedExceptApplication syntaxComments
                                    operatorExpression.expression
                        in
                        { previousLineSpread = Print.lineSpread expressionPrint
                        , rightToLeft =
                            { operator = operatorExpression.operator
                            , expression = operatorExpression.expression
                            , expressionPrint = expressionPrint
                            , maybeCommentsBeforeExpression = operatorExpression.maybeCommentsBeforeExpression
                            , previousLineSpread = soFar.previousLineSpread
                            }
                                :: soFar.rightToLeft
                        }
                    )
                    { previousLineSpread = leftestPrint |> Print.lineSpread
                    , rightToLeft = []
                    }

        rightestOperatorExpressionPrint : Print
        rightestOperatorExpressionPrint =
            case operationExpanded.rightestOperator of
                "<|" ->
                    let
                        expressionPrint : Print
                        expressionPrint =
                            expressionParenthesizedIfSpaceSeparatedExceptApplicationAndLambda syntaxComments
                                operationExpanded.rightestExpression
                    in
                    Print.spaceOrLinebreakIndented
                        beforeRightestOperatorExpressionChainWithPreviousLineSpread.previousLineSpread
                        |> Print.followedBy printExactlyLessThanVerticalBar
                        |> Print.followedBy
                            (Print.withIndentAtNextMultipleOf4
                                (Print.spaceOrLinebreakIndented lineSpread
                                    |> Print.followedBy
                                        (case commentsBeforeRightestExpression of
                                            [] ->
                                                expressionPrint

                                            _ :: _ ->
                                                commentsCollapsibleBeforeRightestExpression.print
                                                    |> Print.followedBy
                                                        (Print.spaceOrLinebreakIndented
                                                            (commentsCollapsibleBeforeRightestExpression.lineSpread
                                                                |> Print.lineSpreadMergeWith
                                                                    (\() -> expressionPrint |> Print.lineSpread)
                                                            )
                                                        )
                                                    |> Print.followedBy expressionPrint
                                        )
                                )
                            )

                nonApLOperator ->
                    let
                        expressionPrint : Print
                        expressionPrint =
                            expressionParenthesizedIfSpaceSeparatedExceptApplication syntaxComments
                                operationExpanded.rightestExpression
                    in
                    Print.withIndentAtNextMultipleOf4
                        (Print.spaceOrLinebreakIndented lineSpread
                            |> Print.followedBy (Print.exactly (nonApLOperator ++ " "))
                            |> Print.followedBy
                                (Print.withIndentIncreasedBy (String.length nonApLOperator + 1)
                                    (case commentsBeforeRightestExpression of
                                        [] ->
                                            expressionPrint

                                        _ :: _ ->
                                            commentsCollapsibleBeforeRightestExpression.print
                                                |> Print.followedBy
                                                    (Print.spaceOrLinebreakIndented
                                                        (commentsCollapsibleBeforeRightestExpression.lineSpread
                                                            |> Print.lineSpreadMergeWith
                                                                (\() -> expressionPrint |> Print.lineSpread)
                                                        )
                                                    )
                                                |> Print.followedBy expressionPrint
                                    )
                                )
                        )
    in
    leftestPrint
        |> Print.followedBy
            (beforeRightestOperatorExpressionChainWithPreviousLineSpread.rightToLeft
                |> List.foldl
                    (\operatorExpression chainRightPrint ->
                        case operatorExpression.operator of
                            "<|" ->
                                Print.spaceOrLinebreakIndented operatorExpression.previousLineSpread
                                    |> Print.followedBy printExactlyLessThanVerticalBar
                                    |> Print.followedBy
                                        (Print.withIndentAtNextMultipleOf4
                                            (Print.spaceOrLinebreakIndented lineSpread
                                                |> Print.followedBy
                                                    (case operatorExpression.maybeCommentsBeforeExpression of
                                                        Nothing ->
                                                            operatorExpression.expressionPrint

                                                        Just commentsBeforeExpression ->
                                                            commentsBeforeExpression.print
                                                                |> Print.followedBy
                                                                    (Print.spaceOrLinebreakIndented
                                                                        (commentsBeforeExpression.lineSpread
                                                                            |> Print.lineSpreadMergeWith
                                                                                (\() -> operatorExpression.expressionPrint |> Print.lineSpread)
                                                                        )
                                                                    )
                                                                |> Print.followedBy operatorExpression.expressionPrint
                                                    )
                                                |> Print.followedBy chainRightPrint
                                            )
                                        )

                            nonApLOperator ->
                                Print.withIndentAtNextMultipleOf4
                                    (Print.spaceOrLinebreakIndented lineSpread
                                        |> Print.followedBy (Print.exactly (nonApLOperator ++ " "))
                                        |> Print.followedBy
                                            (Print.withIndentIncreasedBy (String.length nonApLOperator + 1)
                                                (case operatorExpression.maybeCommentsBeforeExpression of
                                                    Nothing ->
                                                        operatorExpression.expressionPrint

                                                    Just commentsBeforeExpression ->
                                                        commentsBeforeExpression.print
                                                            |> Print.followedBy
                                                                (Print.spaceOrLinebreakIndented
                                                                    (commentsBeforeExpression.lineSpread
                                                                        |> Print.lineSpreadMergeWith
                                                                            (\() -> operatorExpression.expressionPrint |> Print.lineSpread)
                                                                    )
                                                                )
                                                            |> Print.followedBy
                                                                operatorExpression.expressionPrint
                                                )
                                            )
                                    )
                                    |> Print.followedBy chainRightPrint
                    )
                    rightestOperatorExpressionPrint
            )


expressionOperationExpand :
    GrenSyntax.Node GrenSyntax.Expression
    -> String
    -> GrenSyntax.Node GrenSyntax.Expression
    ->
        { leftest : GrenSyntax.Node GrenSyntax.Expression
        , beforeRightestOperatorExpressionChain :
            List
                { operator : String
                , expression : GrenSyntax.Node GrenSyntax.Expression
                }
        , rightestOperator : String
        , rightestExpression : GrenSyntax.Node GrenSyntax.Expression
        }
expressionOperationExpand left operator right =
    let
        rightExpanded :
            { beforeRightestOperatorExpressionChain :
                List { operator : String, expression : GrenSyntax.Node GrenSyntax.Expression }
            , rightestOperator : String
            , rightestExpression : GrenSyntax.Node GrenSyntax.Expression
            }
        rightExpanded =
            case right.value of
                GrenSyntax.ExpressionInfixOperation rightInfixOperation ->
                    let
                        rightOperationExpanded :
                            { leftest : GrenSyntax.Node GrenSyntax.Expression
                            , beforeRightestOperatorExpressionChain :
                                List
                                    { operator : String
                                    , expression : GrenSyntax.Node GrenSyntax.Expression
                                    }
                            , rightestOperator : String
                            , rightestExpression : GrenSyntax.Node GrenSyntax.Expression
                            }
                        rightOperationExpanded =
                            expressionOperationExpand
                                rightInfixOperation.left
                                rightInfixOperation.operator
                                rightInfixOperation.right
                    in
                    { beforeRightestOperatorExpressionChain =
                        { operator = operator, expression = rightOperationExpanded.leftest }
                            :: rightOperationExpanded.beforeRightestOperatorExpressionChain
                    , rightestOperator = rightOperationExpanded.rightestOperator
                    , rightestExpression = rightOperationExpanded.rightestExpression
                    }

                _ ->
                    { beforeRightestOperatorExpressionChain = []
                    , rightestOperator = operator
                    , rightestExpression = right
                    }
    in
    case left.value of
        GrenSyntax.ExpressionInfixOperation leftInfixOperation ->
            let
                leftOperationExpanded :
                    { leftest : GrenSyntax.Node GrenSyntax.Expression
                    , beforeRightestOperatorExpressionChain :
                        List
                            { operator : String
                            , expression : GrenSyntax.Node GrenSyntax.Expression
                            }
                    , rightestOperator : String
                    , rightestExpression : GrenSyntax.Node GrenSyntax.Expression
                    }
                leftOperationExpanded =
                    expressionOperationExpand
                        leftInfixOperation.left
                        leftInfixOperation.operator
                        leftInfixOperation.right
            in
            { leftest = leftOperationExpanded.leftest
            , beforeRightestOperatorExpressionChain =
                leftOperationExpanded.beforeRightestOperatorExpressionChain
                    ++ ({ operator = leftOperationExpanded.rightestOperator
                        , expression = leftOperationExpanded.rightestExpression
                        }
                            :: rightExpanded.beforeRightestOperatorExpressionChain
                       )
            , rightestOperator = rightExpanded.rightestOperator
            , rightestExpression = rightExpanded.rightestExpression
            }

        _ ->
            { leftest = left
            , beforeRightestOperatorExpressionChain = rightExpanded.beforeRightestOperatorExpressionChain
            , rightestOperator = rightExpanded.rightestOperator
            , rightestExpression = rightExpanded.rightestExpression
            }


expressionIsSpaceSeparatedExceptApplication : GrenSyntax.Node GrenSyntax.Expression -> Bool
expressionIsSpaceSeparatedExceptApplication expressionNode =
    if expressionIsSpaceSeparated (expressionNode |> GrenSyntax.nodeValue) then
        case expressionNode |> expressionToNotParenthesized |> .value of
            GrenSyntax.ExpressionCall _ ->
                False

            _ ->
                True

    else
        False


expressionParenthesizedIfSpaceSeparatedExceptApplication :
    List (GrenSyntax.Node String)
    -> GrenSyntax.Node GrenSyntax.Expression
    -> Print
expressionParenthesizedIfSpaceSeparatedExceptApplication syntaxComments expressionNode =
    if expressionNode |> expressionIsSpaceSeparatedExceptApplication then
        expressionParenthesized syntaxComments expressionNode

    else
        expressionNotParenthesized syntaxComments expressionNode


expressionParenthesizedIfSpaceSeparatedExceptApplicationAndLambda :
    List (GrenSyntax.Node String)
    -> GrenSyntax.Node GrenSyntax.Expression
    -> Print
expressionParenthesizedIfSpaceSeparatedExceptApplicationAndLambda syntaxComments expressionNode =
    if expressionNode |> GrenSyntax.nodeValue |> expressionIsSpaceSeparated then
        case expressionNode |> expressionToNotParenthesized |> GrenSyntax.nodeValue of
            GrenSyntax.ExpressionCall _ ->
                expressionNotParenthesized syntaxComments expressionNode

            GrenSyntax.ExpressionLambda _ ->
                expressionNotParenthesized syntaxComments expressionNode

            _ ->
                expressionParenthesized syntaxComments expressionNode

    else
        expressionNotParenthesized syntaxComments expressionNode


expressionList :
    List (GrenSyntax.Node String)
    ->
        { elements : List (GrenSyntax.Node GrenSyntax.Expression)
        , fullRange : GrenSyntax.Range
        }
    -> Print
expressionList syntaxComments syntaxList =
    case syntaxList.elements of
        [] ->
            printExactlySquareOpening
                |> Print.followedBy
                    (case commentsInRange syntaxList.fullRange syntaxComments of
                        [] ->
                            printExactlySquareClosing

                        comment0 :: comment1Up ->
                            let
                                commentsCollapsed : { print : Print, lineSpread : Print.LineSpread }
                                commentsCollapsed =
                                    collapsibleComments (comment0 :: comment1Up)
                            in
                            Print.withIndentIncreasedBy 1
                                commentsCollapsed.print
                                |> Print.followedBy
                                    (Print.emptyOrLinebreakIndented
                                        commentsCollapsed.lineSpread
                                    )
                                |> Print.followedBy printExactlySquareClosing
                    )

        element0 :: element1Up ->
            let
                elementPrintsWithCommentsBefore :
                    { endLocation : GrenSyntax.Location
                    , reverse : List Print
                    }
                elementPrintsWithCommentsBefore =
                    (element0 :: element1Up)
                        |> List.foldl
                            (\elementNode soFar ->
                                let
                                    print : Print
                                    print =
                                        expressionNotParenthesized syntaxComments
                                            elementNode
                                in
                                { endLocation = elementNode.range.end
                                , reverse =
                                    (case
                                        commentsInRange { start = soFar.endLocation, end = elementNode.range.start }
                                            syntaxComments
                                     of
                                        [] ->
                                            print

                                        comment0 :: comment1Up ->
                                            let
                                                commentsBefore : { print : Print, lineSpread : Print.LineSpread }
                                                commentsBefore =
                                                    collapsibleComments (comment0 :: comment1Up)
                                            in
                                            commentsBefore.print
                                                |> Print.followedBy
                                                    (Print.spaceOrLinebreakIndented
                                                        (commentsBefore.lineSpread
                                                            |> Print.lineSpreadMergeWith
                                                                (\() -> print |> Print.lineSpread)
                                                        )
                                                    )
                                                |> Print.followedBy print
                                    )
                                        :: soFar.reverse
                                }
                            )
                            { endLocation = syntaxList.fullRange.start
                            , reverse = []
                            }

                commentsAfterElements : List String
                commentsAfterElements =
                    commentsInRange { start = elementPrintsWithCommentsBefore.endLocation, end = syntaxList.fullRange.end } syntaxComments

                lineSpread : Print.LineSpread
                lineSpread =
                    lineSpreadInRange syntaxList.fullRange
                        |> Print.lineSpreadMergeWith
                            (\() ->
                                elementPrintsWithCommentsBefore.reverse
                                    |> Print.lineSpreadListMapAndCombine Print.lineSpread
                            )
                        |> Print.lineSpreadMergeWith
                            (\() ->
                                case commentsAfterElements of
                                    [] ->
                                        Print.SingleLine

                                    _ :: _ ->
                                        Print.MultipleLines
                            )
            in
            printExactlySquareOpeningSpace
                |> Print.followedBy
                    (elementPrintsWithCommentsBefore.reverse
                        |> Print.listReverseAndMapAndIntersperseAndFlatten
                            (\elementPrintWithCommentsBefore ->
                                Print.withIndentIncreasedBy 2
                                    elementPrintWithCommentsBefore
                            )
                            (Print.emptyOrLinebreakIndented lineSpread
                                |> Print.followedBy printExactlyCommaSpace
                            )
                    )
                |> Print.followedBy
                    (case commentsAfterElements of
                        [] ->
                            Print.spaceOrLinebreakIndented lineSpread

                        comment0 :: comment1Up ->
                            Print.linebreak
                                |> Print.followedBy (Print.spaceOrLinebreakIndented lineSpread)
                                |> Print.followedBy (comments (comment0 :: comment1Up))
                                |> Print.followedBy (Print.spaceOrLinebreakIndented lineSpread)
                    )
                |> Print.followedBy printExactlySquareClosing


expressionRecordUpdate :
    List (GrenSyntax.Node String)
    ->
        { fullRange : GrenSyntax.Range
        , record : GrenSyntax.Node GrenSyntax.Expression
        , fields :
            List
                (GrenSyntax.Node
                    { name : GrenSyntax.Node String
                    , value : GrenSyntax.Node GrenSyntax.Expression
                    }
                )
        }
    -> Print
expressionRecordUpdate syntaxComments syntaxRecordUpdate =
    let
        fieldPrintsWithCommentsBefore :
            { endLocation : GrenSyntax.Location
            , reverse : List Print
            }
        fieldPrintsWithCommentsBefore =
            syntaxRecordUpdate.fields
                |> List.foldl
                    (\fieldSyntaxNode soFar ->
                        let
                            valuePrint : Print
                            valuePrint =
                                expressionNotParenthesized syntaxComments
                                    fieldSyntaxNode.value.value
                        in
                        { endLocation = fieldSyntaxNode.value.value.range.end
                        , reverse =
                            (Print.withIndentIncreasedBy 2
                                (case
                                    commentsInRange
                                        { start = soFar.endLocation, end = fieldSyntaxNode.value.name.range.start }
                                        syntaxComments
                                 of
                                    [] ->
                                        Print.exactly (fieldSyntaxNode.value.name.value ++ " =")

                                    comment0 :: comment1Up ->
                                        let
                                            commentsBeforeName : { print : Print, lineSpread : Print.LineSpread }
                                            commentsBeforeName =
                                                collapsibleComments (comment0 :: comment1Up)
                                        in
                                        commentsBeforeName.print
                                            |> Print.followedBy
                                                (Print.spaceOrLinebreakIndented commentsBeforeName.lineSpread)
                                            |> Print.followedBy (Print.exactly (fieldSyntaxNode.value.name.value ++ " ="))
                                )
                                |> Print.followedBy
                                    (Print.withIndentAtNextMultipleOf4
                                        ((case
                                            commentsInRange
                                                { start = fieldSyntaxNode.value.name.range.start, end = fieldSyntaxNode.value.value.range.start }
                                                syntaxComments
                                          of
                                            [] ->
                                                Print.spaceOrLinebreakIndented
                                                    (lineSpreadBetweenRanges
                                                        fieldSyntaxNode.value.name.range
                                                        fieldSyntaxNode.value.value.range
                                                        |> Print.lineSpreadMergeWith (\() -> valuePrint |> Print.lineSpread)
                                                    )

                                            comment0 :: comment1Up ->
                                                let
                                                    commentsBeforeValue : { print : Print, lineSpread : Print.LineSpread }
                                                    commentsBeforeValue =
                                                        collapsibleComments (comment0 :: comment1Up)

                                                    layout : Print.Print
                                                    layout =
                                                        Print.spaceOrLinebreakIndented
                                                            (commentsBeforeValue.lineSpread
                                                                |> Print.lineSpreadMergeWith
                                                                    (\() ->
                                                                        lineSpreadBetweenRanges
                                                                            fieldSyntaxNode.value.name.range
                                                                            fieldSyntaxNode.value.value.range
                                                                    )
                                                                |> Print.lineSpreadMergeWith (\() -> valuePrint |> Print.lineSpread)
                                                            )
                                                in
                                                layout
                                                    |> Print.followedBy commentsBeforeValue.print
                                                    |> Print.followedBy layout
                                         )
                                            |> Print.followedBy valuePrint
                                        )
                                    )
                            )
                                :: soFar.reverse
                        }
                    )
                    { endLocation =
                        syntaxRecordUpdate.record
                            |> GrenSyntax.nodeRange
                            |> .end
                    , reverse = []
                    }

        commentsAfterFields : List String
        commentsAfterFields =
            commentsInRange
                { start = fieldPrintsWithCommentsBefore.endLocation
                , end = syntaxRecordUpdate.fullRange.end
                }
                syntaxComments

        maybeCommentsBeforeRecordVariable : Maybe { print : Print, lineSpread : Print.LineSpread }
        maybeCommentsBeforeRecordVariable =
            case
                commentsInRange
                    { start = syntaxRecordUpdate.fullRange.start
                    , end = syntaxRecordUpdate.record |> GrenSyntax.nodeRange |> .start
                    }
                    syntaxComments
            of
                [] ->
                    Nothing

                comment0 :: comment1Up ->
                    Just (collapsibleComments (comment0 :: comment1Up))

        lineSpread : Print.LineSpread
        lineSpread =
            lineSpreadInRange syntaxRecordUpdate.fullRange
                |> Print.lineSpreadMergeWith
                    (\() -> maybeCommentsBeforeRecordVariable |> maybeLineSpread .lineSpread)
                |> Print.lineSpreadMergeWith
                    (\() -> recordPrint |> Print.lineSpread)
                |> Print.lineSpreadMergeWith
                    (\() ->
                        case commentsAfterFields of
                            [] ->
                                Print.SingleLine

                            _ :: _ ->
                                Print.MultipleLines
                    )
                |> Print.lineSpreadMergeWith
                    (\() ->
                        fieldPrintsWithCommentsBefore.reverse
                            |> Print.lineSpreadListMapAndCombine Print.lineSpread
                    )

        recordPrint : Print
        recordPrint =
            expressionNotParenthesized syntaxComments
                syntaxRecordUpdate.record
    in
    printExactlyCurlyOpeningSpace
        |> Print.followedBy
            (Print.withIndentIncreasedBy 2
                (case maybeCommentsBeforeRecordVariable of
                    Nothing ->
                        recordPrint

                    Just commentsCollapsibleBeforeRecordVariable ->
                        commentsCollapsibleBeforeRecordVariable.print
                            |> Print.followedBy
                                (Print.spaceOrLinebreakIndented commentsCollapsibleBeforeRecordVariable.lineSpread)
                            |> Print.followedBy recordPrint
                )
            )
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.spaceOrLinebreakIndented lineSpread
                    |> Print.followedBy printExactlyVerticalBarSpace
                    |> Print.followedBy
                        (fieldPrintsWithCommentsBefore.reverse
                            |> Print.listReverseAndIntersperseAndFlatten
                                (Print.emptyOrLinebreakIndented lineSpread
                                    |> Print.followedBy printExactlyCommaSpace
                                )
                        )
                    |> -- indenting trailing comments is a rudiment from elm-format
                       Print.followedBy
                        (case commentsAfterFields of
                            [] ->
                                Print.empty

                            comment0 :: comment1Up ->
                                Print.linebreak
                                    |> Print.followedBy (Print.spaceOrLinebreakIndented lineSpread)
                                    |> Print.followedBy (comments (comment0 :: comment1Up))
                        )
                )
            )
        |> Print.followedBy (Print.spaceOrLinebreakIndented lineSpread)
        |> Print.followedBy printExactlyCurlyClosing


expressionLambda :
    List (GrenSyntax.Node String)
    ->
        GrenSyntax.Node
            { parameters : List (GrenSyntax.Node GrenSyntax.Pattern)
            , result : GrenSyntax.Node GrenSyntax.Expression
            }
    -> Print
expressionLambda syntaxComments syntaxLambdaNode =
    let
        parameterPrintsWithCommentsBefore :
            { endLocation : GrenSyntax.Location
            , reverse : List Print
            }
        parameterPrintsWithCommentsBefore =
            syntaxLambdaNode.value.parameters
                |> List.foldl
                    (\parameterPattern soFar ->
                        let
                            parameterRange : GrenSyntax.Range
                            parameterRange =
                                parameterPattern |> GrenSyntax.nodeRange

                            print : Print
                            print =
                                patternParenthesizedIfSpaceSeparated syntaxComments parameterPattern
                        in
                        { reverse =
                            (case
                                commentsInRange
                                    { start = soFar.endLocation, end = parameterRange.start }
                                    syntaxComments
                             of
                                [] ->
                                    print

                                comment0 :: comment1Up ->
                                    let
                                        commentsBefore : { print : Print, lineSpread : Print.LineSpread }
                                        commentsBefore =
                                            collapsibleComments (comment0 :: comment1Up)
                                    in
                                    commentsBefore.print
                                        |> Print.followedBy
                                            (Print.spaceOrLinebreakIndented
                                                (commentsBefore.lineSpread
                                                    |> Print.lineSpreadMergeWith
                                                        (\() -> print |> Print.lineSpread)
                                                )
                                            )
                                        |> Print.followedBy print
                            )
                                :: soFar.reverse
                        , endLocation = parameterRange.end
                        }
                    )
                    { reverse = []
                    , endLocation = syntaxLambdaNode.range.start
                    }

        commentsBeforeResult : List String
        commentsBeforeResult =
            commentsInRange
                { start = parameterPrintsWithCommentsBefore.endLocation
                , end = syntaxLambdaNode.value.result |> GrenSyntax.nodeRange |> .start
                }
                syntaxComments

        parametersLineSpread : Print.LineSpread
        parametersLineSpread =
            parameterPrintsWithCommentsBefore.reverse
                |> Print.lineSpreadListMapAndCombine Print.lineSpread

        resultPrint : Print
        resultPrint =
            expressionNotParenthesized syntaxComments
                syntaxLambdaNode.value.result
    in
    printExactlyBackSlash
        |> Print.followedBy
            (Print.withIndentIncreasedBy 1
                (Print.emptyOrLinebreakIndented parametersLineSpread
                    |> Print.followedBy
                        (parameterPrintsWithCommentsBefore.reverse
                            |> Print.listReverseAndIntersperseAndFlatten
                                (Print.spaceOrLinebreakIndented parametersLineSpread)
                        )
                )
                |> Print.followedBy (Print.spaceOrLinebreakIndented parametersLineSpread)
                |> Print.followedBy printExactlyMinusGreaterThan
                |> Print.followedBy
                    (Print.withIndentAtNextMultipleOf4
                        ((case commentsBeforeResult of
                            [] ->
                                Print.spaceOrLinebreakIndented
                                    (parametersLineSpread
                                        |> Print.lineSpreadMergeWith
                                            (\() ->
                                                lineSpreadInRange syntaxLambdaNode.range
                                            )
                                        |> Print.lineSpreadMergeWith
                                            (\() -> resultPrint |> Print.lineSpread)
                                    )

                            comment0 :: comment1Up ->
                                Print.linebreakIndented
                                    |> Print.followedBy (comments (comment0 :: comment1Up))
                                    |> Print.followedBy Print.linebreakIndented
                         )
                            |> Print.followedBy
                                resultPrint
                        )
                    )
            )


expressionIfThenElse :
    List (GrenSyntax.Node String)
    ->
        { fullRange : GrenSyntax.Range
        , condition : GrenSyntax.Node GrenSyntax.Expression
        , conditionLineSpreadMinimum : Print.LineSpread
        , onTrue : GrenSyntax.Node GrenSyntax.Expression
        , onFalse : GrenSyntax.Node GrenSyntax.Expression
        }
    -> Print
expressionIfThenElse syntaxComments syntaxIfThenElse =
    -- IGNORE TCO
    let
        commentsBeforeCondition : List String
        commentsBeforeCondition =
            commentsInRange
                { start = syntaxIfThenElse.fullRange.start
                , end = syntaxIfThenElse.condition |> GrenSyntax.nodeRange |> .start
                }
                syntaxComments

        commentsBeforeOnTrue : List String
        commentsBeforeOnTrue =
            commentsInRange
                { start = syntaxIfThenElse.condition |> GrenSyntax.nodeRange |> .end
                , end = syntaxIfThenElse.onTrue |> GrenSyntax.nodeRange |> .start
                }
                syntaxComments

        onFalseNotParenthesized : GrenSyntax.Node GrenSyntax.Expression
        onFalseNotParenthesized =
            syntaxIfThenElse.onFalse |> expressionToNotParenthesized

        commentsBeforeOnFalseNotParenthesizedInParens : List String
        commentsBeforeOnFalseNotParenthesizedInParens =
            commentsInRange
                { start = syntaxIfThenElse.onFalse |> GrenSyntax.nodeRange |> .start
                , end = onFalseNotParenthesized |> GrenSyntax.nodeRange |> .start
                }
                syntaxComments

        commentsBeforeOnFalse : List String
        commentsBeforeOnFalse =
            commentsInRange
                { start = syntaxIfThenElse.onTrue |> GrenSyntax.nodeRange |> .end
                , end = syntaxIfThenElse.onFalse |> GrenSyntax.nodeRange |> .start
                }
                syntaxComments

        conditionPrint : Print
        conditionPrint =
            expressionNotParenthesized syntaxComments syntaxIfThenElse.condition

        conditionLineSpread : Print.LineSpread
        conditionLineSpread =
            syntaxIfThenElse.conditionLineSpreadMinimum
                |> Print.lineSpreadMergeWith
                    (\() -> Print.lineSpread conditionPrint)
                |> Print.lineSpreadMergeWith
                    (\() ->
                        case commentsBeforeCondition of
                            _ :: _ ->
                                Print.MultipleLines

                            [] ->
                                Print.SingleLine
                    )

        onTruePrint : Print
        onTruePrint =
            expressionNotParenthesized syntaxComments syntaxIfThenElse.onTrue
    in
    printExactlyIf
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.spaceOrLinebreakIndented conditionLineSpread
                    |> Print.followedBy
                        (case commentsBeforeCondition of
                            [] ->
                                conditionPrint

                            comment0 :: comment1Up ->
                                comments (comment0 :: comment1Up)
                                    |> Print.followedBy Print.linebreakIndented
                                    |> Print.followedBy conditionPrint
                        )
                )
            )
        |> Print.followedBy (Print.spaceOrLinebreakIndented conditionLineSpread)
        |> Print.followedBy printExactlyThen
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (case commentsBeforeOnTrue of
                            [] ->
                                onTruePrint

                            comment0 :: comment1Up ->
                                comments (comment0 :: comment1Up)
                                    |> Print.followedBy Print.linebreakIndented
                                    |> Print.followedBy onTruePrint
                        )
                    |> Print.followedBy Print.linebreak
                )
            )
        |> Print.followedBy Print.linebreakIndented
        |> Print.followedBy printExactlyElse
        |> Print.followedBy
            (case ( commentsBeforeOnFalseNotParenthesizedInParens, onFalseNotParenthesized.value ) of
                ( [], GrenSyntax.ExpressionIfThenElse ifThenElse ) ->
                    case commentsBeforeOnFalse of
                        [] ->
                            printExactlySpace
                                |> Print.followedBy
                                    (expressionIfThenElse syntaxComments
                                        { fullRange = onFalseNotParenthesized.range
                                        , condition = ifThenElse.condition
                                        , conditionLineSpreadMinimum = Print.SingleLine
                                        , onTrue = ifThenElse.onTrue
                                        , onFalse = ifThenElse.onFalse
                                        }
                                    )

                        comment0 :: comment1Up ->
                            Print.linebreakIndented
                                |> Print.followedBy
                                    (comments (comment0 :: comment1Up))
                                |> Print.followedBy Print.linebreakIndented
                                |> Print.followedBy
                                    (expressionIfThenElse syntaxComments
                                        { fullRange = onFalseNotParenthesized.range
                                        , conditionLineSpreadMinimum =
                                            -- don't ask me why
                                            Print.MultipleLines
                                        , condition = ifThenElse.condition
                                        , onTrue = ifThenElse.onTrue
                                        , onFalse = ifThenElse.onFalse
                                        }
                                    )

                _ ->
                    let
                        onFalsePrint : Print
                        onFalsePrint =
                            expressionNotParenthesized syntaxComments syntaxIfThenElse.onFalse
                    in
                    Print.withIndentAtNextMultipleOf4
                        (Print.linebreakIndented
                            |> Print.followedBy
                                (case commentsBeforeOnFalse of
                                    [] ->
                                        onFalsePrint

                                    comment0 :: comment1Up ->
                                        comments (comment0 :: comment1Up)
                                            |> Print.followedBy Print.linebreakIndented
                                            |> Print.followedBy onFalsePrint
                                )
                        )
            )


expressionCaseOf :
    List (GrenSyntax.Node String)
    ->
        { fullRange : GrenSyntax.Range
        , expression : GrenSyntax.Node GrenSyntax.Expression
        , cases :
            List
                { pattern : GrenSyntax.Node GrenSyntax.Pattern
                , result : GrenSyntax.Node GrenSyntax.Expression
                }
        }
    -> Print
expressionCaseOf syntaxComments syntaxCaseOf =
    let
        commentsBeforeCasedExpression : List String
        commentsBeforeCasedExpression =
            commentsInRange
                { start = syntaxCaseOf.fullRange.start
                , end = syntaxCaseOf.expression |> GrenSyntax.nodeRange |> .start
                }
                syntaxComments

        casedExpressionLineSpread : Print.LineSpread
        casedExpressionLineSpread =
            case commentsBeforeCasedExpression of
                _ :: _ ->
                    Print.MultipleLines

                [] ->
                    lineSpreadInNode syntaxCaseOf.expression

        casedExpressionPrint : Print
        casedExpressionPrint =
            expressionNotParenthesized syntaxComments
                syntaxCaseOf.expression
    in
    printExactlyWhen
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.spaceOrLinebreakIndented casedExpressionLineSpread
                    |> Print.followedBy
                        (case commentsBeforeCasedExpression of
                            [] ->
                                casedExpressionPrint

                            comment0 :: comment1Up ->
                                comments (comment0 :: comment1Up)
                                    |> Print.followedBy Print.linebreakIndented
                                    |> Print.followedBy casedExpressionPrint
                        )
                )
            )
        |> Print.followedBy
            (Print.spaceOrLinebreakIndented casedExpressionLineSpread)
        |> Print.followedBy printExactlyIs
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (syntaxCaseOf.cases
                            |> List.foldl
                                (\syntaxCase soFar ->
                                    let
                                        commentsBeforeCasePattern : List String
                                        commentsBeforeCasePattern =
                                            commentsInRange
                                                { start = soFar.endLocation
                                                , end = syntaxCase.pattern |> GrenSyntax.nodeRange |> .start
                                                }
                                                syntaxComments

                                        casePrint : Print
                                        casePrint =
                                            case_ syntaxComments syntaxCase

                                        commentsAndCasePrint : Print
                                        commentsAndCasePrint =
                                            case commentsBeforeCasePattern of
                                                [] ->
                                                    casePrint

                                                comment0 :: comment1Up ->
                                                    comments (comment0 :: comment1Up)
                                                        |> Print.followedBy Print.linebreakIndented
                                                        |> Print.followedBy casePrint
                                    in
                                    { endLocation = syntaxCase.result |> GrenSyntax.nodeRange |> .end
                                    , reverse = commentsAndCasePrint :: soFar.reverse
                                    }
                                )
                                { endLocation = syntaxCaseOf.expression |> GrenSyntax.nodeRange |> .end
                                , reverse = []
                                }
                            |> .reverse
                            |> Print.listReverseAndIntersperseAndFlatten
                                printLinebreakLinebreakIndented
                        )
                )
            )


expressionLetIn :
    List (GrenSyntax.Node String)
    ->
        { fullRange : GrenSyntax.Range
        , letDeclaration0 : GrenSyntax.Node GrenSyntax.LetDeclaration
        , letDeclaration1Up : List (GrenSyntax.Node GrenSyntax.LetDeclaration)
        , result : GrenSyntax.Node GrenSyntax.Expression
        }
    -> Print
expressionLetIn syntaxComments syntaxLetIn =
    let
        letDeclarationPrints : { endLocation : GrenSyntax.Location, reverse : List Print }
        letDeclarationPrints =
            (syntaxLetIn.letDeclaration0 :: syntaxLetIn.letDeclaration1Up)
                |> List.foldl
                    (\letDeclarationNode soFar ->
                        let
                            commentsBefore : List String
                            commentsBefore =
                                commentsInRange
                                    { start = soFar.endLocation
                                    , end = letDeclarationNode.range.start
                                    }
                                    syntaxComments

                            letDeclarationPrint : Print
                            letDeclarationPrint =
                                expressionLetDeclaration syntaxComments
                                    letDeclarationNode.value

                            letDeclarationWithCommentsBeforePrint : Print
                            letDeclarationWithCommentsBeforePrint =
                                case commentsBefore of
                                    [] ->
                                        letDeclarationPrint

                                    comment0 :: comment1Up ->
                                        comments (comment0 :: comment1Up)
                                            |> Print.followedBy Print.linebreakIndented
                                            |> Print.followedBy letDeclarationPrint
                        in
                        { endLocation = letDeclarationNode.range.end
                        , reverse =
                            letDeclarationWithCommentsBeforePrint :: soFar.reverse
                        }
                    )
                    { endLocation = syntaxLetIn.fullRange.start
                    , reverse = []
                    }

        commentsBeforeResult : List String
        commentsBeforeResult =
            commentsInRange
                { start = letDeclarationPrints.endLocation
                , end =
                    syntaxLetIn.result
                        |> GrenSyntax.nodeRange
                        |> .start
                }
                syntaxComments

        letInResultPrint : Print
        letInResultPrint =
            expressionNotParenthesized syntaxComments syntaxLetIn.result
    in
    printExactlyLet
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (letDeclarationPrints.reverse
                            |> Print.listReverseAndIntersperseAndFlatten
                                printLinebreakLinebreakIndented
                        )
                )
            )
        |> Print.followedBy printLinebreakIndentedInLinebreakIndented
        |> Print.followedBy
            (case commentsBeforeResult of
                [] ->
                    letInResultPrint

                comment0 :: comment1Up ->
                    comments (comment0 :: comment1Up)
                        |> Print.followedBy Print.linebreakIndented
                        |> Print.followedBy letInResultPrint
            )


expressionLetDeclaration :
    List (GrenSyntax.Node String)
    -> GrenSyntax.LetDeclaration
    -> Print
expressionLetDeclaration syntaxComments letDeclaration =
    case letDeclaration of
        GrenSyntax.LetFunction letDeclarationExpression ->
            let
                implementationPrint : Print
                implementationPrint =
                    declarationExpressionImplementation syntaxComments
                        (letDeclarationExpression.declaration |> GrenSyntax.nodeValue)
            in
            case letDeclarationExpression.signature of
                Nothing ->
                    implementationPrint

                Just signatureNode ->
                    let
                        commentsBetweenSignatureAndImplementationName : List String
                        commentsBetweenSignatureAndImplementationName =
                            commentsInRange
                                { start = signatureNode.range.end
                                , end =
                                    letDeclarationExpression.declaration
                                        |> GrenSyntax.nodeRange
                                        |> .start
                                }
                                syntaxComments
                    in
                    declarationSignature syntaxComments signatureNode.value
                        |> Print.followedBy
                            (case commentsBetweenSignatureAndImplementationName of
                                [] ->
                                    Print.linebreakIndented

                                comment0 :: comment1Up ->
                                    Print.linebreakIndented
                                        |> Print.followedBy
                                            (comments (comment0 :: comment1Up))
                                        |> Print.followedBy Print.linebreakIndented
                            )
                        |> Print.followedBy implementationPrint

        GrenSyntax.LetDestructuring destructuringPattern destructuredExpression ->
            let
                commentsBeforeDestructuredExpression : List String
                commentsBeforeDestructuredExpression =
                    commentsInRange
                        { start = destructuringPattern |> GrenSyntax.nodeRange |> .end
                        , end = destructuredExpression |> GrenSyntax.nodeRange |> .start
                        }
                        syntaxComments

                destructuringPatternPrint : Print
                destructuringPatternPrint =
                    patternParenthesizedIfSpaceSeparated syntaxComments destructuringPattern

                destructuredExpressionPrint : Print
                destructuredExpressionPrint =
                    expressionNotParenthesized syntaxComments
                        destructuredExpression
            in
            destructuringPatternPrint
                |> Print.followedBy
                    (Print.withIndentAtNextMultipleOf4
                        (Print.spaceOrLinebreakIndented
                            (destructuringPatternPrint |> Print.lineSpread)
                            |> Print.followedBy printEqualsLinebreakIndented
                            |> Print.followedBy
                                (case commentsBeforeDestructuredExpression of
                                    [] ->
                                        destructuredExpressionPrint

                                    comment0 :: comment1Up ->
                                        comments (comment0 :: comment1Up)
                                            |> Print.followedBy
                                                Print.linebreakIndented
                                            |> Print.followedBy destructuredExpressionPrint
                                )
                        )
                    )


expressionToNotParenthesized :
    GrenSyntax.Node GrenSyntax.Expression
    -> GrenSyntax.Node GrenSyntax.Expression
expressionToNotParenthesized syntaxExpressionNode =
    -- IGNORE TCO
    case syntaxExpressionNode.value of
        GrenSyntax.ExpressionParenthesized inParens ->
            inParens |> expressionToNotParenthesized

        syntaxExpressionNotParenthesized ->
            { range = syntaxExpressionNode.range, value = syntaxExpressionNotParenthesized }


{-| Print a single [`GrenSyntax.Case`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Expression#Case)
-}
case_ :
    List (GrenSyntax.Node String)
    ->
        { pattern : GrenSyntax.Node GrenSyntax.Pattern
        , result : GrenSyntax.Node GrenSyntax.Expression
        }
    -> Print
case_ syntaxComments syntaxCase =
    let
        patternPrint : Print
        patternPrint =
            patternNotParenthesized syntaxComments syntaxCase.pattern

        commentsBeforeExpression : List String
        commentsBeforeExpression =
            commentsInRange
                { start = syntaxCase.pattern |> GrenSyntax.nodeRange |> .end
                , end = syntaxCase.result |> GrenSyntax.nodeRange |> .start
                }
                syntaxComments

        caseResultPrint : Print
        caseResultPrint =
            expressionNotParenthesized syntaxComments syntaxCase.result
    in
    patternPrint
        |> Print.followedBy
            (Print.spaceOrLinebreakIndented (patternPrint |> Print.lineSpread))
        |> Print.followedBy printExactlyMinusGreaterThan
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (case commentsBeforeExpression of
                            [] ->
                                caseResultPrint

                            comment0 :: comment1Up ->
                                comments (comment0 :: comment1Up)
                                    |> Print.followedBy Print.linebreakIndented
                                    |> Print.followedBy caseResultPrint
                        )
                )
            )


printLinebreakIndentedCommaSpace : Print.Print
printLinebreakIndentedCommaSpace =
    Print.linebreakIndented
        |> Print.followedBy printExactlyCommaSpace


printLinebreakIndentedAs : Print.Print
printLinebreakIndentedAs =
    Print.linebreakIndented
        |> Print.followedBy printExactlyAs


printLinebreakIndentedExposing : Print.Print
printLinebreakIndentedExposing =
    Print.linebreakIndented
        |> Print.followedBy printExactlyExposing


printLinebreakIndentedVerticalBarSpace : Print.Print
printLinebreakIndentedVerticalBarSpace =
    Print.linebreakIndented
        |> Print.followedBy printExactlyVerticalBarSpace


printLinebreakLinebreak : Print.Print
printLinebreakLinebreak =
    Print.linebreak
        |> Print.followedBy Print.linebreak


printLinebreakLinebreakIndented : Print.Print
printLinebreakLinebreakIndented =
    Print.linebreak
        |> Print.followedBy Print.linebreakIndented


printLinebreakLinebreakLinebreak : Print.Print
printLinebreakLinebreakLinebreak =
    Print.linebreak
        |> Print.followedBy Print.linebreak
        |> Print.followedBy Print.linebreak


printLinebreakIndentedInLinebreakIndented : Print.Print
printLinebreakIndentedInLinebreakIndented =
    Print.linebreakIndented
        |> Print.followedBy printExactlyIn
        |> Print.followedBy Print.linebreakIndented


printEqualsLinebreakIndented : Print.Print
printEqualsLinebreakIndented =
    printExactlyEquals
        |> Print.followedBy Print.linebreakIndented


printExactlyCurlyBraceOpeningCurlyBraceClosing : Print.Print
printExactlyCurlyBraceOpeningCurlyBraceClosing =
    Print.exactly "{}"


printExactlySpaceSpace : Print.Print
printExactlySpaceSpace =
    Print.exactly "  "


printExactlySpace : Print.Print
printExactlySpace =
    Print.exactly " "


printExactlyEqualsSpace : Print.Print
printExactlyEqualsSpace =
    Print.exactly "= "


printExactlyCommaSpace : Print.Print
printExactlyCommaSpace =
    Print.exactly ", "


printExactlyColonColonSpace : Print.Print
printExactlyColonColonSpace =
    Print.exactly ":: "


printExactlyPortSpace : Print.Print
printExactlyPortSpace =
    Print.exactly "port "


printExactlyTypeSpaceAlias : Print.Print
printExactlyTypeSpaceAlias =
    Print.exactly "type alias"


printExactlySquareOpeningSpace : Print.Print
printExactlySquareOpeningSpace =
    Print.exactly "[ "


printExactlyParensOpeningSpace : Print.Print
printExactlyParensOpeningSpace =
    Print.exactly "( "


printExactlyCurlyOpeningSpace : Print.Print
printExactlyCurlyOpeningSpace =
    Print.exactly "{ "


printExactlyVerticalBarSpace : Print.Print
printExactlyVerticalBarSpace =
    Print.exactly "| "


printExactlyCurlyOpeningDotDotCurlyClosing : Print.Print
printExactlyCurlyOpeningDotDotCurlyClosing =
    Print.exactly "{--}"


printExactlyParensOpening : Print
printExactlyParensOpening =
    Print.exactly "("


printExactlyParensClosing : Print
printExactlyParensClosing =
    Print.exactly ")"


printExactlySquareOpening : Print
printExactlySquareOpening =
    Print.exactly "["


printExactlySquareClosing : Print
printExactlySquareClosing =
    Print.exactly "]"


printExactlyCurlyOpening : Print
printExactlyCurlyOpening =
    Print.exactly "{"


printExactlyCurlyClosing : Print
printExactlyCurlyClosing =
    Print.exactly "}"


printExactlyDoubleQuoteDoubleQuoteDoubleQuote : Print
printExactlyDoubleQuoteDoubleQuoteDoubleQuote =
    Print.exactly "\"\"\""


printExactlyMinusGreaterThan : Print
printExactlyMinusGreaterThan =
    Print.exactly "->"


printExactlyEquals : Print
printExactlyEquals =
    Print.exactly "="


printExactlyParensOpeningDotDotParensClosing : Print
printExactlyParensOpeningDotDotParensClosing =
    Print.exactly "(..)"


printExactlyBackSlash : Print
printExactlyBackSlash =
    Print.exactly "\\"


printExactlyLessThanVerticalBar : Print
printExactlyLessThanVerticalBar =
    Print.exactly "<|"


printExactlyMinus : Print
printExactlyMinus =
    Print.exactly "-"


printExactlyZero : Print
printExactlyZero =
    Print.exactly "0"


printExactlyZeroXZeroZero : Print
printExactlyZeroXZeroZero =
    Print.exactly "0x00"


printExactlyCurlyOpeningMinus : Print
printExactlyCurlyOpeningMinus =
    Print.exactly "{-"


printExactlyMinusCurlyClosing : Print
printExactlyMinusCurlyClosing =
    Print.exactly "-}"


printExactlyUnderscore : Print
printExactlyUnderscore =
    Print.exactly "_"


printExactlyType : Print
printExactlyType =
    Print.exactly "type"


printExactlyAs : Print
printExactlyAs =
    Print.exactly "as"


printExactlyExposing : Print
printExactlyExposing =
    Print.exactly "exposing"


printExactlyLet : Print
printExactlyLet =
    Print.exactly "let"


printExactlyIn : Print
printExactlyIn =
    Print.exactly "in"


printExactlyWhen : Print
printExactlyWhen =
    Print.exactly "when"


printExactlyIs : Print
printExactlyIs =
    Print.exactly "is"


printExactlyIf : Print
printExactlyIf =
    Print.exactly "if"


printExactlyThen : Print
printExactlyThen =
    Print.exactly "then"


printExactlyElse : Print
printExactlyElse =
    Print.exactly "else"


printExactImport : Print
printExactImport =
    Print.exactly "import"
