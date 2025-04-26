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

import Char.Extra
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
import GrenSyntax
import Print exposing (Print)
import Unicode


{-| Print an [`GrenSyntax.File`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-File#File)
-}
module_ : GrenSyntax.File -> Print
module_ syntaxModule =
    let
        maybeModuleDocumentation : Maybe (Gren.Syntax.Node.Node String)
        maybeModuleDocumentation =
            moduleDocumentation syntaxModule

        commentsAndPortDocumentationComments :
            { portDocumentationComments : List (Gren.Syntax.Node.Node String)
            , remainingComments : List (Gren.Syntax.Node.Node String)
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

                Just (Gren.Syntax.Node.Node _ syntaxModuleDocumentation) ->
                    Just
                        (syntaxModuleDocumentation
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

        lastSyntaxLocationBeforeDeclarations : Gren.Syntax.Range.Location
        lastSyntaxLocationBeforeDeclarations =
            case syntaxModule.imports of
                (Gren.Syntax.Node.Node firstImportRange _) :: _ ->
                    firstImportRange.end

                [] ->
                    syntaxModule.moduleDefinition
                        |> Gren.Syntax.Node.range
                        |> .end

        commentsBeforeDeclarations : List String
        commentsBeforeDeclarations =
            case syntaxModule.declarations of
                [] ->
                    -- invalid syntax
                    []

                (Gren.Syntax.Node.Node declaration0Range _) :: _ ->
                    commentsInRange
                        { start = lastSyntaxLocationBeforeDeclarations
                        , end = declaration0Range.start
                        }
                        commentsAndPortDocumentationComments.remainingComments
    in
    syntaxModule.moduleDefinition
        |> Gren.Syntax.Node.value
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

                (Gren.Syntax.Node.Node import0Range import0) :: import1Up ->
                    (case
                        commentsInRange
                            { start =
                                syntaxModule.moduleDefinition
                                    |> Gren.Syntax.Node.range
                                    |> .end
                            , end = import0Range.start
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
                            ((Gren.Syntax.Node.Node import0Range import0 :: import1Up)
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
                                |> Gren.Syntax.Node.range
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
    List (Gren.Syntax.Node.Node String)
    ->
        { portDocumentationComments : List (Gren.Syntax.Node.Node String)
        , remainingComments : List (Gren.Syntax.Node.Node String)
        }
splitOffPortDocumentationComments commentsAndPortDocumentationComments =
    commentsAndPortDocumentationComments
        |> List.foldr
            (\commentOrPortDocumentationComments soFar ->
                if commentOrPortDocumentationComments |> Gren.Syntax.Node.value |> String.startsWith "{-|" then
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


moduleDocumentation : GrenSyntax.File -> Maybe (Gren.Syntax.Node.Node String)
moduleDocumentation ast =
    let
        cutOffLine : Int
        cutOffLine =
            case ast.imports of
                (Gren.Syntax.Node.Node firstImportRange _) :: _ ->
                    firstImportRange.start.row

                [] ->
                    case ast.declarations of
                        (Gren.Syntax.Node.Node firstDeclarationRange _) :: _ ->
                            firstDeclarationRange.start.row

                        [] ->
                            -- Should not happen, as every module should have at least one declaration
                            0
    in
    moduleDocumentationBeforeCutOffLine cutOffLine ast.comments


moduleDocumentationBeforeCutOffLine : Int -> List (Gren.Syntax.Node.Node String) -> Maybe (Gren.Syntax.Node.Node String)
moduleDocumentationBeforeCutOffLine cutOffLine allComments =
    case allComments of
        [] ->
            Nothing

        headComment :: restOfComments ->
            let
                (Gren.Syntax.Node.Node range content) =
                    headComment
            in
            if range.start.row > cutOffLine then
                Nothing

            else if String.startsWith "{-|" content then
                Just headComment

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

                        else if line == "@docs" then
                            { rawSinceAtDocs = ""
                            , finishedBlocks =
                                { rawBefore = soFar.rawSinceAtDocs
                                , atDocsLine = []
                                }
                                    :: soFar.finishedBlocks
                            }

                        else
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


commentsAfter : Gren.Syntax.Range.Location -> List (Gren.Syntax.Node.Node String) -> List String
commentsAfter end sortedComments =
    case sortedComments of
        [] ->
            []

        (Gren.Syntax.Node.Node headCommentRange headComment) :: tailComments ->
            case locationCompareFast headCommentRange.start end of
                LT ->
                    commentsAfter end tailComments

                GT ->
                    headComment :: (tailComments |> List.map Gren.Syntax.Node.value)

                EQ ->
                    headComment :: (tailComments |> List.map Gren.Syntax.Node.value)


moduleLevelCommentsBeforeDeclaration : { comment0 : String, comment1Up : List String } -> Print
moduleLevelCommentsBeforeDeclaration syntaxComments =
    Print.linebreak
        |> Print.followedBy
            (moduleLevelComments (syntaxComments.comment0 :: syntaxComments.comment1Up))
        |> Print.followedBy
            (case listFilledLast syntaxComments.comment0 syntaxComments.comment1Up of
                "{--}" ->
                    -- don't ask me why gren-format formats it that way
                    Print.empty

                _ ->
                    printLinebreakLinebreak
            )


commentNodesInRange : Gren.Syntax.Range.Range -> List (Gren.Syntax.Node.Node String) -> List (Gren.Syntax.Node.Node String)
commentNodesInRange range sortedComments =
    case sortedComments of
        [] ->
            []

        headCommentNode :: tailComments ->
            let
                (Gren.Syntax.Node.Node headCommentRange _) =
                    headCommentNode
            in
            case locationCompareFast headCommentRange.start range.start of
                LT ->
                    commentNodesInRange range tailComments

                EQ ->
                    headCommentNode :: commentNodesInRange range tailComments

                GT ->
                    case locationCompareFast headCommentRange.end range.end of
                        GT ->
                            []

                        LT ->
                            headCommentNode :: commentNodesInRange range tailComments

                        EQ ->
                            headCommentNode :: commentNodesInRange range tailComments


commentsInRange : Gren.Syntax.Range.Range -> List (Gren.Syntax.Node.Node String) -> List String
commentsInRange range sortedComments =
    case sortedComments of
        [] ->
            []

        (Gren.Syntax.Node.Node headCommentRange headComment) :: tailComments ->
            case locationCompareFast headCommentRange.start range.start of
                LT ->
                    commentsInRange range tailComments

                EQ ->
                    headComment :: commentsInRange range tailComments

                GT ->
                    case locationCompareFast headCommentRange.end range.end of
                        GT ->
                            []

                        LT ->
                            headComment :: commentsInRange range tailComments

                        EQ ->
                            headComment :: commentsInRange range tailComments


lineSpreadInRange : Gren.Syntax.Range.Range -> Print.LineSpread
lineSpreadInRange range =
    if range.end.row - range.start.row == 0 then
        Print.SingleLine

    else
        Print.MultipleLines


exposingMulti :
    List (Gren.Syntax.Node.Node String)
    ->
        { fullRange : Gren.Syntax.Range.Range
        , expose0 : Gren.Syntax.Node.Node Gren.Syntax.Exposing.TopLevelExpose
        , expose1Up : List (Gren.Syntax.Node.Node Gren.Syntax.Exposing.TopLevelExpose)
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
                    (\(Gren.Syntax.Node.Node _ syntaxExpose) ->
                        Print.exactly (expose syntaxExpose)
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


exposeCompare : Gren.Syntax.Exposing.TopLevelExpose -> Gren.Syntax.Exposing.TopLevelExpose -> Basics.Order
exposeCompare a b =
    case a of
        Gren.Syntax.Exposing.InfixExpose aOperatorSymbol ->
            case b of
                Gren.Syntax.Exposing.InfixExpose bOperatorSymbol ->
                    compare aOperatorSymbol bOperatorSymbol

                Gren.Syntax.Exposing.FunctionExpose _ ->
                    LT

                Gren.Syntax.Exposing.TypeOrAliasExpose _ ->
                    LT

                Gren.Syntax.Exposing.TypeExpose _ ->
                    LT

        Gren.Syntax.Exposing.FunctionExpose aName ->
            case b of
                Gren.Syntax.Exposing.InfixExpose _ ->
                    GT

                Gren.Syntax.Exposing.FunctionExpose bName ->
                    compare aName bName

                Gren.Syntax.Exposing.TypeOrAliasExpose _ ->
                    GT

                Gren.Syntax.Exposing.TypeExpose _ ->
                    GT

        Gren.Syntax.Exposing.TypeOrAliasExpose aName ->
            case b of
                Gren.Syntax.Exposing.InfixExpose _ ->
                    GT

                Gren.Syntax.Exposing.FunctionExpose _ ->
                    LT

                Gren.Syntax.Exposing.TypeOrAliasExpose bName ->
                    compare aName bName

                Gren.Syntax.Exposing.TypeExpose bTypeExpose ->
                    compare aName bTypeExpose.name

        Gren.Syntax.Exposing.TypeExpose aTypeExpose ->
            case b of
                Gren.Syntax.Exposing.InfixExpose _ ->
                    GT

                Gren.Syntax.Exposing.FunctionExpose _ ->
                    LT

                Gren.Syntax.Exposing.TypeOrAliasExpose bName ->
                    compare aTypeExpose.name bName

                Gren.Syntax.Exposing.TypeExpose bTypeExpose ->
                    compare aTypeExpose.name bTypeExpose.name


exposeListToNormal :
    List (Gren.Syntax.Node.Node Gren.Syntax.Exposing.TopLevelExpose)
    -> List (Gren.Syntax.Node.Node Gren.Syntax.Exposing.TopLevelExpose)
exposeListToNormal syntaxExposeList =
    syntaxExposeList
        |> List.sortWith
            (\(Gren.Syntax.Node.Node _ a) (Gren.Syntax.Node.Node _ b) ->
                exposeCompare a b
            )
        |> exposesCombine


exposesCombine :
    List (Gren.Syntax.Node.Node Gren.Syntax.Exposing.TopLevelExpose)
    -> List (Gren.Syntax.Node.Node Gren.Syntax.Exposing.TopLevelExpose)
exposesCombine syntaxExposes =
    case syntaxExposes of
        [] ->
            []

        [ _ ] as onlyExposeList ->
            onlyExposeList

        expose0Node :: (expose1Node :: expose2Up) ->
            let
                (Gren.Syntax.Node.Node _ expose1) =
                    expose1Node

                (Gren.Syntax.Node.Node expose0Range expose0) =
                    expose0Node
            in
            case exposeCompare expose0 expose1 of
                EQ ->
                    exposesCombine
                        (Gren.Syntax.Node.Node expose0Range
                            (exposeMerge expose0 expose1)
                            :: expose2Up
                        )

                LT ->
                    expose0Node :: exposesCombine (expose1Node :: expose2Up)

                GT ->
                    expose0Node :: exposesCombine (expose1Node :: expose2Up)


exposeMerge : Gren.Syntax.Exposing.TopLevelExpose -> Gren.Syntax.Exposing.TopLevelExpose -> Gren.Syntax.Exposing.TopLevelExpose
exposeMerge a b =
    case a of
        Gren.Syntax.Exposing.TypeExpose aTypeExpose ->
            case b of
                Gren.Syntax.Exposing.TypeExpose bTypeExpose ->
                    Gren.Syntax.Exposing.TypeExpose
                        { name = aTypeExpose.name
                        , open =
                            case aTypeExpose.open of
                                Just openRange ->
                                    Just openRange

                                Nothing ->
                                    bTypeExpose.open
                        }

                Gren.Syntax.Exposing.InfixExpose _ ->
                    Gren.Syntax.Exposing.TypeExpose aTypeExpose

                Gren.Syntax.Exposing.FunctionExpose _ ->
                    Gren.Syntax.Exposing.TypeExpose aTypeExpose

                Gren.Syntax.Exposing.TypeOrAliasExpose _ ->
                    Gren.Syntax.Exposing.TypeExpose aTypeExpose

        Gren.Syntax.Exposing.InfixExpose _ ->
            b

        Gren.Syntax.Exposing.FunctionExpose _ ->
            b

        Gren.Syntax.Exposing.TypeOrAliasExpose _ ->
            b


{-| Print the stuff after `exposing` in a module header.
For import exposing: [`importExposing`](#importExposing)
-}
moduleExposing :
    { atDocsLines : List (List String), comments : List (Gren.Syntax.Node.Node String) }
    -> Gren.Syntax.Node.Node Gren.Syntax.Exposing.Exposing
    -> Print
moduleExposing context (Gren.Syntax.Node.Node exposingRange syntaxExposing) =
    case syntaxExposing of
        Gren.Syntax.Exposing.All _ ->
            printExactlyParensOpeningDotDotParensClosing

        Gren.Syntax.Exposing.Explicit exposingSet ->
            case exposingSet |> exposeListToNormal of
                [] ->
                    printExactlyParensOpeningParensClosed

                [ Gren.Syntax.Node.Node _ onlySyntaxExpose ] ->
                    let
                        containedComments : List String
                        containedComments =
                            commentsInRange exposingRange context.comments

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
                            ++ expose onlySyntaxExpose
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
                                    { remainingExposes : List Gren.Syntax.Exposing.TopLevelExpose
                                    , atDocsExposeLines : List (List Gren.Syntax.Exposing.TopLevelExpose)
                                    }
                                atDocsExposeLines =
                                    (atDocsLine0 :: atDocsLine1Up)
                                        |> List.foldr
                                            (\atDocsLine soFar ->
                                                let
                                                    atDocsExposeLine :
                                                        { remaining : List Gren.Syntax.Exposing.TopLevelExpose
                                                        , exposes : List Gren.Syntax.Exposing.TopLevelExpose
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
                                                    |> List.map Gren.Syntax.Node.value
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
                                        { fullRange = exposingRange
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
                                { fullRange = exposingRange
                                , expose0 = expose0
                                , expose1Up = expose1 :: expose2Up
                                }


atDocsLineToExposesAndRemaining :
    List String
    -> List Gren.Syntax.Exposing.TopLevelExpose
    ->
        { remaining : List Gren.Syntax.Exposing.TopLevelExpose
        , exposes : List Gren.Syntax.Exposing.TopLevelExpose
        }
atDocsLineToExposesAndRemaining atDocsLine remainingExposes =
    atDocsLine
        |> List.foldr
            (\exposeAsAtDocsString soFar ->
                let
                    toExposeReferencedByAtDocsString : Gren.Syntax.Exposing.TopLevelExpose -> Maybe Gren.Syntax.Exposing.TopLevelExpose
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


exposeToAtDocsString : Gren.Syntax.Exposing.TopLevelExpose -> String
exposeToAtDocsString syntaxExpose =
    case syntaxExpose of
        Gren.Syntax.Exposing.InfixExpose operatorSymbol ->
            "(" ++ operatorSymbol ++ ")"

        Gren.Syntax.Exposing.FunctionExpose name ->
            name

        Gren.Syntax.Exposing.TypeOrAliasExpose name ->
            name

        Gren.Syntax.Exposing.TypeExpose choiceTypeExpose ->
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
    { atDocsLines : List (List String), comments : List (Gren.Syntax.Node.Node String) }
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
                    ++ moduleName (defaultModuleData.moduleName |> Gren.Syntax.Node.value)
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
                        (defaultModuleData.moduleName |> Gren.Syntax.Node.value)
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
                        (effectModuleData.moduleName |> Gren.Syntax.Node.value)
                    ++ " where { "
                    ++ ([ case effectModuleData.command of
                            Nothing ->
                                Nothing

                            Just (Gren.Syntax.Node.Node _ name) ->
                                Just ("command = " ++ name)
                        , case effectModuleData.subscription of
                            Nothing ->
                                Nothing

                            Just (Gren.Syntax.Node.Node _ name) ->
                                Just ("subscription = " ++ name)
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
    List (Gren.Syntax.Node.Node String)
    -> List (Gren.Syntax.Node.Node GrenSyntax.Import)
    -> Print
imports syntaxComments syntaxImports =
    case syntaxImports of
        [] ->
            Print.empty

        (Gren.Syntax.Node.Node import0Range import0) :: imports1Up ->
            let
                commentsBetweenImports : List String
                commentsBetweenImports =
                    (Gren.Syntax.Node.Node import0Range import0 :: imports1Up)
                        |> List.foldl
                            (\(Gren.Syntax.Node.Node importRange _) soFar ->
                                { previousImportRange = importRange
                                , commentsBetweenImports =
                                    soFar.commentsBetweenImports
                                        ++ commentsInRange { start = soFar.previousImportRange.end, end = importRange.start }
                                            syntaxComments
                                }
                            )
                            { previousImportRange = import0Range
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
                    ((Gren.Syntax.Node.Node import0Range import0 :: imports1Up)
                        |> List.sortWith
                            (\(Gren.Syntax.Node.Node _ a) (Gren.Syntax.Node.Node _ b) ->
                                compare (a.moduleName |> Gren.Syntax.Node.value) (b.moduleName |> Gren.Syntax.Node.value)
                            )
                        |> importsCombine
                        |> Print.listMapAndIntersperseAndFlatten
                            (\syntaxImport -> import_ syntaxComments syntaxImport)
                            Print.linebreak
                    )


importsCombine :
    List (Gren.Syntax.Node.Node GrenSyntax.Import)
    -> List (Gren.Syntax.Node.Node GrenSyntax.Import)
importsCombine syntaxImports =
    case syntaxImports of
        [] ->
            []

        [ onlyImport ] ->
            [ onlyImport |> Gren.Syntax.Node.map importToNormal ]

        (Gren.Syntax.Node.Node import0Range import0) :: (Gren.Syntax.Node.Node import1Range import1) :: import2Up ->
            if (import0.moduleName |> Gren.Syntax.Node.value) == (import1.moduleName |> Gren.Syntax.Node.value) then
                importsCombine
                    (Gren.Syntax.Node.Node import1Range
                        (importsMerge import0 import1)
                        :: import2Up
                    )

            else
                Gren.Syntax.Node.Node import0Range (import0 |> importToNormal)
                    :: importsCombine (Gren.Syntax.Node.Node import1Range import1 :: import2Up)


importToNormal : GrenSyntax.Import -> GrenSyntax.Import
importToNormal syntaxImport =
    { moduleName = syntaxImport.moduleName
    , moduleAlias = syntaxImport.moduleAlias
    , exposingList =
        case syntaxImport.exposingList of
            Nothing ->
                Nothing

            Just (Gren.Syntax.Node.Node exposingRange syntaxExposing) ->
                Just
                    (Gren.Syntax.Node.Node exposingRange
                        (syntaxExposing |> exposingToNormal)
                    )
    }


exposingToNormal : Gren.Syntax.Exposing.Exposing -> Gren.Syntax.Exposing.Exposing
exposingToNormal syntaxExposing =
    case syntaxExposing of
        Gren.Syntax.Exposing.All allRange ->
            Gren.Syntax.Exposing.All allRange

        Gren.Syntax.Exposing.Explicit exposeSet ->
            Gren.Syntax.Exposing.Explicit (exposeSet |> exposeListToNormal)


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
    Maybe (Gren.Syntax.Node.Node Gren.Syntax.Exposing.Exposing)
    -> Maybe (Gren.Syntax.Node.Node Gren.Syntax.Exposing.Exposing)
    -> Maybe (Gren.Syntax.Node.Node Gren.Syntax.Exposing.Exposing)
exposingCombine a b =
    case a of
        Just (Gren.Syntax.Node.Node exposingAllRange (Gren.Syntax.Exposing.All allRange)) ->
            Just (Gren.Syntax.Node.Node exposingAllRange (Gren.Syntax.Exposing.All allRange))

        Just (Gren.Syntax.Node.Node earlierExposingExplicitRange (Gren.Syntax.Exposing.Explicit earlierExposeSet)) ->
            Just
                (case b of
                    Just (Gren.Syntax.Node.Node exposingAllRange (Gren.Syntax.Exposing.All allRange)) ->
                        Gren.Syntax.Node.Node exposingAllRange (Gren.Syntax.Exposing.All allRange)

                    Just (Gren.Syntax.Node.Node laterExposingExplicitRange (Gren.Syntax.Exposing.Explicit laterExposeSet)) ->
                        Gren.Syntax.Node.Node
                            (case lineSpreadInRange earlierExposingExplicitRange of
                                Print.MultipleLines ->
                                    earlierExposingExplicitRange

                                Print.SingleLine ->
                                    laterExposingExplicitRange
                            )
                            (Gren.Syntax.Exposing.Explicit
                                (earlierExposeSet ++ laterExposeSet |> exposeListToNormal)
                            )

                    Nothing ->
                        Gren.Syntax.Node.Node earlierExposingExplicitRange (Gren.Syntax.Exposing.Explicit earlierExposeSet)
                )

        Nothing ->
            b


{-| Print a single [`GrenSyntax.Import`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Import#Import)
-}
import_ :
    List (Gren.Syntax.Node.Node String)
    -> Gren.Syntax.Node.Node GrenSyntax.Import
    -> Print
import_ syntaxComments (Gren.Syntax.Node.Node incorrectImportRange syntaxImport) =
    let
        importRange : Gren.Syntax.Range.Range
        importRange =
            case syntaxImport.exposingList of
                Nothing ->
                    incorrectImportRange

                Just (Gren.Syntax.Node.Node syntaxExposingRange _) ->
                    { start = incorrectImportRange.start, end = syntaxExposingRange.end }

        (Gren.Syntax.Node.Node moduleNameRange syntaxModuleName) =
            syntaxImport.moduleName
    in
    printExactImport
        |> Print.followedBy
            (case syntaxImport.moduleAlias of
                Nothing ->
                    case commentsInRange { start = importRange.start, end = moduleNameRange.start } syntaxComments of
                        [] ->
                            Print.exactly
                                (" "
                                    ++ moduleName syntaxModuleName
                                )

                        comment0 :: comment1Up ->
                            Print.withIndentAtNextMultipleOf4
                                (Print.linebreakIndented
                                    |> Print.followedBy (comments (comment0 :: comment1Up))
                                    |> Print.followedBy Print.linebreakIndented
                                    |> Print.followedBy
                                        (Print.exactly (moduleName syntaxModuleName))
                                )

                Just (Gren.Syntax.Node.Node moduleAliasRange moduleAlias) ->
                    case commentsInRange { start = moduleNameRange.end, end = moduleAliasRange.start } syntaxComments of
                        [] ->
                            (case commentsInRange { start = importRange.start, end = moduleNameRange.start } syntaxComments of
                                [] ->
                                    Print.exactly
                                        (" "
                                            ++ moduleName syntaxModuleName
                                        )

                                moduleNameComment0 :: moduleNameComment1Up ->
                                    Print.withIndentAtNextMultipleOf4
                                        (Print.linebreakIndented
                                            |> Print.followedBy (comments (moduleNameComment0 :: moduleNameComment1Up))
                                            |> Print.followedBy Print.linebreakIndented
                                            |> Print.followedBy (Print.exactly (moduleName syntaxModuleName))
                                        )
                            )
                                |> Print.followedBy (Print.exactly (" as " ++ moduleName moduleAlias))

                        aliasComment0 :: aliasComment1Up ->
                            Print.withIndentAtNextMultipleOf4
                                (Print.linebreakIndented
                                    |> Print.followedBy
                                        (case commentsInRange { start = importRange.start, end = moduleNameRange.start } syntaxComments of
                                            [] ->
                                                Print.exactly (moduleName syntaxModuleName)

                                            moduleNameComment0 :: moduleNameComment1Up ->
                                                comments (moduleNameComment0 :: moduleNameComment1Up)
                                                    |> Print.followedBy Print.linebreakIndented
                                                    |> Print.followedBy (Print.exactly (moduleName syntaxModuleName))
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
                                                                (Print.exactly (moduleName moduleAlias))
                                                        )
                                                    )
                                            )
                                        )
                                )
            )
        |> Print.followedBy
            (case syntaxImport.exposingList of
                Nothing ->
                    Print.empty

                Just syntaxExposing ->
                    let
                        exposingPrint : Print
                        exposingPrint =
                            importExposing syntaxComments syntaxExposing

                        exposingPartStart : Gren.Syntax.Range.Location
                        exposingPartStart =
                            case syntaxImport.moduleAlias of
                                Nothing ->
                                    moduleNameRange.end

                                Just (Gren.Syntax.Node.Node moduleAliasRange _) ->
                                    moduleAliasRange.end
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
    List (Gren.Syntax.Node.Node String)
    -> Gren.Syntax.Node.Node Gren.Syntax.Exposing.Exposing
    -> Print
importExposing syntaxComments (Gren.Syntax.Node.Node exposingRange syntaxExposing) =
    case syntaxExposing of
        Gren.Syntax.Exposing.All _ ->
            printExactlyParensOpeningDotDotParensClosing

        Gren.Syntax.Exposing.Explicit exposingSet ->
            case exposingSet of
                [] ->
                    -- invalid syntax
                    printExactlyParensOpeningParensClosed

                expose0 :: expose1Up ->
                    exposingMulti syntaxComments
                        { fullRange = exposingRange, expose0 = expose0, expose1Up = expose1Up }


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
                                        moduleLevgrenultiLneCommentWithoutWhitespace

                                    notEmptyMultiLineComment ->
                                        comment notEmptyMultiLineComment
                                            |> Print.followedBy Print.linebreak
                            )
                    )


moduleLevgrenultiLneCommentWithoutWhitespace : Print.Print
moduleLevgrenultiLneCommentWithoutWhitespace =
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


{-| Print an [`Gren.Syntax.ModuleName.ModuleName`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-ModuleName#ModuleName)
-}
moduleName : Gren.Syntax.ModuleName.ModuleName -> String
moduleName syntaxModuleName =
    syntaxModuleName |> String.join "."


{-| Print a single [`Gren.Syntax.Exposing.TopLevelExpose`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Exposing#TopLevelExpose)
-}
expose : Gren.Syntax.Exposing.TopLevelExpose -> String
expose syntaxExpose =
    case syntaxExpose of
        Gren.Syntax.Exposing.InfixExpose operatorSymbol ->
            "(" ++ operatorSymbol ++ ")"

        Gren.Syntax.Exposing.FunctionExpose name ->
            name

        Gren.Syntax.Exposing.TypeOrAliasExpose name ->
            name

        Gren.Syntax.Exposing.TypeExpose syntaxExposeType ->
            case syntaxExposeType.open of
                Nothing ->
                    syntaxExposeType.name

                Just _ ->
                    syntaxExposeType.name ++ "(..)"


patternParenthesizedIfSpaceSeparated :
    List (Gren.Syntax.Node.Node String)
    -> Gren.Syntax.Node.Node Gren.Syntax.Pattern.Pattern
    -> Print
patternParenthesizedIfSpaceSeparated syntaxComments syntaxPattern =
    if patternIsSpaceSeparated (syntaxPattern |> Gren.Syntax.Node.value) then
        patternParenthesized syntaxComments syntaxPattern

    else
        patternNotParenthesized syntaxComments syntaxPattern


patternParenthesized :
    List (Gren.Syntax.Node.Node String)
    -> Gren.Syntax.Node.Node Gren.Syntax.Pattern.Pattern
    -> Print
patternParenthesized syntaxComments patternNode =
    parenthesized patternNotParenthesized
        { fullRange = patternNode |> Gren.Syntax.Node.range
        , notParenthesized = patternNode |> patternToNotParenthesized
        }
        syntaxComments


patternIsSpaceSeparated : Gren.Syntax.Pattern.Pattern -> Bool
patternIsSpaceSeparated syntaxPattern =
    case syntaxPattern of
        Gren.Syntax.Pattern.AllPattern ->
            False

        Gren.Syntax.Pattern.UnitPattern ->
            False

        Gren.Syntax.Pattern.VarPattern _ ->
            False

        Gren.Syntax.Pattern.CharPattern _ ->
            False

        Gren.Syntax.Pattern.StringPattern _ ->
            False

        Gren.Syntax.Pattern.IntPattern _ ->
            False

        Gren.Syntax.Pattern.HexPattern _ ->
            False

        Gren.Syntax.Pattern.FloatPattern _ ->
            -- invalid syntax
            False

        Gren.Syntax.Pattern.ParenthesizedPattern (Gren.Syntax.Node.Node _ inParens) ->
            patternIsSpaceSeparated inParens

        Gren.Syntax.Pattern.TuplePattern parts ->
            case parts of
                [ Gren.Syntax.Node.Node _ inParens ] ->
                    -- should be covered by ParenthesizedPattern
                    patternIsSpaceSeparated inParens

                [] ->
                    -- should be covered by UnitPattern
                    False

                [ _, _ ] ->
                    False

                [ _, _, _ ] ->
                    False

                _ :: _ :: _ :: _ :: _ ->
                    -- invalid syntax
                    False

        Gren.Syntax.Pattern.RecordPattern _ ->
            False

        Gren.Syntax.Pattern.UnConsPattern _ _ ->
            True

        Gren.Syntax.Pattern.ListPattern _ ->
            False

        Gren.Syntax.Pattern.NamedPattern _ argumentPatterns ->
            case argumentPatterns of
                [] ->
                    False

                _ :: _ ->
                    True

        Gren.Syntax.Pattern.AsPattern _ _ ->
            True


stringLiteral : Gren.Syntax.Node.Node String -> Print
stringLiteral (Gren.Syntax.Node.Node range stringContent) =
    let
        singleDoubleQuotedStringContentEscaped : String
        singleDoubleQuotedStringContentEscaped =
            stringContent
                |> String.foldl
                    (\contentChar soFar ->
                        soFar ++ singleDoubleQuotedStringCharToEscaped contentChar ++ ""
                    )
                    ""

        wasProbablyTripleDoubleQuoteOriginally : Bool
        wasProbablyTripleDoubleQuoteOriginally =
            (range.start.row /= range.end.row)
                || ((range.end.column - range.start.column)
                        - (singleDoubleQuotedStringContentEscaped |> stringUnicodeLength)
                        /= 2
                   )
    in
    if wasProbablyTripleDoubleQuoteOriginally then
        printExactlyDoubleQuoteDoubleQuoteDoubleQuote
            |> Print.followedBy
                (stringContent
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

    else
        Print.exactly ("\"" ++ singleDoubleQuotedStringContentEscaped ++ "\"")


stringUnicodeLength : String -> Int
stringUnicodeLength string =
    string |> String.foldl (\_ soFar -> soFar + 1) 0


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
    String.toUpper (intToHexString (Char.toCode character))


characterIsNotPrint : Char -> Bool
characterIsNotPrint character =
    if
        -- Unicode.getCategory is very expensive so we shortcut if at all possible
        Char.Extra.isLatinAlphaNumOrUnderscoreFast character
            || (character == ' ')
            || (character == '.')
            || (character == '!')
            || (character == '?')
            || (character == '-')
            || (character == ':')
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

                    Unicode.OtherNotAssigned ->
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
    Gren.Syntax.Node.Node Gren.Syntax.Pattern.Pattern
    -> Gren.Syntax.Node.Node Gren.Syntax.Pattern.Pattern
patternToNotParenthesized (Gren.Syntax.Node.Node fullRange syntaxPattern) =
    -- IGNORE TCO
    case syntaxPattern of
        Gren.Syntax.Pattern.ParenthesizedPattern inParens ->
            inParens |> patternToNotParenthesized

        Gren.Syntax.Pattern.TuplePattern parts ->
            case parts of
                [ inParens ] ->
                    -- should be covered by ParenthesizedPattern
                    inParens |> patternToNotParenthesized

                [ part0, part1 ] ->
                    Gren.Syntax.Node.Node fullRange (Gren.Syntax.Pattern.TuplePattern [ part0, part1 ])

                [ part0, part1, part2 ] ->
                    Gren.Syntax.Node.Node fullRange (Gren.Syntax.Pattern.TuplePattern [ part0, part1, part2 ])

                [] ->
                    -- should be covered by UnitPattern
                    Gren.Syntax.Node.Node fullRange Gren.Syntax.Pattern.UnitPattern

                part0 :: part1 :: part2 :: part3 :: part4Up ->
                    -- invalid syntax
                    Gren.Syntax.Node.Node fullRange (Gren.Syntax.Pattern.TuplePattern (part0 :: part1 :: part2 :: part3 :: part4Up))

        Gren.Syntax.Pattern.AllPattern ->
            Gren.Syntax.Node.Node fullRange Gren.Syntax.Pattern.AllPattern

        Gren.Syntax.Pattern.UnitPattern ->
            Gren.Syntax.Node.Node fullRange Gren.Syntax.Pattern.UnitPattern

        Gren.Syntax.Pattern.VarPattern name ->
            Gren.Syntax.Node.Node fullRange (Gren.Syntax.Pattern.VarPattern name)

        Gren.Syntax.Pattern.CharPattern char ->
            Gren.Syntax.Node.Node fullRange (Gren.Syntax.Pattern.CharPattern char)

        Gren.Syntax.Pattern.StringPattern string ->
            Gren.Syntax.Node.Node fullRange (Gren.Syntax.Pattern.StringPattern string)

        Gren.Syntax.Pattern.IntPattern int ->
            Gren.Syntax.Node.Node fullRange (Gren.Syntax.Pattern.IntPattern int)

        Gren.Syntax.Pattern.HexPattern int ->
            Gren.Syntax.Node.Node fullRange (Gren.Syntax.Pattern.HexPattern int)

        Gren.Syntax.Pattern.FloatPattern float ->
            -- invalid syntax
            Gren.Syntax.Node.Node fullRange (Gren.Syntax.Pattern.FloatPattern float)

        Gren.Syntax.Pattern.RecordPattern fields ->
            Gren.Syntax.Node.Node fullRange (Gren.Syntax.Pattern.RecordPattern fields)

        Gren.Syntax.Pattern.UnConsPattern headPattern tailPattern ->
            Gren.Syntax.Node.Node fullRange (Gren.Syntax.Pattern.UnConsPattern headPattern tailPattern)

        Gren.Syntax.Pattern.ListPattern elementPatterns ->
            Gren.Syntax.Node.Node fullRange (Gren.Syntax.Pattern.ListPattern elementPatterns)

        Gren.Syntax.Pattern.NamedPattern syntaxQualifiedNameRef argumentPatterns ->
            Gren.Syntax.Node.Node fullRange (Gren.Syntax.Pattern.NamedPattern syntaxQualifiedNameRef argumentPatterns)

        Gren.Syntax.Pattern.AsPattern aliasedPattern aliasNameNode ->
            Gren.Syntax.Node.Node fullRange (Gren.Syntax.Pattern.AsPattern aliasedPattern aliasNameNode)


{-| Print an [`Gren.Syntax.Pattern.Pattern`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Pattern#Pattern)
-}
patternNotParenthesized :
    List (Gren.Syntax.Node.Node String)
    -> Gren.Syntax.Node.Node Gren.Syntax.Pattern.Pattern
    -> Print
patternNotParenthesized syntaxComments (Gren.Syntax.Node.Node fullRange syntaxPattern) =
    -- IGNORE TCO
    case syntaxPattern of
        Gren.Syntax.Pattern.AllPattern ->
            printExactlyUnderscore

        Gren.Syntax.Pattern.UnitPattern ->
            printExactlyParensOpeningParensClosed

        Gren.Syntax.Pattern.VarPattern name ->
            Print.exactly name

        Gren.Syntax.Pattern.CharPattern char ->
            Print.exactly (charLiteral char)

        Gren.Syntax.Pattern.StringPattern string ->
            stringLiteral (Gren.Syntax.Node.Node fullRange string)

        Gren.Syntax.Pattern.IntPattern int ->
            Print.exactly (intLiteral int)

        Gren.Syntax.Pattern.HexPattern int ->
            Print.exactly (hexLiteral int)

        Gren.Syntax.Pattern.FloatPattern float ->
            -- invalid syntax
            Print.exactly (String.fromFloat float)

        Gren.Syntax.Pattern.ParenthesizedPattern inParens ->
            let
                commentsBeforeInParens : List String
                commentsBeforeInParens =
                    commentsInRange { start = fullRange.start, end = inParens |> Gren.Syntax.Node.range |> .start } syntaxComments

                commentsAfterInParens : List String
                commentsAfterInParens =
                    commentsInRange { start = inParens |> Gren.Syntax.Node.range |> .end, end = fullRange.end } syntaxComments
            in
            case ( commentsBeforeInParens, commentsAfterInParens ) of
                ( [], [] ) ->
                    patternNotParenthesized syntaxComments inParens

                _ ->
                    parenthesized patternNotParenthesized
                        { notParenthesized = inParens |> patternToNotParenthesized
                        , fullRange = fullRange
                        }
                        syntaxComments

        Gren.Syntax.Pattern.TuplePattern parts ->
            case parts of
                [ part0, part1 ] ->
                    { part0 = part0, part1 = part1, fullRange = fullRange }
                        |> tuple
                            { printPartNotParenthesized = patternNotParenthesized
                            , lineSpreadMinimum = Print.SingleLine
                            }
                            syntaxComments

                [ part0, part1, part2 ] ->
                    { part0 = part0, part1 = part1, part2 = part2, fullRange = fullRange }
                        |> triple
                            { printPartNotParenthesized = patternNotParenthesized
                            , lineSpreadMinimum = Print.SingleLine
                            }
                            syntaxComments

                [] ->
                    -- should be covered by UnitPattern
                    printExactlyParensOpeningParensClosed

                [ inParens ] ->
                    -- should be covered by ParenthesizedPattern
                    let
                        commentsBeforeInParens : List String
                        commentsBeforeInParens =
                            commentsInRange { start = fullRange.start, end = inParens |> Gren.Syntax.Node.range |> .start } syntaxComments

                        commentsAfterInParens : List String
                        commentsAfterInParens =
                            commentsInRange { start = inParens |> Gren.Syntax.Node.range |> .end, end = fullRange.end } syntaxComments
                    in
                    case ( commentsBeforeInParens, commentsAfterInParens ) of
                        ( [], [] ) ->
                            patternNotParenthesized syntaxComments inParens

                        _ ->
                            parenthesized patternNotParenthesized
                                { notParenthesized = inParens |> patternToNotParenthesized
                                , fullRange = fullRange
                                }
                                syntaxComments

                part0 :: part1 :: part2 :: part3 :: part4Up ->
                    -- invalid syntax
                    invalidNTuple patternNotParenthesized
                        syntaxComments
                        { fullRange = fullRange, part0 = part0, part1 = part1, part2 = part2, part3 = part3, part4Up = part4Up }

        Gren.Syntax.Pattern.RecordPattern fields ->
            patternRecord syntaxComments
                { fullRange = fullRange, fields = fields }

        Gren.Syntax.Pattern.UnConsPattern headPattern tailPattern ->
            patternCons syntaxComments
                { head = headPattern, tail = tailPattern }

        Gren.Syntax.Pattern.ListPattern elementPatterns ->
            patternList syntaxComments
                { fullRange = fullRange, elements = elementPatterns }

        Gren.Syntax.Pattern.NamedPattern syntaxQualifiedNameRef argumentPatterns ->
            construct
                { printArgumentParenthesizedIfSpaceSeparated =
                    patternParenthesizedIfSpaceSeparated
                , lineSpreadMinimum = Print.SingleLine
                }
                syntaxComments
                { fullRange = fullRange
                , start =
                    qualifiedReference
                        { qualification = syntaxQualifiedNameRef.moduleName
                        , unqualified = syntaxQualifiedNameRef.name
                        }
                , arguments = argumentPatterns
                }

        Gren.Syntax.Pattern.AsPattern aliasedPattern aliasNameNode ->
            patternAs syntaxComments
                { aliasedPattern = aliasedPattern, aliasNameNode = aliasNameNode }


patternList :
    List (Gren.Syntax.Node.Node String)
    ->
        { elements : List (Gren.Syntax.Node.Node Gren.Syntax.Pattern.Pattern)
        , fullRange : Gren.Syntax.Range.Range
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
                    { endLocation : Gren.Syntax.Range.Location
                    , reverse : List Print
                    }
                elementPrintsWithCommentsBefore =
                    (element0 :: element1Up)
                        |> List.foldl
                            (\elementNode soFar ->
                                let
                                    (Gren.Syntax.Node.Node elementRange _) =
                                        elementNode

                                    elementPrint : Print
                                    elementPrint =
                                        patternNotParenthesized syntaxComments
                                            elementNode
                                in
                                { endLocation = elementRange.end
                                , reverse =
                                    (case
                                        commentsInRange { start = soFar.endLocation, end = elementRange.start }
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
    List (Gren.Syntax.Node.Node String)
    ->
        { head : Gren.Syntax.Node.Node Gren.Syntax.Pattern.Pattern
        , tail : Gren.Syntax.Node.Node Gren.Syntax.Pattern.Pattern
        }
    -> Print
patternCons syntaxComments syntaxCons =
    let
        headPrint : Print
        headPrint =
            patternParenthesizedIfSpaceSeparated syntaxComments syntaxCons.head

        tailPatterns : List (Gren.Syntax.Node.Node Gren.Syntax.Pattern.Pattern)
        tailPatterns =
            syntaxCons.tail |> patternConsExpand

        tailPatternPrintsAndCommentsBeforeReverse : List Print
        tailPatternPrintsAndCommentsBeforeReverse =
            tailPatterns
                |> List.foldl
                    (\tailPatternNode soFar ->
                        let
                            (Gren.Syntax.Node.Node tailPatternRange _) =
                                tailPatternNode

                            print : Print
                            print =
                                patternParenthesizedIfSpaceSeparated syntaxComments
                                    tailPatternNode
                        in
                        { reverse =
                            (case
                                commentsInRange { start = soFar.endLocation, end = tailPatternRange.start }
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
                        , endLocation = tailPatternRange.end
                        }
                    )
                    { reverse = []
                    , endLocation = syntaxCons.head |> Gren.Syntax.Node.range |> .end
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
    Gren.Syntax.Node.Node Gren.Syntax.Pattern.Pattern
    -> List (Gren.Syntax.Node.Node Gren.Syntax.Pattern.Pattern)
patternConsExpand (Gren.Syntax.Node.Node fulRange syntaxPattern) =
    case syntaxPattern of
        Gren.Syntax.Pattern.UnConsPattern headPattern tailPattern ->
            headPattern :: patternConsExpand tailPattern

        Gren.Syntax.Pattern.AllPattern ->
            [ Gren.Syntax.Node.Node fulRange Gren.Syntax.Pattern.AllPattern ]

        Gren.Syntax.Pattern.UnitPattern ->
            [ Gren.Syntax.Node.Node fulRange Gren.Syntax.Pattern.UnitPattern ]

        Gren.Syntax.Pattern.CharPattern char ->
            [ Gren.Syntax.Node.Node fulRange (Gren.Syntax.Pattern.CharPattern char) ]

        Gren.Syntax.Pattern.StringPattern string ->
            [ Gren.Syntax.Node.Node fulRange (Gren.Syntax.Pattern.StringPattern string) ]

        Gren.Syntax.Pattern.IntPattern int ->
            [ Gren.Syntax.Node.Node fulRange (Gren.Syntax.Pattern.IntPattern int) ]

        Gren.Syntax.Pattern.HexPattern int ->
            [ Gren.Syntax.Node.Node fulRange (Gren.Syntax.Pattern.HexPattern int) ]

        Gren.Syntax.Pattern.FloatPattern float ->
            [ Gren.Syntax.Node.Node fulRange (Gren.Syntax.Pattern.FloatPattern float) ]

        Gren.Syntax.Pattern.TuplePattern parts ->
            case parts of
                [ part0, part1 ] ->
                    [ Gren.Syntax.Node.Node fulRange (Gren.Syntax.Pattern.TuplePattern [ part0, part1 ]) ]

                [ part0, part1, part2 ] ->
                    [ Gren.Syntax.Node.Node fulRange (Gren.Syntax.Pattern.TuplePattern [ part0, part1, part2 ]) ]

                [] ->
                    -- should be handled by UnitPattern
                    [ Gren.Syntax.Node.Node fulRange Gren.Syntax.Pattern.UnitPattern ]

                [ inParens ] ->
                    -- should be handled by ParenthesizedPattern
                    [ Gren.Syntax.Node.Node fulRange (Gren.Syntax.Pattern.TuplePattern [ inParens ]) ]

                part0 :: part1 :: part2 :: part3 :: part4Up ->
                    -- should be handled by ParenthesizedPattern
                    [ Gren.Syntax.Node.Node fulRange (Gren.Syntax.Pattern.TuplePattern (part0 :: part1 :: part2 :: part3 :: part4Up)) ]

        Gren.Syntax.Pattern.RecordPattern fields ->
            [ Gren.Syntax.Node.Node fulRange (Gren.Syntax.Pattern.RecordPattern fields) ]

        Gren.Syntax.Pattern.ListPattern elements ->
            [ Gren.Syntax.Node.Node fulRange (Gren.Syntax.Pattern.ListPattern elements) ]

        Gren.Syntax.Pattern.VarPattern variableName ->
            [ Gren.Syntax.Node.Node fulRange (Gren.Syntax.Pattern.VarPattern variableName) ]

        Gren.Syntax.Pattern.NamedPattern reference parameters ->
            [ Gren.Syntax.Node.Node fulRange (Gren.Syntax.Pattern.NamedPattern reference parameters) ]

        Gren.Syntax.Pattern.AsPattern aliasedPattern aliasName ->
            [ Gren.Syntax.Node.Node fulRange (Gren.Syntax.Pattern.AsPattern aliasedPattern aliasName) ]

        Gren.Syntax.Pattern.ParenthesizedPattern inParens ->
            [ Gren.Syntax.Node.Node fulRange (Gren.Syntax.Pattern.ParenthesizedPattern inParens) ]


patternAs :
    List (Gren.Syntax.Node.Node String)
    ->
        { aliasNameNode : Gren.Syntax.Node.Node String
        , aliasedPattern : Gren.Syntax.Node.Node Gren.Syntax.Pattern.Pattern
        }
    -> Print
patternAs syntaxComments syntaxAs =
    let
        aliasedPatternPrint : Print
        aliasedPatternPrint =
            patternParenthesizedIfSpaceSeparated syntaxComments syntaxAs.aliasedPattern

        commentsBeforeAliasName : List String
        commentsBeforeAliasName =
            commentsInRange
                { start = syntaxAs.aliasedPattern |> Gren.Syntax.Node.range |> .end
                , end = syntaxAs.aliasNameNode |> Gren.Syntax.Node.range |> .start
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
            Print.exactly (syntaxAs.aliasNameNode |> Gren.Syntax.Node.value)
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
    List (Gren.Syntax.Node.Node String)
    ->
        { fields : List (Gren.Syntax.Node.Node String)
        , fullRange : Gren.Syntax.Range.Range
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
                    { endLocation : Gren.Syntax.Range.Location
                    , reverse : List Print
                    }
                fieldPrintsWithCommentsBefore =
                    (field0 :: field1Up)
                        |> List.foldl
                            (\(Gren.Syntax.Node.Node elementRange fieldName) soFar ->
                                { endLocation = elementRange.end
                                , reverse =
                                    (case commentsInRange { start = soFar.endLocation, end = elementRange.start } syntaxComments of
                                        [] ->
                                            Print.exactly fieldName

                                        comment0 :: comment1Up ->
                                            let
                                                commentsBefore : { print : Print, lineSpread : Print.LineSpread }
                                                commentsBefore =
                                                    collapsibleComments (comment0 :: comment1Up)
                                            in
                                            commentsBefore.print
                                                |> Print.followedBy
                                                    (Print.spaceOrLinebreakIndented commentsBefore.lineSpread)
                                                |> Print.followedBy (Print.exactly fieldName)
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
    List (Gren.Syntax.Node.Node String)
    ->
        { fullRange : Gren.Syntax.Range.Range
        , recordVariable : Gren.Syntax.Node.Node String
        , fields :
            List
                (Gren.Syntax.Node.Node
                    ( Gren.Syntax.Node.Node String
                    , Gren.Syntax.Node.Node Gren.Syntax.TypeAnnotation.TypeAnnotation
                    )
                )
        }
    -> Print
typeRecordExtension syntaxComments syntaxRecordExtension =
    let
        commentsBeforeRecordVariable : List String
        commentsBeforeRecordVariable =
            commentsInRange
                { start = syntaxRecordExtension.fullRange.start
                , end = syntaxRecordExtension.recordVariable |> Gren.Syntax.Node.range |> .start
                }
                syntaxComments

        commentsCollapsibleBeforeRecordVariable : { print : Print, lineSpread : Print.LineSpread }
        commentsCollapsibleBeforeRecordVariable =
            collapsibleComments commentsBeforeRecordVariable

        fieldPrintsAndComments :
            { endLocation : Gren.Syntax.Range.Location
            , reverse :
                List
                    { syntax :
                        ( Gren.Syntax.Node.Node String
                        , Gren.Syntax.Node.Node Gren.Syntax.TypeAnnotation.TypeAnnotation
                        )
                    , valuePrint : Print
                    , maybeCommentsBeforeName : Maybe { print : Print, lineSpread : Print.LineSpread }
                    , maybeCommentsBetweenNameAndValue : Maybe { print : Print, lineSpread : Print.LineSpread }
                    }
            }
        fieldPrintsAndComments =
            syntaxRecordExtension.fields
                |> List.foldl
                    (\(Gren.Syntax.Node.Node _ ( Gren.Syntax.Node.Node fieldNameRange fieldName, fieldValueNode )) soFar ->
                        let
                            (Gren.Syntax.Node.Node fieldValueRange _) =
                                fieldValueNode

                            commentsBeforeName : List String
                            commentsBeforeName =
                                commentsInRange
                                    { start = soFar.endLocation, end = fieldNameRange.start }
                                    syntaxComments

                            commentsBetweenNameAndValue : List String
                            commentsBetweenNameAndValue =
                                commentsInRange
                                    { start = fieldNameRange.start, end = fieldValueRange.start }
                                    syntaxComments
                        in
                        { endLocation = fieldValueRange.end
                        , reverse =
                            { syntax = ( Gren.Syntax.Node.Node fieldNameRange fieldName, fieldValueNode )
                            , valuePrint = typeNotParenthesized syntaxComments fieldValueNode
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
                            |> Gren.Syntax.Node.range
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
                (syntaxRecordExtension.recordVariable |> Gren.Syntax.Node.value)
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
                                        ( Gren.Syntax.Node.Node fieldNameRange fieldName, fieldValueNode ) =
                                            field.syntax

                                        lineSpreadBetweenNameAndValueNotConsideringComments : () -> Print.LineSpread
                                        lineSpreadBetweenNameAndValueNotConsideringComments () =
                                            lineSpreadInRange
                                                { start = fieldNameRange.start
                                                , end = fieldValueNode |> Gren.Syntax.Node.range |> .end
                                                }
                                                |> Print.lineSpreadMergeWith
                                                    (\() -> field.valuePrint |> Print.lineSpread)
                                    in
                                    Print.withIndentIncreasedBy 2
                                        ((case field.maybeCommentsBeforeName of
                                            Nothing ->
                                                Print.exactly (fieldName ++ " :")

                                            Just commentsBeforeName ->
                                                commentsBeforeName.print
                                                    |> Print.followedBy
                                                        (Print.spaceOrLinebreakIndented
                                                            commentsBeforeName.lineSpread
                                                        )
                                                    |> Print.followedBy (Print.exactly (fieldName ++ " :"))
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
                    |> -- yes, gren-format indents trailing comments
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
        List (Gren.Syntax.Node.Node String) -> Gren.Syntax.Node.Node a -> Print
    , lineSpreadMinimum : Print.LineSpread
    }
    -> List (Gren.Syntax.Node.Node String)
    ->
        { arguments : List (Gren.Syntax.Node.Node a)
        , fullRange : Gren.Syntax.Range.Range
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
                                    , end = argument |> Gren.Syntax.Node.range |> .start
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
                        , endLocation = argument |> Gren.Syntax.Node.range |> .end
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


tuple :
    { printPartNotParenthesized :
        List (Gren.Syntax.Node.Node String) -> Gren.Syntax.Node.Node part -> Print
    , lineSpreadMinimum : Print.LineSpread
    }
    -> List (Gren.Syntax.Node.Node String)
    ->
        { fullRange : Gren.Syntax.Range.Range
        , part0 : Gren.Syntax.Node.Node part
        , part1 : Gren.Syntax.Node.Node part
        }
    -> Print
tuple config syntaxComments syntaxTuple =
    let
        beforePart0Comments : List String
        beforePart0Comments =
            commentsInRange
                { start = syntaxTuple.fullRange.start
                , end = syntaxTuple.part0 |> Gren.Syntax.Node.range |> .start
                }
                syntaxComments

        beforePart0CommentsCollapsible : { print : Print, lineSpread : Print.LineSpread }
        beforePart0CommentsCollapsible =
            collapsibleComments beforePart0Comments

        beforePart1Comments : List String
        beforePart1Comments =
            commentsInRange
                { start = syntaxTuple.part0 |> Gren.Syntax.Node.range |> .end
                , end = syntaxTuple.part1 |> Gren.Syntax.Node.range |> .start
                }
                syntaxComments

        beforePart1CommentsCollapsible : { print : Print, lineSpread : Print.LineSpread }
        beforePart1CommentsCollapsible =
            collapsibleComments beforePart1Comments

        afterPart1Comments : List String
        afterPart1Comments =
            commentsInRange
                { start = syntaxTuple.part1 |> Gren.Syntax.Node.range |> .end
                , end = syntaxTuple.fullRange.end
                }
                syntaxComments

        lineSpread : Print.LineSpread
        lineSpread =
            config.lineSpreadMinimum
                |> Print.lineSpreadMergeWithStrict
                    beforePart0CommentsCollapsible.lineSpread
                |> Print.lineSpreadMergeWithStrict
                    beforePart1CommentsCollapsible.lineSpread
                |> Print.lineSpreadMergeWith
                    (\() ->
                        case afterPart1Comments of
                            _ :: _ ->
                                Print.MultipleLines

                            [] ->
                                Print.SingleLine
                    )

        part0Print : Print
        part0Print =
            config.printPartNotParenthesized syntaxComments syntaxTuple.part0

        part1Print : Print
        part1Print =
            config.printPartNotParenthesized syntaxComments syntaxTuple.part1
    in
    printExactlyParensOpeningSpace
        |> Print.followedBy
            (Print.withIndentIncreasedBy 2
                (case beforePart0Comments of
                    [] ->
                        part0Print

                    _ :: _ ->
                        beforePart0CommentsCollapsible.print
                            |> Print.followedBy
                                (Print.spaceOrLinebreakIndented
                                    (beforePart0CommentsCollapsible.lineSpread
                                        |> Print.lineSpreadMergeWith
                                            (\() -> part0Print |> Print.lineSpread)
                                    )
                                )
                            |> Print.followedBy part0Print
                )
            )
        |> Print.followedBy (Print.emptyOrLinebreakIndented lineSpread)
        |> Print.followedBy printExactlyCommaSpace
        |> Print.followedBy
            (Print.withIndentIncreasedBy 2
                ((case beforePart1Comments of
                    [] ->
                        part1Print

                    _ :: _ ->
                        beforePart1CommentsCollapsible.print
                            |> Print.followedBy
                                (Print.spaceOrLinebreakIndented
                                    (beforePart1CommentsCollapsible.lineSpread
                                        |> Print.lineSpreadMergeWith
                                            (\() -> part1Print |> Print.lineSpread)
                                    )
                                )
                            |> Print.followedBy part1Print
                 )
                    |> Print.followedBy
                        (case afterPart1Comments of
                            [] ->
                                Print.empty

                            comment0 :: comment1Up ->
                                Print.linebreakIndented
                                    |> Print.followedBy (comments (comment0 :: comment1Up))
                        )
                )
            )
        |> Print.followedBy (Print.spaceOrLinebreakIndented lineSpread)
        |> Print.followedBy printExactlyParensClosing


triple :
    { printPartNotParenthesized :
        List (Gren.Syntax.Node.Node String) -> Gren.Syntax.Node.Node part -> Print
    , lineSpreadMinimum : Print.LineSpread
    }
    -> List (Gren.Syntax.Node.Node String)
    ->
        { fullRange : Gren.Syntax.Range.Range
        , part0 : Gren.Syntax.Node.Node part
        , part1 : Gren.Syntax.Node.Node part
        , part2 : Gren.Syntax.Node.Node part
        }
    -> Print
triple config syntaxComments syntaxTriple =
    let
        beforePart0Comments : List String
        beforePart0Comments =
            commentsInRange
                { start = syntaxTriple.fullRange.start
                , end = syntaxTriple.part0 |> Gren.Syntax.Node.range |> .start
                }
                syntaxComments

        beforePart0CommentsCollapsible : { print : Print, lineSpread : Print.LineSpread }
        beforePart0CommentsCollapsible =
            collapsibleComments beforePart0Comments

        beforePart1Comments : List String
        beforePart1Comments =
            commentsInRange
                { start = syntaxTriple.part0 |> Gren.Syntax.Node.range |> .end
                , end = syntaxTriple.part1 |> Gren.Syntax.Node.range |> .start
                }
                syntaxComments

        beforePart1CommentsCollapsible : { print : Print, lineSpread : Print.LineSpread }
        beforePart1CommentsCollapsible =
            collapsibleComments beforePart1Comments

        beforePart2Comments : List String
        beforePart2Comments =
            commentsInRange
                { start = syntaxTriple.part1 |> Gren.Syntax.Node.range |> .end
                , end = syntaxTriple.part2 |> Gren.Syntax.Node.range |> .start
                }
                syntaxComments

        beforePart2CommentsCollapsible : { print : Print, lineSpread : Print.LineSpread }
        beforePart2CommentsCollapsible =
            collapsibleComments beforePart2Comments

        afterPart2Comments : List String
        afterPart2Comments =
            commentsInRange
                { start = syntaxTriple.part2 |> Gren.Syntax.Node.range |> .end
                , end = syntaxTriple.fullRange.end
                }
                syntaxComments

        lineSpread : Print.LineSpread
        lineSpread =
            config.lineSpreadMinimum
                |> Print.lineSpreadMergeWithStrict
                    beforePart0CommentsCollapsible.lineSpread
                |> Print.lineSpreadMergeWithStrict
                    beforePart1CommentsCollapsible.lineSpread
                |> Print.lineSpreadMergeWithStrict
                    beforePart2CommentsCollapsible.lineSpread
                |> Print.lineSpreadMergeWith
                    (\() ->
                        case afterPart2Comments of
                            _ :: _ ->
                                Print.MultipleLines

                            [] ->
                                Print.SingleLine
                    )

        part0Print : Print
        part0Print =
            config.printPartNotParenthesized syntaxComments syntaxTriple.part0

        part1Print : Print
        part1Print =
            config.printPartNotParenthesized syntaxComments syntaxTriple.part1

        part2Print : Print
        part2Print =
            config.printPartNotParenthesized syntaxComments syntaxTriple.part2
    in
    printExactlyParensOpeningSpace
        |> Print.followedBy
            (Print.withIndentIncreasedBy 2
                (case beforePart0Comments of
                    [] ->
                        part0Print

                    _ :: _ ->
                        beforePart0CommentsCollapsible.print
                            |> Print.followedBy
                                (Print.spaceOrLinebreakIndented
                                    (beforePart0CommentsCollapsible.lineSpread
                                        |> Print.lineSpreadMergeWith
                                            (\() -> part0Print |> Print.lineSpread)
                                    )
                                )
                            |> Print.followedBy part0Print
                )
            )
        |> Print.followedBy (Print.emptyOrLinebreakIndented lineSpread)
        |> Print.followedBy printExactlyCommaSpace
        |> Print.followedBy
            (Print.withIndentIncreasedBy 2
                (case beforePart1Comments of
                    [] ->
                        part1Print

                    _ :: _ ->
                        beforePart1CommentsCollapsible.print
                            |> Print.followedBy
                                (Print.spaceOrLinebreakIndented
                                    (beforePart1CommentsCollapsible.lineSpread
                                        |> Print.lineSpreadMergeWith
                                            (\() -> part1Print |> Print.lineSpread)
                                    )
                                )
                            |> Print.followedBy part1Print
                )
            )
        |> Print.followedBy (Print.emptyOrLinebreakIndented lineSpread)
        |> Print.followedBy printExactlyCommaSpace
        |> Print.followedBy
            (Print.withIndentIncreasedBy 2
                ((case beforePart2Comments of
                    [] ->
                        part2Print

                    _ :: _ ->
                        beforePart2CommentsCollapsible.print
                            |> Print.followedBy
                                (Print.spaceOrLinebreakIndented
                                    (beforePart2CommentsCollapsible.lineSpread
                                        |> Print.lineSpreadMergeWith
                                            (\() -> part2Print |> Print.lineSpread)
                                    )
                                )
                            |> Print.followedBy part2Print
                 )
                    |> Print.followedBy
                        (case afterPart2Comments of
                            [] ->
                                Print.empty

                            comment0 :: comment1Up ->
                                Print.linebreakIndented
                                    |> Print.followedBy (comments (comment0 :: comment1Up))
                        )
                )
            )
        |> Print.followedBy (Print.spaceOrLinebreakIndented lineSpread)
        |> Print.followedBy printExactlyParensClosing


invalidNTuple :
    (b -> a -> Print)
    -> b
    ->
        { fullRange : Gren.Syntax.Range.Range
        , part0 : a
        , part1 : a
        , part2 : a
        , part3 : a
        , part4Up : List a
        }
    -> Print
invalidNTuple printPartNotParenthesized syntaxComments syntaxTuple =
    -- low-effort (eating comments etc) bc this shouldn't parse in the first place
    let
        lineSpread : Print.LineSpread
        lineSpread =
            lineSpreadInRange syntaxTuple.fullRange
    in
    printExactlyParensOpeningSpace
        |> Print.followedBy
            ((syntaxTuple.part0 :: syntaxTuple.part1 :: syntaxTuple.part2 :: syntaxTuple.part3 :: syntaxTuple.part4Up)
                |> Print.listMapAndIntersperseAndFlatten
                    (\part ->
                        Print.withIndentIncreasedBy 2
                            (printPartNotParenthesized syntaxComments part)
                    )
                    (Print.emptyOrLinebreakIndented lineSpread
                        |> Print.followedBy printExactlyCommaSpace
                    )
            )
        |> Print.followedBy (Print.spaceOrLinebreakIndented lineSpread)
        |> Print.followedBy printExactlyParensClosing


recordLiteral :
    { nameValueSeparator : String
    , printValueNotParenthesized : List (Gren.Syntax.Node.Node String) -> Gren.Syntax.Node.Node fieldValue -> Print
    }
    -> List (Gren.Syntax.Node.Node String)
    ->
        { fields :
            List
                (Gren.Syntax.Node.Node
                    ( Gren.Syntax.Node.Node String
                    , Gren.Syntax.Node.Node fieldValue
                    )
                )
        , fullRange : Gren.Syntax.Range.Range
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
                    { endLocation : Gren.Syntax.Range.Location
                    , reverse :
                        List
                            { syntax :
                                ( Gren.Syntax.Node.Node String
                                , Gren.Syntax.Node.Node fieldValue
                                )
                            , valuePrint : Print
                            , maybeCommentsBeforeName : Maybe { print : Print, lineSpread : Print.LineSpread }
                            , maybeCommentsBetweenNameAndValue : Maybe { print : Print, lineSpread : Print.LineSpread }
                            }
                    }
                fieldPrintsAndComments =
                    (field0 :: field1Up)
                        |> List.foldl
                            (\(Gren.Syntax.Node.Node _ ( Gren.Syntax.Node.Node fieldNameRange fieldName, fieldValueNode )) soFar ->
                                let
                                    (Gren.Syntax.Node.Node fieldValueRange _) =
                                        fieldValueNode

                                    commentsBeforeName : List String
                                    commentsBeforeName =
                                        commentsInRange
                                            { start = soFar.endLocation, end = fieldNameRange.start }
                                            syntaxComments

                                    commentsBetweenNameAndValue : List String
                                    commentsBetweenNameAndValue =
                                        commentsInRange
                                            { start = fieldNameRange.start, end = fieldValueRange.start }
                                            syntaxComments
                                in
                                { endLocation = fieldValueRange.end
                                , reverse =
                                    { syntax = ( Gren.Syntax.Node.Node fieldNameRange fieldName, fieldValueNode )
                                    , valuePrint = fieldSpecific.printValueNotParenthesized syntaxComments fieldValueNode
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
                            (\field ->
                                let
                                    ( Gren.Syntax.Node.Node fieldNameRange fieldName, fieldValue ) =
                                        field.syntax

                                    lineSpreadBetweenNameAndValueNotConsideringComments : () -> Print.LineSpread
                                    lineSpreadBetweenNameAndValueNotConsideringComments () =
                                        lineSpreadInRange
                                            { start = fieldNameRange.start
                                            , end = fieldValue |> Gren.Syntax.Node.range |> .end
                                            }
                                            |> Print.lineSpreadMergeWith
                                                (\() -> field.valuePrint |> Print.lineSpread)

                                    nameSeparatorValuePrint : Print
                                    nameSeparatorValuePrint =
                                        Print.exactly (fieldName ++ " " ++ fieldSpecific.nameValueSeparator)
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
                                in
                                Print.withIndentIncreasedBy 2
                                    (case field.maybeCommentsBeforeName of
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
qualifiedReference : { qualification : List String, unqualified : String } -> String
qualifiedReference syntaxReference =
    case syntaxReference.qualification of
        [] ->
            syntaxReference.unqualified

        modulePartHead :: modulePartTail ->
            modulePartHead
                ++ (modulePartTail
                        |> listMapAndFlattenToString
                            (\modulePart -> "." ++ modulePart)
                   )
                ++ "."
                ++ syntaxReference.unqualified


lineSpreadBetweenRanges : Gren.Syntax.Range.Range -> Gren.Syntax.Range.Range -> Print.LineSpread
lineSpreadBetweenRanges earlierRange laterRange =
    if laterRange.end.row - earlierRange.start.row == 0 then
        Print.SingleLine

    else
        Print.MultipleLines


lineSpreadBetweenNodes : Gren.Syntax.Node.Node a_ -> Gren.Syntax.Node.Node b_ -> Print.LineSpread
lineSpreadBetweenNodes (Gren.Syntax.Node.Node earlierRange _) (Gren.Syntax.Node.Node laterRange _) =
    if laterRange.end.row - earlierRange.start.row == 0 then
        Print.SingleLine

    else
        Print.MultipleLines


lineSpreadInNode : Gren.Syntax.Node.Node a_ -> Print.LineSpread
lineSpreadInNode (Gren.Syntax.Node.Node range _) =
    lineSpreadInRange range


typeFunctionNotParenthesized :
    List (Gren.Syntax.Node.Node String)
    ->
        { fullRange : Gren.Syntax.Range.Range
        , inType : Gren.Syntax.Node.Node Gren.Syntax.TypeAnnotation.TypeAnnotation
        , outType : Gren.Syntax.Node.Node Gren.Syntax.TypeAnnotation.TypeAnnotation
        }
    -> Print
typeFunctionNotParenthesized syntaxComments function =
    let
        afterArrowTypes :
            { beforeRightest : List (Gren.Syntax.Node.Node Gren.Syntax.TypeAnnotation.TypeAnnotation)
            , rightest : Gren.Syntax.Node.Node Gren.Syntax.TypeAnnotation.TypeAnnotation
            }
        afterArrowTypes =
            typeFunctionExpand function.outType

        afterArrowTypesBeforeRightestPrintsWithCommentsBefore :
            { endLocation : Gren.Syntax.Range.Location
            , reverse : List Print
            }
        afterArrowTypesBeforeRightestPrintsWithCommentsBefore =
            afterArrowTypes.beforeRightest
                |> List.foldl
                    (\afterArrowTypeNode soFar ->
                        let
                            (Gren.Syntax.Node.Node afterArrowTypeRange _) =
                                afterArrowTypeNode

                            print : Print
                            print =
                                typeParenthesizedIfFunction syntaxComments
                                    afterArrowTypeNode
                        in
                        { endLocation = afterArrowTypeRange.end
                        , reverse =
                            ((case
                                commentsInRange
                                    { start = soFar.endLocation, end = afterArrowTypeRange.start }
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
                    { endLocation = function.inType |> Gren.Syntax.Node.range |> .end
                    , reverse = []
                    }

        commentsBeforeRightestAfterArrowType : List String
        commentsBeforeRightestAfterArrowType =
            commentsInRange
                { start = afterArrowTypesBeforeRightestPrintsWithCommentsBefore.endLocation
                , end = afterArrowTypes.rightest |> Gren.Syntax.Node.range |> .start
                }
                syntaxComments

        commentsCollapsibleBeforeRightestAfterArrowType : { print : Print, lineSpread : Print.LineSpread }
        commentsCollapsibleBeforeRightestAfterArrowType =
            collapsibleComments commentsBeforeRightestAfterArrowType

        inTypePrint : Print
        inTypePrint =
            typeParenthesizedIfFunction syntaxComments function.inType

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
    List (Gren.Syntax.Node.Node String)
    -> Gren.Syntax.Node.Node Gren.Syntax.TypeAnnotation.TypeAnnotation
    -> Print
typeParenthesizedIfParenthesizedFunction syntaxComments typeNode =
    case typeNode |> Gren.Syntax.Node.value of
        Gren.Syntax.TypeAnnotation.Tupled [ inParens ] ->
            inParens |> typeParenthesizedIfFunction syntaxComments

        _ ->
            typeNotParenthesized syntaxComments typeNode


typeParenthesizedIfFunction :
    List (Gren.Syntax.Node.Node String)
    -> Gren.Syntax.Node.Node Gren.Syntax.TypeAnnotation.TypeAnnotation
    -> Print
typeParenthesizedIfFunction syntaxComments typeNode =
    case typeNode |> typeToFunction of
        Just _ ->
            typeParenthesized syntaxComments typeNode

        Nothing ->
            typeNotParenthesized syntaxComments typeNode


typeToFunction :
    Gren.Syntax.Node.Node Gren.Syntax.TypeAnnotation.TypeAnnotation
    ->
        Maybe
            { inType : Gren.Syntax.Node.Node Gren.Syntax.TypeAnnotation.TypeAnnotation
            , outType : Gren.Syntax.Node.Node Gren.Syntax.TypeAnnotation.TypeAnnotation
            }
typeToFunction typeNode =
    case typeNode |> typeToNotParenthesized |> Gren.Syntax.Node.value of
        Gren.Syntax.TypeAnnotation.FunctionTypeAnnotation inType outType ->
            Just { inType = inType, outType = outType }

        Gren.Syntax.TypeAnnotation.GenericType _ ->
            Nothing

        Gren.Syntax.TypeAnnotation.Typed _ _ ->
            Nothing

        Gren.Syntax.TypeAnnotation.Unit ->
            Nothing

        Gren.Syntax.TypeAnnotation.Tupled _ ->
            Nothing

        Gren.Syntax.TypeAnnotation.Record _ ->
            Nothing

        Gren.Syntax.TypeAnnotation.GenericRecord _ _ ->
            Nothing


{-| Remove as many parens as possible
-}
typeToNotParenthesized :
    Gren.Syntax.Node.Node Gren.Syntax.TypeAnnotation.TypeAnnotation
    -> Gren.Syntax.Node.Node Gren.Syntax.TypeAnnotation.TypeAnnotation
typeToNotParenthesized (Gren.Syntax.Node.Node typeRange syntaxType) =
    case syntaxType of
        Gren.Syntax.TypeAnnotation.GenericType name ->
            Gren.Syntax.Node.Node typeRange (Gren.Syntax.TypeAnnotation.GenericType name)

        Gren.Syntax.TypeAnnotation.Typed reference arguments ->
            Gren.Syntax.Node.Node typeRange (Gren.Syntax.TypeAnnotation.Typed reference arguments)

        Gren.Syntax.TypeAnnotation.Unit ->
            Gren.Syntax.Node.Node typeRange Gren.Syntax.TypeAnnotation.Unit

        Gren.Syntax.TypeAnnotation.Tupled parts ->
            case parts of
                [ inParens ] ->
                    typeToNotParenthesized inParens

                [] ->
                    -- should be handled by Unit
                    Gren.Syntax.Node.Node typeRange Gren.Syntax.TypeAnnotation.Unit

                [ part0, part1 ] ->
                    Gren.Syntax.Node.Node typeRange (Gren.Syntax.TypeAnnotation.Tupled [ part0, part1 ])

                [ part0, part1, part2 ] ->
                    Gren.Syntax.Node.Node typeRange (Gren.Syntax.TypeAnnotation.Tupled [ part0, part1, part2 ])

                part0 :: part1 :: part2 :: part3 :: part4Up ->
                    -- invalid syntax
                    Gren.Syntax.Node.Node typeRange (Gren.Syntax.TypeAnnotation.Tupled (part0 :: part1 :: part2 :: part3 :: part4Up))

        Gren.Syntax.TypeAnnotation.Record fields ->
            Gren.Syntax.Node.Node typeRange (Gren.Syntax.TypeAnnotation.Record fields)

        Gren.Syntax.TypeAnnotation.GenericRecord extendedRecordVariableName additionalFieldsNode ->
            Gren.Syntax.Node.Node typeRange (Gren.Syntax.TypeAnnotation.GenericRecord extendedRecordVariableName additionalFieldsNode)

        Gren.Syntax.TypeAnnotation.FunctionTypeAnnotation inType outType ->
            Gren.Syntax.Node.Node typeRange (Gren.Syntax.TypeAnnotation.FunctionTypeAnnotation inType outType)


typeFunctionExpand :
    Gren.Syntax.Node.Node Gren.Syntax.TypeAnnotation.TypeAnnotation
    ->
        { beforeRightest :
            List (Gren.Syntax.Node.Node Gren.Syntax.TypeAnnotation.TypeAnnotation)
        , rightest : Gren.Syntax.Node.Node Gren.Syntax.TypeAnnotation.TypeAnnotation
        }
typeFunctionExpand typeNode =
    case typeNode of
        Gren.Syntax.Node.Node _ (Gren.Syntax.TypeAnnotation.FunctionTypeAnnotation inType outType) ->
            let
                outTypeExpanded :
                    { beforeRightest : List (Gren.Syntax.Node.Node Gren.Syntax.TypeAnnotation.TypeAnnotation)
                    , rightest : Gren.Syntax.Node.Node Gren.Syntax.TypeAnnotation.TypeAnnotation
                    }
                outTypeExpanded =
                    typeFunctionExpand outType
            in
            { beforeRightest = inType :: outTypeExpanded.beforeRightest
            , rightest = outTypeExpanded.rightest
            }

        typeNodeNotFunction ->
            { beforeRightest = [], rightest = typeNodeNotFunction }


typeParenthesizedIfSpaceSeparated :
    List (Gren.Syntax.Node.Node String)
    -> Gren.Syntax.Node.Node Gren.Syntax.TypeAnnotation.TypeAnnotation
    -> Print
typeParenthesizedIfSpaceSeparated syntaxComments typeNode =
    if typeIsSpaceSeparated (typeNode |> Gren.Syntax.Node.value) then
        typeParenthesized syntaxComments typeNode

    else
        typeNotParenthesized syntaxComments typeNode


typeParenthesized :
    List (Gren.Syntax.Node.Node String)
    -> Gren.Syntax.Node.Node Gren.Syntax.TypeAnnotation.TypeAnnotation
    -> Print
typeParenthesized syntaxComments typeNode =
    parenthesized typeNotParenthesized
        { notParenthesized = typeNode |> typeToNotParenthesized
        , fullRange = typeNode |> Gren.Syntax.Node.range
        }
        syntaxComments


parenthesized :
    (List (Gren.Syntax.Node.Node String) -> Gren.Syntax.Node.Node a -> Print)
    ->
        { notParenthesized : Gren.Syntax.Node.Node a
        , fullRange : Gren.Syntax.Range.Range
        }
    -> List (Gren.Syntax.Node.Node String)
    -> Print
parenthesized printNotParenthesized syntax syntaxComments =
    let
        commentsBeforeInner : List String
        commentsBeforeInner =
            commentsInRange
                { start = syntax.fullRange.start
                , end = syntax.notParenthesized |> Gren.Syntax.Node.range |> .start
                }
                syntaxComments

        commentsAfterInner : List String
        commentsAfterInner =
            commentsInRange
                { start = syntax.notParenthesized |> Gren.Syntax.Node.range |> .end
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


typeIsSpaceSeparated : Gren.Syntax.TypeAnnotation.TypeAnnotation -> Bool
typeIsSpaceSeparated syntaxType =
    case syntaxType of
        Gren.Syntax.TypeAnnotation.GenericType _ ->
            False

        Gren.Syntax.TypeAnnotation.Typed _ arguments ->
            case arguments of
                [] ->
                    False

                _ :: _ ->
                    True

        Gren.Syntax.TypeAnnotation.Unit ->
            False

        Gren.Syntax.TypeAnnotation.Tupled parts ->
            case parts of
                [] ->
                    -- should be handled by Unit
                    False

                [ Gren.Syntax.Node.Node _ inParens ] ->
                    typeIsSpaceSeparated inParens

                [ _, _ ] ->
                    False

                [ _, _, _ ] ->
                    False

                _ :: _ :: _ :: _ :: _ ->
                    False

        Gren.Syntax.TypeAnnotation.Record _ ->
            False

        Gren.Syntax.TypeAnnotation.GenericRecord _ _ ->
            False

        Gren.Syntax.TypeAnnotation.FunctionTypeAnnotation _ _ ->
            True


{-| Print an [`Gren.Syntax.TypeAnnotation.TypeAnnotation`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-TypeAnnotation#TypeAnnotation)
-}
typeNotParenthesized :
    List (Gren.Syntax.Node.Node String)
    -> Gren.Syntax.Node.Node Gren.Syntax.TypeAnnotation.TypeAnnotation
    -> Print
typeNotParenthesized syntaxComments (Gren.Syntax.Node.Node fullRange syntaxType) =
    -- IGNORE TCO
    case syntaxType of
        Gren.Syntax.TypeAnnotation.Unit ->
            printExactlyParensOpeningParensClosed

        Gren.Syntax.TypeAnnotation.GenericType name ->
            Print.exactly name

        Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node _ ( referenceQualification, referenceUnqualified )) arguments ->
            construct
                { printArgumentParenthesizedIfSpaceSeparated =
                    typeParenthesizedIfSpaceSeparated
                , lineSpreadMinimum = lineSpreadInRange fullRange
                }
                syntaxComments
                { fullRange = fullRange
                , start =
                    qualifiedReference
                        { qualification = referenceQualification
                        , unqualified = referenceUnqualified
                        }
                , arguments = arguments
                }

        Gren.Syntax.TypeAnnotation.Tupled parts ->
            case parts of
                [] ->
                    -- should be handled by Unit
                    printExactlyParensOpeningParensClosed

                [ inParens ] ->
                    let
                        commentsBeforeInParens : List String
                        commentsBeforeInParens =
                            commentsInRange { start = fullRange.start, end = inParens |> Gren.Syntax.Node.range |> .start } syntaxComments

                        commentsAfterInParens : List String
                        commentsAfterInParens =
                            commentsInRange { start = inParens |> Gren.Syntax.Node.range |> .end, end = fullRange.end } syntaxComments
                    in
                    case ( commentsBeforeInParens, commentsAfterInParens ) of
                        ( [], [] ) ->
                            typeNotParenthesized syntaxComments inParens

                        _ ->
                            parenthesized typeNotParenthesized
                                { notParenthesized = inParens |> typeToNotParenthesized
                                , fullRange = fullRange
                                }
                                syntaxComments

                [ part0, part1 ] ->
                    tuple
                        { printPartNotParenthesized = typeNotParenthesized
                        , lineSpreadMinimum = lineSpreadInRange fullRange
                        }
                        syntaxComments
                        { part0 = part0
                        , part1 = part1
                        , fullRange = fullRange
                        }

                [ part0, part1, part2 ] ->
                    triple
                        { printPartNotParenthesized = typeNotParenthesized
                        , lineSpreadMinimum = lineSpreadInRange fullRange
                        }
                        syntaxComments
                        { part0 = part0
                        , part1 = part1
                        , part2 = part2
                        , fullRange = fullRange
                        }

                part0 :: part1 :: part2 :: part3 :: part4Up ->
                    -- invalid syntax
                    invalidNTuple typeNotParenthesized
                        syntaxComments
                        { fullRange = fullRange, part0 = part0, part1 = part1, part2 = part2, part3 = part3, part4Up = part4Up }

        Gren.Syntax.TypeAnnotation.Record fields ->
            recordLiteral
                { printValueNotParenthesized = typeNotParenthesized
                , nameValueSeparator = ":"
                }
                syntaxComments
                { fullRange = fullRange, fields = fields }

        Gren.Syntax.TypeAnnotation.GenericRecord recordVariable (Gren.Syntax.Node.Node _ fields) ->
            typeRecordExtension syntaxComments
                { fullRange = fullRange
                , recordVariable = recordVariable
                , fields = fields
                }

        Gren.Syntax.TypeAnnotation.FunctionTypeAnnotation inType outType ->
            { fullRange = fullRange, inType = inType, outType = outType }
                |> typeFunctionNotParenthesized syntaxComments


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
    case syntaxDeclarations of
        [] ->
            -- invalid syntax
            Print.empty

        (Gren.Syntax.Node.Node declaration0Range declaration0) :: declarations1Up ->
            declaration
                { comments = context.comments
                , portDocumentationComment =
                    case declaration0 of
                        GrenSyntax.PortDeclaration _ ->
                            firstCommentInRange { start = context.previousEnd, end = declaration0Range.start } context.portDocumentationComments

                        GrenSyntax.FunctionDeclaration _ ->
                            Nothing

                        GrenSyntax.AliasDeclaration _ ->
                            Nothing

                        GrenSyntax.CustomTypeDeclaration _ ->
                            Nothing

                        GrenSyntax.InfixDeclaration _ ->
                            Nothing
                }
                declaration0
                |> Print.followedBy
                    (declarations1Up
                        |> List.foldl
                            (\(Gren.Syntax.Node.Node declarationRange syntaxDeclaration) soFar ->
                                let
                                    maybeDeclarationPortDocumentationComment : Maybe (Gren.Syntax.Node.Node String)
                                    maybeDeclarationPortDocumentationComment =
                                        case syntaxDeclaration of
                                            GrenSyntax.PortDeclaration _ ->
                                                firstCommentInRange { start = soFar.previousRange.end, end = declarationRange.start } context.portDocumentationComments

                                            GrenSyntax.FunctionDeclaration _ ->
                                                Nothing

                                            GrenSyntax.AliasDeclaration _ ->
                                                Nothing

                                            GrenSyntax.CustomTypeDeclaration _ ->
                                                Nothing

                                            GrenSyntax.InfixDeclaration _ ->
                                                Nothing
                                in
                                { print =
                                    soFar.print
                                        |> Print.followedBy
                                            (case commentsInRange { start = soFar.previousRange.end, end = declarationRange.start } context.comments of
                                                comment0 :: comment1Up ->
                                                    printLinebreakLinebreakLinebreak
                                                        |> Print.followedBy
                                                            (moduleLevelCommentsBeforeDeclaration
                                                                { comment0 = comment0, comment1Up = comment1Up }
                                                            )
                                                        |> Print.followedBy
                                                            (declaration
                                                                { comments = commentNodesInRange declarationRange context.comments
                                                                , portDocumentationComment = maybeDeclarationPortDocumentationComment
                                                                }
                                                                syntaxDeclaration
                                                            )

                                                [] ->
                                                    linebreaksFollowedByDeclaration
                                                        { comments = commentNodesInRange declarationRange context.comments
                                                        , portDocumentationComment = maybeDeclarationPortDocumentationComment
                                                        }
                                                        syntaxDeclaration
                                            )
                                , previousRange = declarationRange
                                }
                            )
                            { print = Print.empty
                            , previousRange = declaration0Range
                            }
                        |> .print
                    )


firstCommentInRange :
    Gren.Syntax.Range.Range
    -> List (Gren.Syntax.Node.Node String)
    -> Maybe (Gren.Syntax.Node.Node String)
firstCommentInRange range sortedComments =
    case sortedComments of
        [] ->
            Nothing

        (Gren.Syntax.Node.Node headCommentRange headComment) :: tailComments ->
            case locationCompareFast headCommentRange.start range.start of
                LT ->
                    firstCommentInRange range tailComments

                EQ ->
                    Just (Gren.Syntax.Node.Node headCommentRange headComment)

                GT ->
                    case locationCompareFast headCommentRange.end range.end of
                        GT ->
                            Nothing

                        LT ->
                            Just (Gren.Syntax.Node.Node headCommentRange headComment)

                        EQ ->
                            Just (Gren.Syntax.Node.Node headCommentRange headComment)


locationCompareFast : Gren.Syntax.Range.Location -> Gren.Syntax.Range.Location -> Basics.Order
locationCompareFast left right =
    if left.row - right.row < 0 then
        LT

    else if left.row - right.row > 0 then
        GT

    else
        Basics.compare left.column right.column


linebreaksFollowedByDeclaration :
    { portDocumentationComment : Maybe (Gren.Syntax.Node.Node String)
    , comments : List (Gren.Syntax.Node.Node String)
    }
    -> GrenSyntax.Declaration
    -> Print
linebreaksFollowedByDeclaration syntaxComments syntaxDeclaration =
    case syntaxDeclaration of
        GrenSyntax.FunctionDeclaration syntaxExpressionDeclaration ->
            printLinebreakLinebreakLinebreak
                |> Print.followedBy (declarationExpression syntaxComments.comments syntaxExpressionDeclaration)

        GrenSyntax.AliasDeclaration syntaxTypeAliasDeclaration ->
            printLinebreakLinebreakLinebreak
                |> Print.followedBy (declarationTypeAlias syntaxComments.comments syntaxTypeAliasDeclaration)

        GrenSyntax.CustomTypeDeclaration syntaxChoiceTypeDeclaration ->
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


declarationDestructuring :
    List (Gren.Syntax.Node.Node String)
    -> Gren.Syntax.Node.Node Gren.Syntax.Pattern.Pattern
    -> Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
    -> Print
declarationDestructuring syntaxComments destructuringPattern destructuringExpression =
    -- invalid syntax
    patternParenthesizedIfSpaceSeparated syntaxComments destructuringPattern
        |> Print.followedBy (Print.exactly " =")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy (expressionNotParenthesized [] destructuringExpression)
                )
            )


{-| Print an [`GrenSyntax.Declaration`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Declaration#Declaration)
-}
declaration :
    { portDocumentationComment : Maybe (Gren.Syntax.Node.Node String)
    , comments : List (Gren.Syntax.Node.Node String)
    }
    -> GrenSyntax.Declaration
    -> Print
declaration syntaxComments syntaxDeclaration =
    case syntaxDeclaration of
        GrenSyntax.FunctionDeclaration syntaxExpressionDeclaration ->
            declarationExpression syntaxComments.comments syntaxExpressionDeclaration

        GrenSyntax.AliasDeclaration syntaxTypeAliasDeclaration ->
            declarationTypeAlias syntaxComments.comments syntaxTypeAliasDeclaration

        GrenSyntax.CustomTypeDeclaration syntaxChoiceTypeDeclaration ->
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
    List (Gren.Syntax.Node.Node String)
    ->
        { name : Gren.Syntax.Node.Node String
        , typeAnnotation : Gren.Syntax.Node.Node Gren.Syntax.TypeAnnotation.TypeAnnotation
        }
    -> Print
declarationSignature syntaxComments signature =
    let
        typePrint : Print
        typePrint =
            typeNotParenthesized syntaxComments signature.typeAnnotation

        rangeBetweenNameAndType : Gren.Syntax.Range.Range
        rangeBetweenNameAndType =
            { start = signature.name |> Gren.Syntax.Node.range |> .end
            , end = signature.typeAnnotation |> Gren.Syntax.Node.range |> .start
            }
    in
    Print.exactly
        ((signature.name |> Gren.Syntax.Node.value)
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
    { documentationComment : Maybe (Gren.Syntax.Node.Node String)
    , comments : List (Gren.Syntax.Node.Node String)
    }
    ->
        { name : Gren.Syntax.Node.Node String
        , typeAnnotation : Gren.Syntax.Node.Node Gren.Syntax.TypeAnnotation.TypeAnnotation
        }
    -> Print
declarationPort syntaxComments signature =
    (case syntaxComments.documentationComment of
        Nothing ->
            printExactlyPortSpace

        Just (Gren.Syntax.Node.Node documentationRange documentation) ->
            Print.exactly documentation
                |> Print.followedBy Print.linebreak
                |> Print.followedBy
                    (commentsBetweenDocumentationAndDeclaration
                        (commentsInRange
                            { start = documentationRange.start
                            , end =
                                signature.name
                                    |> Gren.Syntax.Node.range
                                    |> .start
                            }
                            syntaxComments.comments
                        )
                    )
                |> Print.followedBy printExactlyPortSpace
    )
        |> Print.followedBy (declarationSignature syntaxComments.comments signature)


{-| Print an [`Gren.Syntax.TypeAlias.TypeAlias`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-TypeAlias#TypeAlias) declaration
-}
declarationTypeAlias :
    List (Gren.Syntax.Node.Node String)
    -> Gren.Syntax.TypeAlias.TypeAlias
    -> Print
declarationTypeAlias syntaxComments syntaxTypeAliasDeclaration =
    let
        parameterPrintsWithCommentsBeforeReverse : List Print
        parameterPrintsWithCommentsBeforeReverse =
            syntaxTypeAliasDeclaration.generics
                |> List.foldl
                    (\parameterName soFar ->
                        let
                            parameterPrintedRange : Gren.Syntax.Range.Range
                            parameterPrintedRange =
                                parameterName |> Gren.Syntax.Node.range

                            parameterNamePrint : Print
                            parameterNamePrint =
                                Print.exactly (parameterName |> Gren.Syntax.Node.value)
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
                            |> Gren.Syntax.Node.range
                            |> .end
                    }
                |> .reverse

        parametersLineSpread : Print.LineSpread
        parametersLineSpread =
            parameterPrintsWithCommentsBeforeReverse |> Print.lineSpreadListMapAndCombine Print.lineSpread

        rangeBetweenParametersAndType : Gren.Syntax.Range.Range
        rangeBetweenParametersAndType =
            case syntaxTypeAliasDeclaration.generics of
                [] ->
                    { start =
                        syntaxTypeAliasDeclaration.name
                            |> Gren.Syntax.Node.range
                            |> .end
                    , end = syntaxTypeAliasDeclaration.typeAnnotation |> Gren.Syntax.Node.range |> .start
                    }

                parameter0 :: parameter1Up ->
                    { start =
                        listFilledLast parameter0 parameter1Up
                            |> Gren.Syntax.Node.range
                            |> .end
                    , end = syntaxTypeAliasDeclaration.typeAnnotation |> Gren.Syntax.Node.range |> .start
                    }

        aliasedTypePrint : Print
        aliasedTypePrint =
            typeNotParenthesized syntaxComments
                syntaxTypeAliasDeclaration.typeAnnotation
    in
    (case syntaxTypeAliasDeclaration.documentation of
        Nothing ->
            printExactlyTypeSpaceAlias

        Just (Gren.Syntax.Node.Node documentationRange documentation) ->
            Print.exactly documentation
                |> Print.followedBy Print.linebreak
                |> Print.followedBy
                    (commentsBetweenDocumentationAndDeclaration
                        (commentsInRange
                            { start = documentationRange.start
                            , end =
                                syntaxTypeAliasDeclaration.name
                                    |> Gren.Syntax.Node.range
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
                        (Print.exactly (syntaxTypeAliasDeclaration.name |> Gren.Syntax.Node.value))
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


{-| Print an [`Gren.Syntax.Type.Type`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Type#Type) declaration
-}
declarationChoiceType :
    List (Gren.Syntax.Node.Node String)
    -> Gren.Syntax.Type.Type
    -> Print
declarationChoiceType syntaxComments syntaxChoiceTypeDeclaration =
    let
        parameterPrints :
            { endLocation : Gren.Syntax.Range.Location
            , reverse : List Print
            }
        parameterPrints =
            syntaxChoiceTypeDeclaration.generics
                |> List.foldl
                    (\parameterName soFar ->
                        let
                            parameterPrintedRange : Gren.Syntax.Range.Range
                            parameterPrintedRange =
                                parameterName |> Gren.Syntax.Node.range

                            parameterNamePrint : Print
                            parameterNamePrint =
                                Print.exactly (parameterName |> Gren.Syntax.Node.value)
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
                            |> Gren.Syntax.Node.range
                            |> .end
                    }

        parametersLineSpread : Print.LineSpread
        parametersLineSpread =
            parameterPrints.reverse |> Print.lineSpreadListMapAndCombine Print.lineSpread

        variantPrintsWithCommentsBeforeReverse : List Print
        variantPrintsWithCommentsBeforeReverse =
            syntaxChoiceTypeDeclaration.constructors
                |> List.foldl
                    (\(Gren.Syntax.Node.Node variantRange variant) soFar ->
                        let
                            variantPrint : Print
                            variantPrint =
                                construct
                                    { printArgumentParenthesizedIfSpaceSeparated =
                                        typeParenthesizedIfSpaceSeparated
                                    , lineSpreadMinimum = Print.SingleLine
                                    }
                                    syntaxComments
                                    { start = variant.name |> Gren.Syntax.Node.value
                                    , fullRange = variantRange
                                    , arguments = variant.arguments
                                    }

                            commentsVariantPrint : Print
                            commentsVariantPrint =
                                case commentsInRange { start = soFar.endLocation, end = variant.name |> Gren.Syntax.Node.range |> .start } syntaxComments of
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
                        , endLocation = variantRange.end
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

        Just (Gren.Syntax.Node.Node documentationRange documentation) ->
            Print.exactly documentation
                |> Print.followedBy Print.linebreak
                |> Print.followedBy
                    (commentsBetweenDocumentationAndDeclaration
                        (commentsInRange
                            { start = documentationRange.start
                            , end =
                                syntaxChoiceTypeDeclaration.name
                                    |> Gren.Syntax.Node.range
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
                            (syntaxChoiceTypeDeclaration.name |> Gren.Syntax.Node.value)
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


{-| Print an [`Gren.Syntax.Infix.Infix`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Infix#Infix) declaration
-}
declarationInfix : Gren.Syntax.Infix.Infix -> Print
declarationInfix syntaxInfixDeclaration =
    Print.exactly
        ("infix "
            ++ infixDirection (syntaxInfixDeclaration.direction |> Gren.Syntax.Node.value)
            ++ " "
            ++ String.fromInt (syntaxInfixDeclaration.precedence |> Gren.Syntax.Node.value)
            ++ " ("
            ++ (syntaxInfixDeclaration.operator |> Gren.Syntax.Node.value)
            ++ ") = "
            ++ (syntaxInfixDeclaration.function |> Gren.Syntax.Node.value)
        )


infixDirection : Gren.Syntax.Infix.InfixDirection -> String
infixDirection syntaxInfixDirection =
    case syntaxInfixDirection of
        Gren.Syntax.Infix.Left ->
            "left "

        Gren.Syntax.Infix.Right ->
            "right"

        Gren.Syntax.Infix.Non ->
            "non  "


declarationExpressionImplementation :
    List (Gren.Syntax.Node.Node String)
    -> Gren.Syntax.Expression.FunctionImplementation
    -> Print
declarationExpressionImplementation syntaxComments implementation =
    let
        parameterPrintsWithCommentsBefore :
            { endLocation : Gren.Syntax.Range.Location
            , reverse : List Print
            }
        parameterPrintsWithCommentsBefore =
            implementation.arguments
                |> List.foldl
                    (\parameterPattern soFar ->
                        let
                            parameterRange : Gren.Syntax.Range.Range
                            parameterRange =
                                parameterPattern |> Gren.Syntax.Node.range

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
                            |> Gren.Syntax.Node.range
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
                        |> Gren.Syntax.Node.range
                        |> .start
                }
                syntaxComments

        expressionPrint : Print
        expressionPrint =
            expressionNotParenthesized syntaxComments
                implementation.expression
    in
    Print.exactly (implementation.name |> Gren.Syntax.Node.value)
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


{-| Print an [`Gren.Syntax.Expression.Function`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Expression#Function) declaration
-}
declarationExpression :
    List (Gren.Syntax.Node.Node String)
    -> Gren.Syntax.Expression.Function
    -> Print
declarationExpression syntaxComments syntaxExpressionDeclaration =
    let
        implementationPrint : Print
        implementationPrint =
            declarationExpressionImplementation
                syntaxComments
                (syntaxExpressionDeclaration.declaration |> Gren.Syntax.Node.value)

        withoutDocumentationPrint : Print
        withoutDocumentationPrint =
            case syntaxExpressionDeclaration.signature of
                Nothing ->
                    implementationPrint

                Just (Gren.Syntax.Node.Node signatureRange signature) ->
                    let
                        commentsBetweenSignatureAndImplementationName : List String
                        commentsBetweenSignatureAndImplementationName =
                            commentsInRange
                                { start = signatureRange.end
                                , end =
                                    syntaxExpressionDeclaration.declaration
                                        |> Gren.Syntax.Node.range
                                        |> .start
                                }
                                syntaxComments
                    in
                    declarationSignature syntaxComments signature
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

        Just (Gren.Syntax.Node.Node documentationRange documentation) ->
            Print.exactly documentation
                |> Print.followedBy Print.linebreak
                |> Print.followedBy
                    (commentsBetweenDocumentationAndDeclaration
                        (commentsInRange
                            { start = documentationRange.start
                            , end =
                                case syntaxExpressionDeclaration.signature of
                                    Nothing ->
                                        syntaxExpressionDeclaration.declaration |> Gren.Syntax.Node.range |> .start

                                    Just (Gren.Syntax.Node.Node signatureRange _) ->
                                        signatureRange.start
                            }
                            syntaxComments
                        )
                    )
                |> Print.followedBy withoutDocumentationPrint


expressionParenthesized :
    List (Gren.Syntax.Node.Node String)
    -> Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
    -> Print
expressionParenthesized syntaxComments expressionNode =
    parenthesized expressionNotParenthesized
        { notParenthesized = expressionNode |> expressionToNotParenthesized
        , fullRange = expressionNode |> Gren.Syntax.Node.range
        }
        syntaxComments


expressionIsSpaceSeparated : Gren.Syntax.Expression.Expression -> Bool
expressionIsSpaceSeparated syntaxExpression =
    case syntaxExpression of
        Gren.Syntax.Expression.UnitExpr ->
            False

        Gren.Syntax.Expression.Application application ->
            case application of
                [] ->
                    -- invalid syntax
                    False

                [ Gren.Syntax.Node.Node _ notActuallyApplied ] ->
                    -- invalid syntax
                    expressionIsSpaceSeparated notActuallyApplied

                _ :: _ :: _ ->
                    True

        Gren.Syntax.Expression.OperatorApplication _ _ _ ->
            True

        Gren.Syntax.Expression.FunctionOrValue _ _ ->
            False

        Gren.Syntax.Expression.IfBlock _ _ _ ->
            True

        Gren.Syntax.Expression.PrefixOperator _ ->
            False

        Gren.Syntax.Expression.Operator _ ->
            -- invalid syntax
            False

        Gren.Syntax.Expression.Integer _ ->
            False

        Gren.Syntax.Expression.Hex _ ->
            False

        Gren.Syntax.Expression.Floatable _ ->
            False

        Gren.Syntax.Expression.Negation _ ->
            False

        Gren.Syntax.Expression.Literal _ ->
            False

        Gren.Syntax.Expression.CharLiteral _ ->
            False

        Gren.Syntax.Expression.TupledExpression parts ->
            case parts of
                [] ->
                    -- should be handled by UnitExpr
                    False

                [ Gren.Syntax.Node.Node _ inParens ] ->
                    -- should be handled by ParenthesizedExpression
                    expressionIsSpaceSeparated inParens

                [ _, _ ] ->
                    False

                [ _, _, _ ] ->
                    False

                _ :: _ :: _ :: _ :: _ ->
                    -- invalid syntax
                    False

        Gren.Syntax.Expression.ParenthesizedExpression (Gren.Syntax.Node.Node _ inParens) ->
            expressionIsSpaceSeparated inParens

        Gren.Syntax.Expression.LetExpression _ ->
            True

        Gren.Syntax.Expression.CaseExpression _ ->
            True

        Gren.Syntax.Expression.LambdaExpression _ ->
            True

        Gren.Syntax.Expression.RecordExpr _ ->
            False

        Gren.Syntax.Expression.ListExpr _ ->
            False

        Gren.Syntax.Expression.RecordAccess _ _ ->
            False

        Gren.Syntax.Expression.RecordAccessFunction _ ->
            False

        Gren.Syntax.Expression.RecordUpdateExpression _ _ ->
            False

        Gren.Syntax.Expression.GLSLExpression _ ->
            False


expressionParenthesizedIfSpaceSeparated :
    List (Gren.Syntax.Node.Node String)
    -> Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
    -> Print
expressionParenthesizedIfSpaceSeparated syntaxComments expressionNode =
    if expressionIsSpaceSeparated (expressionNode |> Gren.Syntax.Node.value) then
        expressionParenthesized syntaxComments expressionNode

    else
        expressionNotParenthesized syntaxComments expressionNode


{-| Print an [`Gren.Syntax.Expression.Expression`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Expression#Expression)
-}
expressionNotParenthesized :
    List (Gren.Syntax.Node.Node String)
    -> Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
    -> Print
expressionNotParenthesized syntaxComments (Gren.Syntax.Node.Node fullRange syntaxExpression) =
    -- IGNORE TCO
    case syntaxExpression of
        Gren.Syntax.Expression.UnitExpr ->
            printExactlyParensOpeningParensClosed

        Gren.Syntax.Expression.Application application ->
            case application of
                [] ->
                    -- invalid syntax
                    Print.empty

                [ notAppliedAfterAll ] ->
                    -- invalid syntax
                    expressionNotParenthesized syntaxComments notAppliedAfterAll

                applied :: argument0 :: argument1Up ->
                    expressionCall syntaxComments
                        { fullRange = fullRange
                        , applied = applied
                        , argument0 = argument0
                        , argument1Up = argument1Up
                        }

        Gren.Syntax.Expression.OperatorApplication operator left right ->
            expressionOperation syntaxComments
                { fullRange = fullRange
                , operator = operator
                , left = left
                , right = right
                }

        Gren.Syntax.Expression.FunctionOrValue qualification unqualified ->
            Print.exactly
                (qualifiedReference { qualification = qualification, unqualified = unqualified })

        Gren.Syntax.Expression.IfBlock condition onTrue onFalse ->
            expressionIfThenElse syntaxComments
                { fullRange = fullRange
                , condition = condition
                , conditionLineSpreadMinimum = Print.SingleLine
                , onTrue = onTrue
                , onFalse = onFalse
                }

        Gren.Syntax.Expression.PrefixOperator operatorSymbol ->
            Print.exactly ("(" ++ operatorSymbol ++ ")")

        Gren.Syntax.Expression.Operator operatorSymbol ->
            -- invalid syntax
            Print.exactly operatorSymbol

        Gren.Syntax.Expression.Integer int ->
            Print.exactly (intLiteral int)

        Gren.Syntax.Expression.Hex int ->
            Print.exactly (hexLiteral int)

        Gren.Syntax.Expression.Floatable float ->
            Print.exactly (floatLiteral float)

        Gren.Syntax.Expression.Negation negated ->
            printExpressionNegation syntaxComments negated

        Gren.Syntax.Expression.Literal string ->
            stringLiteral (Gren.Syntax.Node.Node fullRange string)

        Gren.Syntax.Expression.CharLiteral char ->
            Print.exactly (charLiteral char)

        Gren.Syntax.Expression.TupledExpression parts ->
            case parts of
                [] ->
                    -- should be handled by Unit
                    printExactlyParensOpeningParensClosed

                [ inParens ] ->
                    -- should be handled by ParenthesizedExpression
                    let
                        commentsBeforeInParens : List String
                        commentsBeforeInParens =
                            commentsInRange { start = fullRange.start, end = inParens |> Gren.Syntax.Node.range |> .start } syntaxComments

                        commentsAfterInParens : List String
                        commentsAfterInParens =
                            commentsInRange { start = inParens |> Gren.Syntax.Node.range |> .end, end = fullRange.end } syntaxComments
                    in
                    case ( commentsBeforeInParens, commentsAfterInParens ) of
                        ( [], [] ) ->
                            expressionNotParenthesized syntaxComments inParens

                        _ ->
                            parenthesized expressionNotParenthesized
                                { notParenthesized = inParens |> expressionToNotParenthesized
                                , fullRange = fullRange
                                }
                                syntaxComments

                [ part0, part1 ] ->
                    tuple
                        { printPartNotParenthesized = expressionNotParenthesized
                        , lineSpreadMinimum = lineSpreadInRange fullRange
                        }
                        syntaxComments
                        { fullRange = fullRange, part0 = part0, part1 = part1 }

                [ part0, part1, part2 ] ->
                    triple
                        { printPartNotParenthesized = expressionNotParenthesized
                        , lineSpreadMinimum = lineSpreadInRange fullRange
                        }
                        syntaxComments
                        { fullRange = fullRange
                        , part0 = part0
                        , part1 = part1
                        , part2 = part2
                        }

                part0 :: part1 :: part2 :: part3 :: part4Up ->
                    -- invalid syntax
                    invalidNTuple expressionNotParenthesized
                        syntaxComments
                        { fullRange = fullRange, part0 = part0, part1 = part1, part2 = part2, part3 = part3, part4Up = part4Up }

        Gren.Syntax.Expression.ParenthesizedExpression inParens ->
            let
                commentsBeforeInParens : List String
                commentsBeforeInParens =
                    commentsInRange { start = fullRange.start, end = inParens |> Gren.Syntax.Node.range |> .start } syntaxComments

                commentsAfterInParens : List String
                commentsAfterInParens =
                    commentsInRange { start = inParens |> Gren.Syntax.Node.range |> .end, end = fullRange.end } syntaxComments
            in
            case ( commentsBeforeInParens, commentsAfterInParens ) of
                ( [], [] ) ->
                    expressionNotParenthesized syntaxComments inParens

                _ ->
                    parenthesized expressionNotParenthesized
                        { notParenthesized = inParens |> expressionToNotParenthesized
                        , fullRange = fullRange
                        }
                        syntaxComments

        Gren.Syntax.Expression.LetExpression syntaxLetIn ->
            case syntaxLetIn.declarations of
                [] ->
                    -- invalid syntax
                    expressionNotParenthesized syntaxComments syntaxLetIn.expression

                letDeclaration0 :: letDeclaration1Up ->
                    expressionLetIn syntaxComments
                        { fullRange = fullRange
                        , letDeclaration0 = letDeclaration0
                        , letDeclaration1Up = letDeclaration1Up
                        , result = syntaxLetIn.expression
                        }

        Gren.Syntax.Expression.CaseExpression syntaxCaseOf ->
            expressionCaseOf syntaxComments
                { fullRange = fullRange
                , expression = syntaxCaseOf.expression
                , cases = syntaxCaseOf.cases
                }

        Gren.Syntax.Expression.LambdaExpression syntaxLambda ->
            expressionLambda syntaxComments (Gren.Syntax.Node.Node fullRange syntaxLambda)

        Gren.Syntax.Expression.RecordExpr fields ->
            recordLiteral
                { printValueNotParenthesized = expressionNotParenthesized
                , nameValueSeparator = "="
                }
                syntaxComments
                { fullRange = fullRange, fields = fields }

        Gren.Syntax.Expression.ListExpr elements ->
            expressionList syntaxComments { fullRange = fullRange, elements = elements }

        Gren.Syntax.Expression.RecordAccess syntaxRecord (Gren.Syntax.Node.Node _ accessedFieldName) ->
            expressionParenthesizedIfSpaceSeparated syntaxComments syntaxRecord
                |> Print.followedBy (Print.exactly ("." ++ accessedFieldName))

        Gren.Syntax.Expression.RecordAccessFunction dotFieldName ->
            Print.exactly ("." ++ (dotFieldName |> String.replace "." ""))

        Gren.Syntax.Expression.RecordUpdateExpression recordVariableNode fields ->
            expressionRecordUpdate syntaxComments
                { fullRange = fullRange
                , recordVariable = recordVariableNode
                , fields = fields
                }

        Gren.Syntax.Expression.GLSLExpression glsl ->
            expressionGlsl glsl


printExpressionNegation :
    List (Gren.Syntax.Node.Node String)
    -> Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
    -> Print
printExpressionNegation syntaxComments negated =
    if negated |> expressionIsBase10Zero then
        printExactlyZero

    else if negated |> expressionIsBase16Zero then
        printExactlyZeroXZeroZero

    else
        case negated |> expressionToNotParenthesized of
            Gren.Syntax.Node.Node doublyNegatedRange (Gren.Syntax.Expression.Negation doublyNegated) ->
                printExactlyMinus
                    |> Print.followedBy
                        (expressionParenthesized syntaxComments
                            (Gren.Syntax.Node.Node doublyNegatedRange (Gren.Syntax.Expression.Negation doublyNegated))
                        )

            negatedNotNegationOrIntegerZero ->
                printExactlyMinus
                    |> Print.followedBy
                        (expressionParenthesizedIfSpaceSeparated syntaxComments
                            negatedNotNegationOrIntegerZero
                        )


expressionIsBase10Zero : Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression -> Bool
expressionIsBase10Zero expression =
    case expression |> expressionToNotParenthesized |> Gren.Syntax.Node.value of
        Gren.Syntax.Expression.Integer 0 ->
            True

        Gren.Syntax.Expression.Negation doublyNegated ->
            expressionIsBase10Zero doublyNegated

        _ ->
            False


expressionIsBase16Zero : Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression -> Bool
expressionIsBase16Zero expression =
    case expression |> expressionToNotParenthesized |> Gren.Syntax.Node.value of
        Gren.Syntax.Expression.Hex 0 ->
            True

        Gren.Syntax.Expression.Negation doublyNegated ->
            expressionIsBase16Zero doublyNegated

        _ ->
            False


expressionGlsl : String -> Print
expressionGlsl glslContent =
    Print.exactly "[glsl|"
        |> Print.followedBy
            (glslContent
                |> String.lines
                |> Print.listMapAndIntersperseAndFlatten
                    Print.exactly
                    Print.linebreak
            )
        |> Print.followedBy (Print.exactly "|]")


floatLiteral : Float -> String
floatLiteral float =
    if (float |> Basics.truncate |> Basics.toFloat) == float then
        String.fromFloat float ++ ".0"

    else
        String.fromFloat float


expressionCall :
    List (Gren.Syntax.Node.Node String)
    ->
        { fullRange : Gren.Syntax.Range.Range
        , applied : Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
        , argument0 : Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
        , argument1Up : List (Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression)
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
                { start = syntaxCall.applied |> Gren.Syntax.Node.range |> .end
                , end =
                    syntaxCall.argument0
                        |> Gren.Syntax.Node.range
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
                                    , end = argument |> Gren.Syntax.Node.range |> .start
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
                        , endLocation = argument |> Gren.Syntax.Node.range |> .end
                        }
                    )
                    { reverse = []
                    , endLocation =
                        syntaxCall.argument0
                            |> Gren.Syntax.Node.range
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
    List (Gren.Syntax.Node.Node String)
    ->
        { fullRange : Gren.Syntax.Range.Range
        , left : Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
        , operator : String
        , right : Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
        }
    -> Print
expressionOperation syntaxComments syntaxOperation =
    let
        operationExpanded :
            { leftest : Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
            , beforeRightestOperatorExpressionChain :
                List
                    { operator : String
                    , expression : Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
                    }
            , rightestOperator : String
            , rightestExpression : Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
            }
        operationExpanded =
            expressionOperationExpand syntaxOperation.left
                syntaxOperation.operator
                syntaxOperation.right

        beforeRightestPrintsAndComments :
            { reverse :
                List
                    { operator : String
                    , expression : Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
                    , maybeCommentsBeforeExpression :
                        Maybe { print : Print, lineSpread : Print.LineSpread }
                    }
            , endLocation : Gren.Syntax.Range.Location
            }
        beforeRightestPrintsAndComments =
            operationExpanded.beforeRightestOperatorExpressionChain
                |> List.foldl
                    (\operatorAndExpressionBeforeRightest soFar ->
                        let
                            expressionRange : Gren.Syntax.Range.Range
                            expressionRange =
                                operatorAndExpressionBeforeRightest.expression
                                    |> Gren.Syntax.Node.range

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
                        operationExpanded.leftest |> Gren.Syntax.Node.range |> .end
                    , reverse = []
                    }

        commentsBeforeRightestExpression : List String
        commentsBeforeRightestExpression =
            commentsInRange
                { start = beforeRightestPrintsAndComments.endLocation
                , end =
                    operationExpanded.rightestExpression
                        |> Gren.Syntax.Node.range
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
                    , expression : Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
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
    Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
    -> String
    -> Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
    ->
        { leftest : Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
        , beforeRightestOperatorExpressionChain :
            List
                { operator : String
                , expression : Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
                }
        , rightestOperator : String
        , rightestExpression : Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
        }
expressionOperationExpand left operator right =
    let
        rightExpanded :
            { beforeRightestOperatorExpressionChain :
                List { operator : String, expression : Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression }
            , rightestOperator : String
            , rightestExpression : Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
            }
        rightExpanded =
            case right of
                Gren.Syntax.Node.Node _ (Gren.Syntax.Expression.OperatorApplication rightOperator rightLeft rightRight) ->
                    let
                        rightOperationExpanded :
                            { leftest : Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
                            , beforeRightestOperatorExpressionChain :
                                List
                                    { operator : String
                                    , expression : Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
                                    }
                            , rightestOperator : String
                            , rightestExpression : Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
                            }
                        rightOperationExpanded =
                            expressionOperationExpand rightLeft rightOperator rightRight
                    in
                    { beforeRightestOperatorExpressionChain =
                        { operator = operator, expression = rightOperationExpanded.leftest }
                            :: rightOperationExpanded.beforeRightestOperatorExpressionChain
                    , rightestOperator = rightOperationExpanded.rightestOperator
                    , rightestExpression = rightOperationExpanded.rightestExpression
                    }

                rightNotOperation ->
                    { beforeRightestOperatorExpressionChain = []
                    , rightestOperator = operator
                    , rightestExpression = rightNotOperation
                    }
    in
    case left of
        Gren.Syntax.Node.Node _ (Gren.Syntax.Expression.OperatorApplication leftOperator leftLeft leftRight) ->
            let
                leftOperationExpanded :
                    { leftest : Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
                    , beforeRightestOperatorExpressionChain :
                        List
                            { operator : String
                            , expression : Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
                            }
                    , rightestOperator : String
                    , rightestExpression : Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
                    }
                leftOperationExpanded =
                    expressionOperationExpand leftLeft leftOperator leftRight
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

        leftNotOperation ->
            { leftest = leftNotOperation
            , beforeRightestOperatorExpressionChain = rightExpanded.beforeRightestOperatorExpressionChain
            , rightestOperator = rightExpanded.rightestOperator
            , rightestExpression = rightExpanded.rightestExpression
            }


expressionIsSpaceSeparatedExceptApplication : Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression -> Bool
expressionIsSpaceSeparatedExceptApplication expressionNode =
    if expressionIsSpaceSeparated (expressionNode |> Gren.Syntax.Node.value) then
        case expressionNode |> expressionToNotParenthesized of
            Gren.Syntax.Node.Node _ (Gren.Syntax.Expression.Application _) ->
                False

            _ ->
                True

    else
        False


expressionParenthesizedIfSpaceSeparatedExceptApplication :
    List (Gren.Syntax.Node.Node String)
    -> Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
    -> Print
expressionParenthesizedIfSpaceSeparatedExceptApplication syntaxComments expressionNode =
    if expressionNode |> expressionIsSpaceSeparatedExceptApplication then
        expressionParenthesized syntaxComments expressionNode

    else
        expressionNotParenthesized syntaxComments expressionNode


expressionParenthesizedIfSpaceSeparatedExceptApplicationAndLambda :
    List (Gren.Syntax.Node.Node String)
    -> Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
    -> Print
expressionParenthesizedIfSpaceSeparatedExceptApplicationAndLambda syntaxComments expressionNode =
    if expressionNode |> Gren.Syntax.Node.value |> expressionIsSpaceSeparated then
        case expressionNode |> expressionToNotParenthesized |> Gren.Syntax.Node.value of
            Gren.Syntax.Expression.Application _ ->
                expressionNotParenthesized syntaxComments expressionNode

            Gren.Syntax.Expression.LambdaExpression _ ->
                expressionNotParenthesized syntaxComments expressionNode

            _ ->
                expressionParenthesized syntaxComments expressionNode

    else
        expressionNotParenthesized syntaxComments expressionNode


expressionList :
    List (Gren.Syntax.Node.Node String)
    ->
        { elements : List (Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression)
        , fullRange : Gren.Syntax.Range.Range
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
                    { endLocation : Gren.Syntax.Range.Location
                    , reverse : List Print
                    }
                elementPrintsWithCommentsBefore =
                    (element0 :: element1Up)
                        |> List.foldl
                            (\elementNode soFar ->
                                let
                                    (Gren.Syntax.Node.Node elementRange _) =
                                        elementNode

                                    print : Print
                                    print =
                                        expressionNotParenthesized syntaxComments
                                            elementNode
                                in
                                { endLocation = elementRange.end
                                , reverse =
                                    (case
                                        commentsInRange { start = soFar.endLocation, end = elementRange.start }
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
    List (Gren.Syntax.Node.Node String)
    ->
        { fullRange : Gren.Syntax.Range.Range
        , recordVariable : Gren.Syntax.Node.Node String
        , fields :
            List
                (Gren.Syntax.Node.Node
                    ( Gren.Syntax.Node.Node String
                    , Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
                    )
                )
        }
    -> Print
expressionRecordUpdate syntaxComments syntaxRecordUpdate =
    let
        fieldPrintsWithCommentsBefore :
            { endLocation : Gren.Syntax.Range.Location
            , reverse : List Print
            }
        fieldPrintsWithCommentsBefore =
            syntaxRecordUpdate.fields
                |> List.foldl
                    (\(Gren.Syntax.Node.Node _ fieldSyntax) soFar ->
                        let
                            ( Gren.Syntax.Node.Node fieldNameRange fieldName, fieldValueNode ) =
                                fieldSyntax

                            valuePrint : Print
                            valuePrint =
                                expressionNotParenthesized syntaxComments fieldValueNode

                            (Gren.Syntax.Node.Node fieldValueRange _) =
                                fieldValueNode
                        in
                        { endLocation = fieldValueRange.end
                        , reverse =
                            (Print.withIndentIncreasedBy 2
                                (case
                                    commentsInRange
                                        { start = soFar.endLocation, end = fieldNameRange.start }
                                        syntaxComments
                                 of
                                    [] ->
                                        Print.exactly (fieldName ++ " =")

                                    comment0 :: comment1Up ->
                                        let
                                            commentsBeforeName : { print : Print, lineSpread : Print.LineSpread }
                                            commentsBeforeName =
                                                collapsibleComments (comment0 :: comment1Up)
                                        in
                                        commentsBeforeName.print
                                            |> Print.followedBy
                                                (Print.spaceOrLinebreakIndented commentsBeforeName.lineSpread)
                                            |> Print.followedBy (Print.exactly (fieldName ++ " ="))
                                )
                                |> Print.followedBy
                                    (Print.withIndentAtNextMultipleOf4
                                        ((case
                                            commentsInRange
                                                { start = fieldNameRange.start, end = fieldValueRange.start }
                                                syntaxComments
                                          of
                                            [] ->
                                                Print.spaceOrLinebreakIndented
                                                    (lineSpreadBetweenRanges
                                                        fieldNameRange
                                                        (fieldValueNode |> Gren.Syntax.Node.range)
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
                                                                            fieldNameRange
                                                                            (fieldValueNode |> Gren.Syntax.Node.range)
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
                        syntaxRecordUpdate.recordVariable
                            |> Gren.Syntax.Node.range
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
                    , end = syntaxRecordUpdate.recordVariable |> Gren.Syntax.Node.range |> .start
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

        recordVariablePrint : Print
        recordVariablePrint =
            Print.exactly
                (syntaxRecordUpdate.recordVariable |> Gren.Syntax.Node.value)
    in
    printExactlyCurlyOpeningSpace
        |> Print.followedBy
            (Print.withIndentIncreasedBy 2
                (case maybeCommentsBeforeRecordVariable of
                    Nothing ->
                        recordVariablePrint

                    Just commentsCollapsibleBeforeRecordVariable ->
                        commentsCollapsibleBeforeRecordVariable.print
                            |> Print.followedBy
                                (Print.spaceOrLinebreakIndented commentsCollapsibleBeforeRecordVariable.lineSpread)
                            |> Print.followedBy recordVariablePrint
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
                    |> -- yes, gren-format indents trailing comments
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
    List (Gren.Syntax.Node.Node String)
    -> Gren.Syntax.Node.Node Gren.Syntax.Expression.Lambda
    -> Print
expressionLambda syntaxComments (Gren.Syntax.Node.Node fullRange syntaxLambda) =
    let
        parameterPrintsWithCommentsBefore :
            { endLocation : Gren.Syntax.Range.Location
            , reverse : List Print
            }
        parameterPrintsWithCommentsBefore =
            syntaxLambda.args
                |> List.foldl
                    (\parameterPattern soFar ->
                        let
                            parameterRange : Gren.Syntax.Range.Range
                            parameterRange =
                                parameterPattern |> Gren.Syntax.Node.range

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
                    , endLocation = fullRange.start
                    }

        commentsBeforeResult : List String
        commentsBeforeResult =
            commentsInRange
                { start = parameterPrintsWithCommentsBefore.endLocation
                , end = syntaxLambda.expression |> Gren.Syntax.Node.range |> .start
                }
                syntaxComments

        parametersLineSpread : Print.LineSpread
        parametersLineSpread =
            parameterPrintsWithCommentsBefore.reverse
                |> Print.lineSpreadListMapAndCombine Print.lineSpread

        resultPrint : Print
        resultPrint =
            expressionNotParenthesized syntaxComments
                syntaxLambda.expression
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
                                                lineSpreadInRange fullRange
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
    List (Gren.Syntax.Node.Node String)
    ->
        { fullRange : Gren.Syntax.Range.Range
        , condition : Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
        , conditionLineSpreadMinimum : Print.LineSpread
        , onTrue : Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
        , onFalse : Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
        }
    -> Print
expressionIfThenElse syntaxComments syntaxIfThenElse =
    -- IGNORE TCO
    let
        commentsBeforeCondition : List String
        commentsBeforeCondition =
            commentsInRange
                { start = syntaxIfThenElse.fullRange.start
                , end = syntaxIfThenElse.condition |> Gren.Syntax.Node.range |> .start
                }
                syntaxComments

        commentsBeforeOnTrue : List String
        commentsBeforeOnTrue =
            commentsInRange
                { start = syntaxIfThenElse.condition |> Gren.Syntax.Node.range |> .end
                , end = syntaxIfThenElse.onTrue |> Gren.Syntax.Node.range |> .start
                }
                syntaxComments

        onFalseNotParenthesized : Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
        onFalseNotParenthesized =
            syntaxIfThenElse.onFalse |> expressionToNotParenthesized

        commentsBeforeOnFalseNotParenthesizedInParens : List String
        commentsBeforeOnFalseNotParenthesizedInParens =
            commentsInRange
                { start = syntaxIfThenElse.onFalse |> Gren.Syntax.Node.range |> .start
                , end = onFalseNotParenthesized |> Gren.Syntax.Node.range |> .start
                }
                syntaxComments

        commentsBeforeOnFalse : List String
        commentsBeforeOnFalse =
            commentsInRange
                { start = syntaxIfThenElse.onTrue |> Gren.Syntax.Node.range |> .end
                , end = syntaxIfThenElse.onFalse |> Gren.Syntax.Node.range |> .start
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
            (case ( commentsBeforeOnFalseNotParenthesizedInParens, onFalseNotParenthesized ) of
                ( [], Gren.Syntax.Node.Node onFalseNotParenthesizedRange (Gren.Syntax.Expression.IfBlock onFalseCondition onFalseOnTrue onFalseOnFalse) ) ->
                    case commentsBeforeOnFalse of
                        [] ->
                            printExactlySpace
                                |> Print.followedBy
                                    (expressionIfThenElse syntaxComments
                                        { fullRange = onFalseNotParenthesizedRange
                                        , condition = onFalseCondition
                                        , conditionLineSpreadMinimum = Print.SingleLine
                                        , onTrue = onFalseOnTrue
                                        , onFalse = onFalseOnFalse
                                        }
                                    )

                        comment0 :: comment1Up ->
                            Print.linebreakIndented
                                |> Print.followedBy
                                    (comments (comment0 :: comment1Up))
                                |> Print.followedBy Print.linebreakIndented
                                |> Print.followedBy
                                    (expressionIfThenElse syntaxComments
                                        { fullRange = onFalseNotParenthesizedRange
                                        , conditionLineSpreadMinimum =
                                            -- don't ask me why
                                            Print.MultipleLines
                                        , condition = onFalseCondition
                                        , onTrue = onFalseOnTrue
                                        , onFalse = onFalseOnFalse
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
    List (Gren.Syntax.Node.Node String)
    ->
        { fullRange : Gren.Syntax.Range.Range
        , expression : Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
        , cases : Gren.Syntax.Expression.Cases
        }
    -> Print
expressionCaseOf syntaxComments syntaxCaseOf =
    let
        commentsBeforeCasedExpression : List String
        commentsBeforeCasedExpression =
            commentsInRange
                { start = syntaxCaseOf.fullRange.start
                , end = syntaxCaseOf.expression |> Gren.Syntax.Node.range |> .start
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
    printExactlyCase
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
        |> Print.followedBy printExactlyOf
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (syntaxCaseOf.cases
                            |> List.foldl
                                (\( casePattern, caseResult ) soFar ->
                                    let
                                        commentsBeforeCasePattern : List String
                                        commentsBeforeCasePattern =
                                            commentsInRange
                                                { start = soFar.endLocation
                                                , end = casePattern |> Gren.Syntax.Node.range |> .start
                                                }
                                                syntaxComments

                                        casePrint : Print
                                        casePrint =
                                            case_ syntaxComments ( casePattern, caseResult )

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
                                    { endLocation = caseResult |> Gren.Syntax.Node.range |> .end
                                    , reverse = commentsAndCasePrint :: soFar.reverse
                                    }
                                )
                                { endLocation = syntaxCaseOf.expression |> Gren.Syntax.Node.range |> .end
                                , reverse = []
                                }
                            |> .reverse
                            |> Print.listReverseAndIntersperseAndFlatten
                                printLinebreakLinebreakIndented
                        )
                )
            )


expressionLetIn :
    List (Gren.Syntax.Node.Node String)
    ->
        { fullRange : Gren.Syntax.Range.Range
        , letDeclaration0 : Gren.Syntax.Node.Node Gren.Syntax.Expression.LetDeclaration
        , letDeclaration1Up : List (Gren.Syntax.Node.Node Gren.Syntax.Expression.LetDeclaration)
        , result : Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
        }
    -> Print
expressionLetIn syntaxComments syntaxLetIn =
    let
        letDeclarationPrints : { endLocation : Gren.Syntax.Range.Location, reverse : List Print }
        letDeclarationPrints =
            (syntaxLetIn.letDeclaration0 :: syntaxLetIn.letDeclaration1Up)
                |> List.foldl
                    (\(Gren.Syntax.Node.Node letDeclarationRange letDeclaration) soFar ->
                        let
                            commentsBefore : List String
                            commentsBefore =
                                commentsInRange
                                    { start = soFar.endLocation
                                    , end = letDeclarationRange.start
                                    }
                                    syntaxComments

                            letDeclarationPrint : Print
                            letDeclarationPrint =
                                expressionLetDeclaration syntaxComments letDeclaration

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
                        { endLocation = letDeclarationRange.end
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
                        |> Gren.Syntax.Node.range
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
    List (Gren.Syntax.Node.Node String)
    -> Gren.Syntax.Expression.LetDeclaration
    -> Print
expressionLetDeclaration syntaxComments letDeclaration =
    case letDeclaration of
        Gren.Syntax.Expression.LetFunction letDeclarationExpression ->
            let
                implementationPrint : Print
                implementationPrint =
                    declarationExpressionImplementation syntaxComments
                        (letDeclarationExpression.declaration |> Gren.Syntax.Node.value)
            in
            case letDeclarationExpression.signature of
                Nothing ->
                    implementationPrint

                Just (Gren.Syntax.Node.Node signatureRange signature) ->
                    let
                        commentsBetweenSignatureAndImplementationName : List String
                        commentsBetweenSignatureAndImplementationName =
                            commentsInRange
                                { start = signatureRange.end
                                , end =
                                    letDeclarationExpression.declaration
                                        |> Gren.Syntax.Node.range
                                        |> .start
                                }
                                syntaxComments
                    in
                    declarationSignature syntaxComments signature
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

        Gren.Syntax.Expression.LetDestructuring destructuringPattern destructuredExpression ->
            let
                commentsBeforeDestructuredExpression : List String
                commentsBeforeDestructuredExpression =
                    commentsInRange
                        { start = destructuringPattern |> Gren.Syntax.Node.range |> .end
                        , end = destructuredExpression |> Gren.Syntax.Node.range |> .start
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
    Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
    -> Gren.Syntax.Node.Node Gren.Syntax.Expression.Expression
expressionToNotParenthesized (Gren.Syntax.Node.Node fullRange syntaxExpression) =
    -- IGNORE TCO
    case syntaxExpression of
        Gren.Syntax.Expression.ParenthesizedExpression inParens ->
            inParens |> expressionToNotParenthesized

        Gren.Syntax.Expression.TupledExpression parts ->
            case parts of
                [ inParens ] ->
                    -- should be handled by ParenthesizedExpression
                    inParens |> expressionToNotParenthesized

                [] ->
                    Gren.Syntax.Node.Node fullRange Gren.Syntax.Expression.UnitExpr

                [ part0, part1 ] ->
                    Gren.Syntax.Node.Node fullRange (Gren.Syntax.Expression.TupledExpression [ part0, part1 ])

                [ part0, part1, part2 ] ->
                    Gren.Syntax.Node.Node fullRange (Gren.Syntax.Expression.TupledExpression [ part0, part1, part2 ])

                part0 :: part1 :: part2 :: part3 :: part4Up ->
                    -- invalid syntax
                    Gren.Syntax.Node.Node fullRange (Gren.Syntax.Expression.TupledExpression (part0 :: part1 :: part2 :: part3 :: part4Up))

        syntaxExpressionNotParenthesized ->
            Gren.Syntax.Node.Node fullRange syntaxExpressionNotParenthesized


{-| Print a single [`Gren.Syntax.Expression.Case`](https://gren-lang.org/packages/stil4m/gren-syntax/latest/Gren-Syntax-Expression#Case)
-}
case_ :
    List (Gren.Syntax.Node.Node String)
    -> Gren.Syntax.Expression.Case
    -> Print
case_ syntaxComments ( casePattern, caseResult ) =
    let
        patternPrint : Print
        patternPrint =
            patternNotParenthesized syntaxComments casePattern

        commentsBeforeExpression : List String
        commentsBeforeExpression =
            commentsInRange
                { start = casePattern |> Gren.Syntax.Node.range |> .end
                , end = caseResult |> Gren.Syntax.Node.range |> .start
                }
                syntaxComments

        caseResultPrint : Print
        caseResultPrint =
            expressionNotParenthesized syntaxComments caseResult
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


printExactlyParensOpeningParensClosed : Print.Print
printExactlyParensOpeningParensClosed =
    Print.exactly "()"


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


printExactlyCase : Print
printExactlyCase =
    Print.exactly "case"


printExactlyOf : Print
printExactlyOf =
    Print.exactly "of"


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
