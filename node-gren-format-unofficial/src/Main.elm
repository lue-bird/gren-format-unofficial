port module Main exposing (main)

import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Dict
import GrenParserLenient
import GrenPrint
import GrenSyntax
import Json.Decode
import Json.Encode
import Node
import Set


type State
    = WaitingForLaunchArguments
    | ShowingHelp
    | WaitingForGrenJson { mode : ProjectMode }
    | SingleProjectRun SingleProjectRunState
    | SingleFileStandardStreamRun SingleFileStandardStreamRunState
    | Watching WatchState
    | GrenJsonReadFailed String


type SingleFileStandardStreamRunState
    = AssemblingModuleSourceFromStandardIn String
    | ModuleSourceReceived String


type alias SingleProjectRunState =
    { sourceDirectoriesToRead : Set.Set String
    , sourceFilesToRead : Set.Set String
    , formattedModulesToWrite : Dict.Dict String Bytes
    , sourceDirectoryReadErrors : Array { path : String, message : String }
    , sourceFileReadErrors : Array { path : String, message : String }
    }


type alias WatchState =
    { grenJsonSourceDirectories : Array String
    , sourceFilesToRead : Set.Set String
    , formattedModulesToWrite : Dict.Dict String Bytes
    , sourceFileReadErrors : Dict.Dict String String
    }


type ProjectMode
    = ProjectModeSingleRun
    | ProjectModeWatch


initialState : State
initialState =
    WaitingForLaunchArguments


interface : State -> Node.Interface State
interface state =
    case state of
        WaitingForLaunchArguments ->
            Node.launchArgumentsRequest
                |> Node.interfaceFutureMap
                    (\launchArguments ->
                        case
                            launchArguments
                                |> Array.drop 2
                                |> Array.filter
                                    (\arg ->
                                        (arg /= "--yes")
                                            && (arg /= "--gren-version")
                                            && (arg /= "0.19.0")
                                            && (arg /= "0.19.1")
                                            && (arg /= "0.19")
                                            && (arg /= "--gren-version=0.19")
                                            && (arg /= "--gren-version=0.19.0")
                                            && (arg /= "--gren-version=0.19.1")
                                    )
                        of
                            [] ->
                                WaitingForGrenJson { mode = ProjectModeSingleRun }

                            [ "watch" ] ->
                                WaitingForGrenJson { mode = ProjectModeWatch }

                            [ "help" ] ->
                                ShowingHelp

                            [ "--stdin" ] ->
                                SingleFileStandardStreamRun (AssemblingModuleSourceFromStandardIn "")

                            _ ->
                                ShowingHelp
                    )

        ShowingHelp ->
            nodeShowHelpText

        SingleFileStandardStreamRun singleFileStandardStreamRun ->
            interfaceSingleFileStandardStreamRun singleFileStandardStreamRun

        WaitingForGrenJson waitingForGrenJson ->
            nodeGrenJsonSourceDirectoriesRequest
                |> Node.interfaceFutureMap
                    (\grenJsonBytesOrError ->
                        case grenJsonBytesOrError of
                            Err grenJsonReadError ->
                                GrenJsonReadFailed grenJsonReadError

                            Ok grenJsonSourceDirectories ->
                                case waitingForGrenJson.mode of
                                    ProjectModeSingleRun ->
                                        SingleProjectRun
                                            { sourceDirectoriesToRead =
                                                grenJsonSourceDirectories |> Set.fromList
                                            , sourceFilesToRead = Set.empty
                                            , formattedModulesToWrite = Dict.empty
                                            , sourceFileReadErrors = []
                                            , sourceDirectoryReadErrors = []
                                            }

                                    ProjectModeWatch ->
                                        Watching
                                            { grenJsonSourceDirectories = grenJsonSourceDirectories
                                            , sourceFilesToRead = Set.empty
                                            , formattedModulesToWrite = Dict.empty
                                            , sourceFileReadErrors = Dict.empty
                                            }
                    )

        SingleProjectRun singleRunState ->
            singleRunInterface singleRunState

        Watching watchState ->
            watchInterface watchState

        GrenJsonReadFailed grenJsonDecodeError ->
            errorInterface grenJsonDecodeError


errorInterface : String -> Node.Interface future_
errorInterface message =
    [ Node.standardErrWrite (message ++ "\n")
    , Node.exit 1
    ]
        |> Node.interfaceBatch


nodeShowHelpText : Node.Interface future_
nodeShowHelpText =
    Node.standardOutWrite
        ("""Format gren 0.5.4 source directory and tests/ modules in the current project.

"""
            ++ ([ { command = "gren-format-unofficial"
                  , description = "format all of them once"
                  }
                , { command = "gren-format-unofficial watch"
                  , description = "format edited and added modules on save"
                  }
                , { command = "gren-format-unofficial --stdin"
                  , description = "for editors and other tooling"
                  }
                ]
                    |> Array.map
                        (\commandAndDescription ->
                            "  - "
                                ++ (commandAndDescription.command
                                        |> ansiBold
                                   )
                                ++ ": "
                                ++ commandAndDescription.description
                        )
                    |> String.join "\n"
               )
            ++ """

"""
        )


ansiBold : String -> String
ansiBold text =
    "\u{001B}[1m" ++ text ++ "\u{001B}[22m"


interfaceSingleFileStandardStreamRun : SingleFileStandardStreamRunState -> Node.Interface State
interfaceSingleFileStandardStreamRun singleFileStandardStreamRun =
    case singleFileStandardStreamRun of
        AssemblingModuleSourceFromStandardIn moduleSourceSoFar ->
            Node.standardInRawListen
                |> Node.interfaceFutureMap
                    (\nextModuleSourceChunkOrEnd ->
                        SingleFileStandardStreamRun
                            (case nextModuleSourceChunkOrEnd of
                                Node.StreamDataEndReached ->
                                    ModuleSourceReceived moduleSourceSoFar

                                Node.StreamDataReceived nextModuleSourceChunk ->
                                    AssemblingModuleSourceFromStandardIn
                                        (moduleSourceSoFar
                                            ++ nextModuleSourceChunk
                                            ++ ""
                                        )
                            )
                    )

        ModuleSourceReceived moduleSource ->
            case moduleSource |> GrenParserLenient.run GrenParserLenient.module_ of
                Nothing ->
                    errorInterface
                        ("module failed to parse: "
                            ++ (moduleSource |> String.left 90)
                            ++ "..."
                            ++ (moduleSource |> String.right 90)
                        )

                Just moduleSyntax ->
                    Node.standardOutWrite
                        (moduleSyntax
                            |> GrenPrint.module_
                            |> GrenPrint.toString
                        )


nodeGrenJsonSourceDirectoriesRequest : Node.Interface (Result String (Array String))
nodeGrenJsonSourceDirectoriesRequest =
    Node.fileRequest "gren.json"
        |> Node.interfaceFutureMap
            (\grenJsonBytesOrError ->
                case grenJsonBytesOrError of
                    Err fileReadError ->
                        Err
                            ("gren.json couldn't be read because "
                                ++ fileReadError.message
                            )

                    Ok grenJsonBytes ->
                        case grenJsonBytes |> Bytes.Decode.decode (Bytes.Decode.string (Bytes.width grenJsonBytes)) of
                            Nothing ->
                                Err "gren.json bytes could not be decoded into UTF-8 String"

                            Just grenJsonString ->
                                case
                                    grenJsonString
                                        |> Json.Decode.decodeString
                                            (Json.Decode.oneOf
                                                [ Json.Decode.field "source-directories" (Json.Decode.list Json.Decode.string)
                                                , Json.Decode.succeed packageSourceDirectories
                                                ]
                                            )
                                of
                                    Err jsonDecodeError ->
                                        Err
                                            ("gren.json failed to parse due to "
                                                ++ (jsonDecodeError |> Json.Decode.errorToString)
                                            )

                                    Ok grenJson ->
                                        Ok grenJson
            )


singleRunInterface : SingleProjectRunState -> Node.Interface State
singleRunInterface state =
    [ state.formattedModulesToWrite
        |> fastDictToListAndMap
            (\moduleToFormatPath moduleToFormatContent ->
                Node.fileWrite
                    { path = moduleToFormatPath
                    , content = moduleToFormatContent
                    }
                    |> Node.interfaceFutureMap
                        (\_ ->
                            SingleProjectRun
                                { formattedModulesToWrite =
                                    state.formattedModulesToWrite |> Dict.remove moduleToFormatPath
                                , sourceDirectoriesToRead = state.sourceDirectoriesToRead
                                , sourceFilesToRead = state.sourceFilesToRead
                                , sourceDirectoryReadErrors = state.sourceDirectoryReadErrors
                                , sourceFileReadErrors = state.sourceFileReadErrors
                                }
                        )
            )
        |> Node.interfaceBatch
    , state.sourceDirectoriesToRead
        |> fastSetToListAndMap
            (\sourceDirectoryPath ->
                Node.directorySubPathsRequest sourceDirectoryPath
                    |> Node.interfaceFutureMap
                        (\subPathsOrError ->
                            case subPathsOrError of
                                Err sourceDirectoryReadError ->
                                    SingleProjectRun
                                        { state
                                            | sourceDirectoryReadErrors =
                                                { path = sourceDirectoryPath
                                                , message = sourceDirectoryReadError.message
                                                }
                                                    :: state.sourceDirectoryReadErrors
                                        }

                                Ok subPaths ->
                                    SingleProjectRun
                                        { sourceDirectoriesToRead =
                                            state.sourceDirectoriesToRead
                                                |> Set.remove sourceDirectoryPath
                                        , sourceFilesToRead =
                                            subPaths
                                                |> Array.foldl
                                                    (\subPath soFar ->
                                                        if subPath |> String.endsWith ".gren" then
                                                            soFar |> Set.insert (sourceDirectoryPath ++ "/" ++ subPath)

                                                        else
                                                            soFar
                                                    )
                                                    state.sourceFilesToRead
                                        , formattedModulesToWrite = state.formattedModulesToWrite
                                        , sourceDirectoryReadErrors = state.sourceDirectoryReadErrors
                                        , sourceFileReadErrors = state.sourceFileReadErrors
                                        }
                        )
            )
        |> Node.interfaceBatch
    , Node.directorySubPathsRequest "tests"
        |> Node.interfaceFutureMap
            (\testSubPathsOrError ->
                case testSubPathsOrError of
                    Err _ ->
                        -- tests/ is optional. So all's fine
                        SingleProjectRun state

                    Ok subPaths ->
                        SingleProjectRun
                            { sourceFilesToRead =
                                subPaths
                                    |> Array.foldl
                                        (\subPath soFar ->
                                            if subPath |> String.endsWith ".gren" then
                                                soFar |> Set.insert ("tests/" ++ subPath)

                                            else
                                                soFar
                                        )
                                        state.sourceFilesToRead
                            , sourceDirectoriesToRead = state.sourceDirectoriesToRead
                            , sourceFileReadErrors = state.sourceFileReadErrors
                            , sourceDirectoryReadErrors = state.sourceDirectoryReadErrors
                            , formattedModulesToWrite = state.formattedModulesToWrite
                            }
            )
    , state.sourceFilesToRead
        |> fastSetToListAndMap
            (\sourceFilePath ->
                Node.fileRequest sourceFilePath
                    |> Node.interfaceFutureMap
                        (\sourceBytesOrError ->
                            let
                                sourceBytesOrReadError : Result String GrenSyntax.File
                                sourceBytesOrReadError =
                                    case sourceBytesOrError of
                                        Err sourceFileReadError ->
                                            Err sourceFileReadError.message

                                        Ok sourceBytes ->
                                            sourceBytes |> bytesToGrenSyntaxModule
                            in
                            case sourceBytesOrReadError of
                                Err readError ->
                                    SingleProjectRun
                                        { state
                                            | sourceFileReadErrors =
                                                { path = sourceFilePath
                                                , message = readError
                                                }
                                                    :: state.sourceFileReadErrors
                                        }

                                Ok syntax ->
                                    SingleProjectRun
                                        { sourceDirectoriesToRead = state.sourceDirectoriesToRead
                                        , sourceFileReadErrors = state.sourceFileReadErrors
                                        , sourceDirectoryReadErrors = state.sourceDirectoryReadErrors
                                        , sourceFilesToRead =
                                            state.sourceFilesToRead
                                                |> Set.remove sourceFilePath
                                        , formattedModulesToWrite =
                                            state.formattedModulesToWrite
                                                |> Dict.insert sourceFilePath
                                                    (syntax |> grenSyntaxModuleToBytes)
                                        }
                        )
            )
        |> Node.interfaceBatch
    , state.sourceDirectoryReadErrors
        |> Array.map
            (\directoryReadError ->
                Node.standardErrWrite
                    ("failed to read the source directory "
                        ++ directoryReadError.path
                        ++ ": "
                        ++ directoryReadError.message
                        ++ "\n"
                    )
            )
        |> Node.interfaceBatch
    , state.sourceFileReadErrors
        |> Array.map
            (\fileReadError ->
                Node.standardErrWrite
                    ("failed to read the source file "
                        ++ fileReadError.path
                        ++ ": "
                        ++ fileReadError.message
                        ++ "\n"
                    )
            )
        |> Node.interfaceBatch
    ]
        |> Node.interfaceBatch


bytesToGrenSyntaxModule : Bytes -> Result String GrenSyntax.File
bytesToGrenSyntaxModule sourceBytes =
    case sourceBytes |> Bytes.Decode.decode (Bytes.Decode.string (sourceBytes |> Bytes.width)) of
        Nothing ->
            Err "source bytes couldn't be decoded into UTF-8"

        Just source ->
            case source |> GrenParserLenient.run GrenParserLenient.module_ of
                Nothing ->
                    Err "source couldn't be parsed. Check for compiler errors."

                Just syntax ->
                    Ok syntax


grenSyntaxModuleToBytes : GrenSyntax.File -> Bytes
grenSyntaxModuleToBytes grenSyntaxModule =
    grenSyntaxModule
        |> GrenPrint.module_
        |> GrenPrint.toString
        |> Bytes.Encode.string
        |> Bytes.Encode.encode


watchInterface : WatchState -> Node.Interface State
watchInterface state =
    [ state.grenJsonSourceDirectories
        |> Array.map
            (\grenJsonSourceDirectory ->
                Node.fileChangeListen grenJsonSourceDirectory
                    |> Node.interfaceFutureMap
                        (\fileChange ->
                            case fileChange of
                                Node.FileRemoved _ ->
                                    Watching state

                                Node.FileAddedOrChanged addedOrChangedPath ->
                                    if addedOrChangedPath |> String.endsWith ".gren" then
                                        Watching
                                            { grenJsonSourceDirectories = state.grenJsonSourceDirectories
                                            , sourceFilesToRead =
                                                state.sourceFilesToRead |> Set.insert addedOrChangedPath
                                            , formattedModulesToWrite = state.formattedModulesToWrite
                                            , sourceFileReadErrors = state.sourceFileReadErrors
                                            }

                                    else
                                        Watching state
                        )
            )
        |> Node.interfaceBatch
    , nodeTestsChangeListen
        |> Node.interfaceFutureMap
            (\fileChange ->
                case fileChange of
                    Node.FileRemoved _ ->
                        Watching state

                    Node.FileAddedOrChanged addedOrChangedPath ->
                        if addedOrChangedPath |> String.endsWith ".gren" then
                            Watching
                                { grenJsonSourceDirectories = state.grenJsonSourceDirectories
                                , sourceFilesToRead =
                                    state.sourceFilesToRead |> Set.insert addedOrChangedPath
                                , formattedModulesToWrite = state.formattedModulesToWrite
                                , sourceFileReadErrors = state.sourceFileReadErrors
                                }

                        else
                            Watching state
            )
    , state.formattedModulesToWrite
        |> fastDictToListAndMap
            (\moduleToFormatPath moduleToFormatContent ->
                Node.fileWrite
                    { path = moduleToFormatPath
                    , content = moduleToFormatContent
                    }
                    |> Node.interfaceFutureMap
                        (\_ ->
                            Watching
                                { grenJsonSourceDirectories = state.grenJsonSourceDirectories
                                , sourceFilesToRead = state.sourceFilesToRead
                                , formattedModulesToWrite =
                                    state.formattedModulesToWrite |> Dict.remove moduleToFormatPath
                                , sourceFileReadErrors = state.sourceFileReadErrors
                                }
                        )
            )
        |> Node.interfaceBatch
    , state.sourceFilesToRead
        |> fastSetToListAndMap
            (\sourceFilePath ->
                Node.fileRequest sourceFilePath
                    |> Node.interfaceFutureMap
                        (\sourceBytesOrError ->
                            let
                                sourceBytesOrReadError : Result String GrenSyntax.File
                                sourceBytesOrReadError =
                                    case sourceBytesOrError of
                                        Err sourceFileReadError ->
                                            Err sourceFileReadError.message

                                        Ok sourceBytes ->
                                            sourceBytes |> bytesToGrenSyntaxModule
                            in
                            case sourceBytesOrReadError of
                                Err readError ->
                                    Watching
                                        { state
                                            | sourceFileReadErrors =
                                                state.sourceFileReadErrors
                                                    |> Dict.insert sourceFilePath readError
                                            , sourceFilesToRead =
                                                state.sourceFilesToRead
                                                    |> Set.remove sourceFilePath
                                        }

                                Ok syntax ->
                                    Watching
                                        { grenJsonSourceDirectories = state.grenJsonSourceDirectories
                                        , sourceFilesToRead =
                                            state.sourceFilesToRead
                                                |> Set.remove sourceFilePath
                                        , formattedModulesToWrite =
                                            state.formattedModulesToWrite
                                                |> Dict.insert sourceFilePath
                                                    (syntax |> grenSyntaxModuleToBytes)
                                        , sourceFileReadErrors =
                                            state.sourceFileReadErrors
                                                |> Dict.remove sourceFilePath
                                        }
                        )
            )
        |> Node.interfaceBatch
    , nodeHideCursor
    , state.sourceFileReadErrors
        |> fastDictToListAndMap
            (\fileReadErrorPath fileReadError ->
                Node.standardErrWrite
                    ("failed to read the source file "
                        ++ fileReadErrorPath
                        ++ ": "
                        ++ fileReadError
                        ++ "\n"
                    )
            )
        |> Node.interfaceBatch
    ]
        |> Node.interfaceBatch


nodeHideCursor : Node.Interface future_
nodeHideCursor =
    Node.standardOutWrite ansiHideCursor


ansiHideCursor : String
ansiHideCursor =
    """\u{001B}[?25l"""


nodeTestsChangeListen : Node.Interface Node.FileChange
nodeTestsChangeListen =
    Node.fileChangeListen "tests"


packageSourceDirectories : Array String
packageSourceDirectories =
    [ "src" ]


fastDictToListAndMap : (a -> b -> c) -> Dict.Dict a b -> Array c
fastDictToListAndMap keyValueToElement fastDict =
    fastDict
        |> Dict.foldr
            (\key value soFar ->
                keyValueToElement key value :: soFar
            )
            []


fastSetToListAndMap : (a -> b) -> Set.Set a -> Array b
fastSetToListAndMap keyToElement fastDict =
    fastDict
        |> Set.foldr
            (\key soFar ->
                keyToElement key :: soFar
            )
            []


main : Node.Program State
main =
    Node.program
        { initialState = initialState
        , interface = interface
        , ports = { fromJs = fromJs, toJs = toJs }
        }


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event
