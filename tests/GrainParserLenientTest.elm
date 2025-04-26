module GrainParserLenientTest exposing (all)

import Expect
import GrainParserLenientTestFullModules
import Gren.Syntax.Exposing
import Gren.Syntax.Expression
import Gren.Syntax.Infix
import Gren.Syntax.Node
import Gren.Syntax.Pattern
import Gren.Syntax.TypeAnnotation
import GrenParserLenient
import GrenSyntax
import Test


all : Test.Test
all =
    Test.concat
        [ Test.describe "FileTests"
            (List.map
                (\( n, s ) ->
                    Test.test ("sample " ++ String.fromInt n)
                        (\() ->
                            case GrenParserLenient.run GrenParserLenient.module_ s of
                                Nothing ->
                                    Expect.fail "failed to parse"

                                Just _ ->
                                    Expect.pass
                        )
                )
                GrainParserLenientTestFullModules.allSamples
            )
        , Test.test "moduleName"
            (\() ->
                "Foo"
                    |> GrenParserLenient.run GrenParserLenient.moduleName
                    |> Maybe.map Gren.Syntax.Node.value
                    |> Expect.equal (Just [ "Foo" ])
            )
        , Test.test "moduleNameDir"
            (\() ->
                "Foo.Bar"
                    |> GrenParserLenient.run GrenParserLenient.moduleName
                    |> Maybe.map Gren.Syntax.Node.value
                    |> Expect.equal (Just [ "Foo", "Bar" ])
            )
        , Test.describe "layout"
            [ Test.test "positively indented across multiple linebreaks and comments"
                (\() ->
                    """a =
        --x
        {- foo 
-}

    f 0"""
                        |> GrenParserLenient.run GrenParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "positively indented, too few spaces"
                (\() ->
                    """a = f
0"""
                        |> GrenParserLenient.run GrenParserLenient.declaration
                        |> Expect.equal Nothing
                )
            , Test.test "top indented across multiple linebreaks and comments"
                (\() ->
                    """a =
    let
        b = 0

        --x
        {- foo 
-}


        c = 0
    in
    b"""
                        |> GrenParserLenient.run GrenParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "top indented, too many spaces"
                (\() ->
                    """a =
    let
        b = 0
         c = 0
    in
    b"""
                        |> GrenParserLenient.run GrenParserLenient.declaration
                        |> Expect.equal Nothing
                )
            , Test.describe "comment"
                [ Test.test "singleLineComment"
                    (\() ->
                        GrenParserLenient.run GrenParserLenient.singleLineComment "--bar"
                            |> Expect.equal
                                (Just (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } "--bar"))
                    )
                , Test.test "singleLineComment state"
                    (\() ->
                        GrenParserLenient.run GrenParserLenient.singleLineComment "--bar"
                            |> Expect.equal
                                (Just (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } "--bar"))
                    )
                , Test.test "singleLineComment including 2-part utf-16 char range"
                    (\() ->
                        GrenParserLenient.run GrenParserLenient.singleLineComment "--bar🔧"
                            |> Expect.equal
                                (Just (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } } "--bar🔧"))
                    )
                , Test.test "singleLineComment does not include new line"
                    (\() ->
                        GrenParserLenient.run GrenParserLenient.singleLineComment "--bar\n"
                            |> Expect.equal Nothing
                    )
                , Test.test "multilineComment parse result"
                    (\() ->
                        GrenParserLenient.run GrenParserLenient.multiLineComment "{-foo\nbar-}"
                            |> Expect.equal
                                (Just (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } "{-foo\nbar-}"))
                    )
                , Test.test "multilineComment range"
                    (\() ->
                        GrenParserLenient.run GrenParserLenient.multiLineComment "{-foo\nbar-}"
                            |> Expect.equal
                                (Just (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } "{-foo\nbar-}"))
                    )
                , Test.test "multilineComment including 2-part utf-16 char range"
                    (\() ->
                        GrenParserLenient.run GrenParserLenient.multiLineComment "{-foo\nbar🔧-}"
                            |> Expect.equal
                                (Just (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 7 } } "{-foo\nbar🔧-}"))
                    )
                , Test.test "nested multilineComment only open"
                    (\() ->
                        GrenParserLenient.run GrenParserLenient.multiLineComment "{- {- -}"
                            |> Expect.equal Nothing
                    )
                , Test.test "nested multilineComment open and close"
                    (\() ->
                        GrenParserLenient.run GrenParserLenient.multiLineComment "{- {- -} -}"
                            |> Expect.equal
                                (Just (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } "{- {- -} -}"))
                    )
                , Test.test "multilineComment on module documentation"
                    (\() ->
                        GrenParserLenient.run GrenParserLenient.multiLineComment "{-|foo\nbar-}"
                            |> Expect.equal Nothing
                    )
                ]
            ]
        , Test.describe "module header"
            [ Test.test "formatted moduleDefinition"
                (\() ->
                    "module Foo exposing (Bar)"
                        |> expectSyntaxWithoutComments GrenParserLenient.moduleHeader
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 26 } }
                                (GrenSyntax.NormalModule
                                    { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Foo" ]
                                    , exposingList =
                                        Gren.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 26 } }
                                            (Gren.Syntax.Exposing.Explicit
                                                [ Gren.Syntax.Node.Node { start = { row = 1, column = 22 }, end = { row = 1, column = 25 } } (Gren.Syntax.Exposing.TypeOrAliasExpose "Bar")
                                                ]
                                            )
                                    }
                                )
                            )
                )
            , Test.test "port moduleDefinition"
                (\() ->
                    "port module Foo exposing (Bar)"
                        |> expectSyntaxWithoutComments GrenParserLenient.moduleHeader
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 31 } }
                                (GrenSyntax.PortModule
                                    { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 16 } } [ "Foo" ]
                                    , exposingList =
                                        Gren.Syntax.Node.Node { start = { row = 1, column = 17 }, end = { row = 1, column = 31 } }
                                            (Gren.Syntax.Exposing.Explicit
                                                [ Gren.Syntax.Node.Node { start = { row = 1, column = 27 }, end = { row = 1, column = 30 } } (Gren.Syntax.Exposing.TypeOrAliasExpose "Bar") ]
                                            )
                                    }
                                )
                            )
                )
            , Test.test "port moduleDefinition with spacing"
                (\() ->
                    "port module Foo exposing ( Bar )"
                        |> expectSyntaxWithoutComments GrenParserLenient.moduleHeader
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 33 } }
                                (GrenSyntax.PortModule
                                    { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 16 } } [ "Foo" ]
                                    , exposingList =
                                        Gren.Syntax.Node.Node { start = { row = 1, column = 17 }, end = { row = 1, column = 33 } }
                                            (Gren.Syntax.Exposing.Explicit
                                                [ Gren.Syntax.Node.Node { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } } (Gren.Syntax.Exposing.TypeOrAliasExpose "Bar") ]
                                            )
                                    }
                                )
                            )
                )
            , Test.test "effect moduleDefinition"
                (\() ->
                    "effect module Foo where {command = MyCmd, subscription = MySub } exposing (Bar)"
                        |> expectSyntaxWithoutComments GrenParserLenient.moduleHeader
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 80 } }
                                (GrenSyntax.EffectModule
                                    { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 18 } } [ "Foo" ]
                                    , exposingList = Gren.Syntax.Node.Node { start = { row = 1, column = 66 }, end = { row = 1, column = 80 } } (Gren.Syntax.Exposing.Explicit [ Gren.Syntax.Node.Node { start = { row = 1, column = 76 }, end = { row = 1, column = 79 } } (Gren.Syntax.Exposing.TypeOrAliasExpose "Bar") ])
                                    , command = Just (Gren.Syntax.Node.Node { start = { row = 1, column = 36 }, end = { row = 1, column = 41 } } "MyCmd")
                                    , subscription = Just (Gren.Syntax.Node.Node { start = { row = 1, column = 58 }, end = { row = 1, column = 63 } } "MySub")
                                    }
                                )
                            )
                )
            , Test.test "unformatted"
                (\() ->
                    "module \n Foo \n exposing  (..)"
                        |> expectSyntaxWithoutComments GrenParserLenient.moduleHeader
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 3, column = 16 } }
                                (GrenSyntax.NormalModule
                                    { moduleName = Gren.Syntax.Node.Node { start = { row = 2, column = 2 }, end = { row = 2, column = 5 } } [ "Foo" ]
                                    , exposingList =
                                        Gren.Syntax.Node.Node { start = { row = 3, column = 2 }, end = { row = 3, column = 16 } }
                                            (Gren.Syntax.Exposing.All { start = { row = 3, column = 13 }, end = { row = 3, column = 15 } })
                                    }
                                )
                            )
                )
            , Test.test "allow module name without indentation"
                (\() ->
                    """module 
Foo 
 exposing  (..)"""
                        |> GrenParserLenient.run GrenParserLenient.moduleHeader
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "exposing .."
                (\() ->
                    "module Foo exposing (..)"
                        |> expectSyntaxWithoutComments GrenParserLenient.moduleHeader
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                (GrenSyntax.NormalModule
                                    { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Foo" ]
                                    , exposingList =
                                        Gren.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } }
                                            (Gren.Syntax.Exposing.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                                    }
                                )
                            )
                )
            , Test.test "exposing ..."
                (\() ->
                    "module Foo exposing (...)"
                        |> expectSyntaxWithoutComments GrenParserLenient.moduleHeader
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 26 } }
                                (GrenSyntax.NormalModule
                                    { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Foo" ]
                                    , exposingList =
                                        Gren.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 26 } }
                                            (Gren.Syntax.Exposing.All { start = { row = 1, column = 22 }, end = { row = 1, column = 25 } })
                                    }
                                )
                            )
                )
            , Test.test "module name with _"
                (\() ->
                    "module I_en_gb exposing (..)"
                        |> expectSyntaxWithoutComments GrenParserLenient.moduleHeader
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 29 } }
                                (GrenSyntax.NormalModule
                                    { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 15 } } [ "I_en_gb" ]
                                    , exposingList =
                                        Gren.Syntax.Node.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 29 } }
                                            (Gren.Syntax.Exposing.All { start = { row = 1, column = 26 }, end = { row = 1, column = 28 } })
                                    }
                                )
                            )
                )
            , Test.describe "uppercase name"
                [ Test.test "lower and upper simple latin"
                    (\() ->
                        "MyCmd"
                            |> GrenParserLenient.run GrenParserLenient.nameUppercase
                            |> Expect.equal (Just "MyCmd")
                    )
                , Test.test "typeName not empty"
                    (\() ->
                        ""
                            |> GrenParserLenient.run GrenParserLenient.nameUppercase
                            |> Expect.equal Nothing
                    )
                , Test.test "typeName with number"
                    (\() ->
                        "T1"
                            |> GrenParserLenient.run GrenParserLenient.nameUppercase
                            |> Expect.equal (Just "T1")
                    )
                , Test.test "ρ function"
                    (\() ->
                        "ρ"
                            |> GrenParserLenient.run GrenParserLenient.nameLowercase
                            |> Expect.notEqual Nothing
                    )
                , Test.test "ε2 function"
                    (\() ->
                        "ε2"
                            |> GrenParserLenient.run GrenParserLenient.nameLowercase
                            |> Expect.notEqual Nothing
                    )
                , Test.test "εε function"
                    (\() ->
                        "εε"
                            |> GrenParserLenient.run GrenParserLenient.nameLowercase
                            |> Expect.notEqual Nothing
                    )
                , Test.test "ρ uppercase function"
                    (\() ->
                        String.toUpper "ρ"
                            |> GrenParserLenient.run GrenParserLenient.nameLowercase
                            |> Expect.equal Nothing
                    )
                , Test.test "ε uppercase function"
                    (\() ->
                        String.toUpper "ε"
                            |> GrenParserLenient.run GrenParserLenient.nameLowercase
                            |> Expect.equal Nothing
                    )
                , Test.test "ρ type name"
                    (\() ->
                        "ρ"
                            |> GrenParserLenient.run GrenParserLenient.nameUppercase
                            |> Expect.equal Nothing
                    )
                , Test.test "ε2 type name"
                    (\() ->
                        "ε2"
                            |> GrenParserLenient.run GrenParserLenient.nameUppercase
                            |> Expect.equal Nothing
                    )
                , Test.test "εε type name"
                    (\() ->
                        "εε"
                            |> GrenParserLenient.run GrenParserLenient.nameUppercase
                            |> Expect.equal Nothing
                    )
                , Test.test "ρ uppercase type name"
                    (\() ->
                        String.toUpper "ρ"
                            |> GrenParserLenient.run GrenParserLenient.nameUppercase
                            |> Expect.notEqual Nothing
                    )
                , Test.test "ε uppercase type name"
                    (\() ->
                        String.toUpper "ε"
                            |> GrenParserLenient.run GrenParserLenient.nameUppercase
                            |> Expect.notEqual Nothing
                    )
                ]
            , Test.describe "lowercase name"
                [ Test.test "simple latin"
                    (\() ->
                        "foo"
                            |> GrenParserLenient.run GrenParserLenient.nameLowercase
                            |> Expect.equal (Just "foo")
                    )
                , Test.test "functionName may not be a keyword"
                    (\() ->
                        "type"
                            |> GrenParserLenient.run GrenParserLenient.nameLowercase
                            |> Expect.equal Nothing
                    )
                , Test.test "functionName may be a keyword suffixed with an underscore"
                    (\() ->
                        "type_"
                            |> GrenParserLenient.run GrenParserLenient.nameLowercase
                            |> Expect.equal (Just "type_")
                    )
                , Test.test "functionName not empty"
                    (\() ->
                        ""
                            |> GrenParserLenient.run GrenParserLenient.nameLowercase
                            |> Expect.equal Nothing
                    )
                , Test.test "functionName with number"
                    (\() ->
                        "n1"
                            |> GrenParserLenient.run GrenParserLenient.nameLowercase
                            |> Expect.equal (Just "n1")
                    )
                , Test.test "alias can be a functionName (it is not reserved)"
                    (\() ->
                        "alias"
                            |> GrenParserLenient.run GrenParserLenient.nameLowercase
                            |> Expect.equal (Just "alias")
                    )
                , Test.test "infix can be a functionName (it is not reserved)"
                    (\() ->
                        "infix"
                            |> GrenParserLenient.run GrenParserLenient.nameLowercase
                            |> Expect.equal (Just "infix")
                    )
                , Test.test "functionName is not matched with 'if'"
                    (\() ->
                        "if"
                            |> GrenParserLenient.run GrenParserLenient.nameLowercase
                            |> Expect.equal Nothing
                    )
                , Test.test "functionName with _"
                    (\() ->
                        "foo_"
                            |> GrenParserLenient.run GrenParserLenient.nameLowercase
                            |> Expect.equal (Just "foo_")
                    )
                ]
            , Test.describe "exposing"
                [ Test.test "Exposing all"
                    (\() ->
                        "(..)"
                            |> expectSyntaxWithoutComments GrenParserLenient.exposing_
                                (Gren.Syntax.Exposing.All { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } })
                    )
                , Test.test "Exposing all with spacing and comment"
                    (\() ->
                        """(
  .. -- foo
  )"""
                            |> expectSyntaxWithComments GrenParserLenient.exposing_
                                { syntax = Gren.Syntax.Exposing.All { start = { row = 2, column = 3 }, end = { row = 3, column = 3 } }
                                , comments = [ Gren.Syntax.Node.Node { start = { row = 2, column = 6 }, end = { row = 2, column = 12 } } "-- foo" ]
                                }
                    )
                , Test.test "allow multi-line exposing all when closing parens is top indented"
                    (\() ->
                        """(
  ..
)"""
                            |> GrenParserLenient.run GrenParserLenient.exposing_
                            |> Maybe.map (\_ -> ())
                            |> Expect.equal (Just ())
                    )
                , Test.test "should fail to parse empty with just 1 `.`"
                    (\() ->
                        "( . )"
                            |> expectFailsToParse GrenParserLenient.exposing_
                    )
                , Test.test "should allow `...`"
                    (\() ->
                        "( ... )"
                            |> GrenParserLenient.run GrenParserLenient.exposing_
                            |> Maybe.map (\_ -> ())
                            |> Expect.equal (Just ())
                    )
                , Test.test "should fail to parse empty with 2 spaced `.`"
                    (\() ->
                        "(. .)"
                            |> expectFailsToParse GrenParserLenient.exposing_
                    )
                , Test.test "should fail to parse empty exposing list"
                    (\() ->
                        "()"
                            |> expectFailsToParse GrenParserLenient.exposing_
                    )
                , Test.test "Explicit exposing list"
                    (\() ->
                        "(Model,Msg(..),Info(..),init,(::))"
                            |> expectSyntaxWithoutComments GrenParserLenient.exposing_
                                (Gren.Syntax.Exposing.Explicit
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } (Gren.Syntax.Exposing.TypeOrAliasExpose "Model")
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 15 } }
                                        (Gren.Syntax.Exposing.TypeExpose
                                            { name = "Msg"
                                            , open = Just { start = { row = 1, column = 11 }, end = { row = 1, column = 15 } }
                                            }
                                        )
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 24 } }
                                        (Gren.Syntax.Exposing.TypeExpose
                                            { name = "Info"
                                            , open = Just { start = { row = 1, column = 20 }, end = { row = 1, column = 24 } }
                                            }
                                        )
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 25 }, end = { row = 1, column = 29 } } (Gren.Syntax.Exposing.FunctionExpose "init")
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 30 }, end = { row = 1, column = 34 } } (Gren.Syntax.Exposing.InfixExpose "::")
                                    ]
                                )
                    )
                , Test.test "exposingList with spacing on one line"
                    (\() ->
                        "(Model, Msg, Info   (..)   ,init,(::) )"
                            |> expectSyntaxWithoutComments GrenParserLenient.exposing_
                                (Gren.Syntax.Exposing.Explicit
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } (Gren.Syntax.Exposing.TypeOrAliasExpose "Model")
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } (Gren.Syntax.Exposing.TypeOrAliasExpose "Msg")
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } }
                                        (Gren.Syntax.Exposing.TypeExpose
                                            { name = "Info"
                                            , open = Just { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } }
                                            }
                                        )
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 29 }, end = { row = 1, column = 33 } } (Gren.Syntax.Exposing.FunctionExpose "init")
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 34 }, end = { row = 1, column = 38 } } (Gren.Syntax.Exposing.InfixExpose "::")
                                    ]
                                )
                    )
                , Test.test "exposingList with extra commas between exposes"
                    (\() ->
                        "(Model,,Msg,,Info   (..)  ,,init,(::) )"
                            |> expectSyntaxWithoutComments GrenParserLenient.exposing_
                                (Gren.Syntax.Exposing.Explicit
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } (Gren.Syntax.Exposing.TypeOrAliasExpose "Model")
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } (Gren.Syntax.Exposing.TypeOrAliasExpose "Msg")
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } }
                                        (Gren.Syntax.Exposing.TypeExpose
                                            { name = "Info"
                                            , open = Just { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } }
                                            }
                                        )
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 29 }, end = { row = 1, column = 33 } } (Gren.Syntax.Exposing.FunctionExpose "init")
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 34 }, end = { row = 1, column = 38 } } (Gren.Syntax.Exposing.InfixExpose "::")
                                    ]
                                )
                    )
                , Test.test "Explicit exposing list with spaces and newlines"
                    (\() ->
                        """(
      A
    , B(...)
    , Info (..)
         , init    ,
 (::)
    )"""
                            |> expectSyntaxWithoutComments GrenParserLenient.exposing_
                                (Gren.Syntax.Exposing.Explicit
                                    [ Gren.Syntax.Node.Node { start = { row = 2, column = 7 }, end = { row = 2, column = 8 } } (Gren.Syntax.Exposing.TypeOrAliasExpose "A")
                                    , Gren.Syntax.Node.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 13 } }
                                        (Gren.Syntax.Exposing.TypeExpose
                                            { name = "B"
                                            , open = Just { start = { row = 3, column = 8 }, end = { row = 3, column = 13 } }
                                            }
                                        )
                                    , Gren.Syntax.Node.Node { start = { row = 4, column = 7 }, end = { row = 4, column = 16 } }
                                        (Gren.Syntax.Exposing.TypeExpose
                                            { name = "Info"
                                            , open = Just { start = { row = 4, column = 12 }, end = { row = 4, column = 16 } }
                                            }
                                        )
                                    , Gren.Syntax.Node.Node { start = { row = 5, column = 12 }, end = { row = 5, column = 16 } } (Gren.Syntax.Exposing.FunctionExpose "init")
                                    , Gren.Syntax.Node.Node { start = { row = 6, column = 2 }, end = { row = 6, column = 6 } } (Gren.Syntax.Exposing.InfixExpose "::")
                                    ]
                                )
                    )
                , Test.test "Explicit exposing list with extra comma before first expose"
                    (\() ->
                        """(
    , A
    , B(..)
    , Info (..)
         , init    ,
 (::)
    )"""
                            |> expectSyntaxWithoutComments GrenParserLenient.exposing_
                                (Gren.Syntax.Exposing.Explicit
                                    [ Gren.Syntax.Node.Node { start = { row = 2, column = 7 }, end = { row = 2, column = 8 } } (Gren.Syntax.Exposing.TypeOrAliasExpose "A")
                                    , Gren.Syntax.Node.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 12 } }
                                        (Gren.Syntax.Exposing.TypeExpose
                                            { name = "B"
                                            , open = Just { start = { row = 3, column = 8 }, end = { row = 3, column = 12 } }
                                            }
                                        )
                                    , Gren.Syntax.Node.Node { start = { row = 4, column = 7 }, end = { row = 4, column = 16 } }
                                        (Gren.Syntax.Exposing.TypeExpose
                                            { name = "Info"
                                            , open = Just { start = { row = 4, column = 12 }, end = { row = 4, column = 16 } }
                                            }
                                        )
                                    , Gren.Syntax.Node.Node { start = { row = 5, column = 12 }, end = { row = 5, column = 16 } } (Gren.Syntax.Exposing.FunctionExpose "init")
                                    , Gren.Syntax.Node.Node { start = { row = 6, column = 2 }, end = { row = 6, column = 6 } } (Gren.Syntax.Exposing.InfixExpose "::")
                                    ]
                                )
                    )
                , Test.test "Comments inside the exposing clause"
                    (\() ->
                        "(foo\n --bar\n )"
                            |> expectSyntaxWithComments GrenParserLenient.exposing_
                                { syntax =
                                    Gren.Syntax.Exposing.Explicit
                                        [ Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 5 } }
                                            (Gren.Syntax.Exposing.FunctionExpose "foo")
                                        ]
                                , comments = [ Gren.Syntax.Node.Node { start = { row = 2, column = 2 }, end = { row = 2, column = 7 } } "--bar" ]
                                }
                    )
                ]
            , Test.test "Regression test for Incorrect range in if expression"
                (\() ->
                    parseSource
                        """module TestModule exposing (..)

a =
    if cond then
        1
    else
        2



{-| doc
-}
b = 3
"""
                        GrenParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName =
                                                Gren.Syntax.Node.Node
                                                    { start = { row = 1, column = 8 }
                                                    , end = { row = 1, column = 18 }
                                                    }
                                                    [ "TestModule" ]
                                            , exposingList =
                                                Gren.Syntax.Node.Node
                                                    { start = { row = 1, column = 19 }
                                                    , end =
                                                        { row = 1
                                                        , column = 32
                                                        }
                                                    }
                                                    (Gren.Syntax.Exposing.All { start = { row = 1, column = 29 }, end = { row = 1, column = 31 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 7, column = 10 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 7, column = 10 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 2 } } "a"
                                                    , arguments = []
                                                    , expression =
                                                        Gren.Syntax.Node.Node { start = { row = 4, column = 5 }, end = { row = 7, column = 10 } }
                                                            (Gren.Syntax.Expression.IfBlock
                                                                (Gren.Syntax.Node.Node { start = { row = 4, column = 8 }, end = { row = 4, column = 12 } }
                                                                    (Gren.Syntax.Expression.FunctionOrValue [] "cond")
                                                                )
                                                                (Gren.Syntax.Node.Node { start = { row = 5, column = 9 }, end = { row = 5, column = 10 } } (Gren.Syntax.Expression.Integer 1))
                                                                (Gren.Syntax.Node.Node
                                                                    { start =
                                                                        { row = 7
                                                                        , column = 9
                                                                        }
                                                                    , end = { row = 7, column = 10 }
                                                                    }
                                                                    (Gren.Syntax.Expression.Integer 2)
                                                                )
                                                            )
                                                    }
                                            }
                                        )
                                    , Gren.Syntax.Node.Node { start = { row = 11, column = 1 }, end = { row = 13, column = 6 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Just (Gren.Syntax.Node.Node { start = { row = 11, column = 1 }, end = { row = 12, column = 3 } } "{-| doc\n-}")
                                            , signature = Nothing
                                            , declaration =
                                                Gren.Syntax.Node.Node
                                                    { start = { row = 13, column = 1 }
                                                    , end =
                                                        { row = 13
                                                        , column = 6
                                                        }
                                                    }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 13, column = 1 }, end = { row = 13, column = 2 } } "b"
                                                    , arguments = []
                                                    , expression = Gren.Syntax.Node.Node { start = { row = 13, column = 5 }, end = { row = 13, column = 6 } } (Gren.Syntax.Expression.Integer 3)
                                                    }
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "Simple module range test"
                (\() ->
                    parseSource
                        """module TestModule exposing (..)

a =
    2



{-| doc
-}
b = 3
"""
                        GrenParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 18 } } [ "TestModule" ]
                                            , exposingList =
                                                Gren.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 32 } }
                                                    (Gren.Syntax.Exposing.All
                                                        { start = { row = 1, column = 29 }
                                                        , end = { row = 1, column = 31 }
                                                        }
                                                    )
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ Gren.Syntax.Node.Node
                                        { start = { row = 3, column = 1 }
                                        , end = { row = 4, column = 6 }
                                        }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 4, column = 6 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 2 } } "a"
                                                    , arguments = []
                                                    , expression =
                                                        Gren.Syntax.Node.Node
                                                            { start = { row = 4, column = 5 }
                                                            , end = { row = 4, column = 6 }
                                                            }
                                                            (Gren.Syntax.Expression.Integer 2)
                                                    }
                                            }
                                        )
                                    , Gren.Syntax.Node.Node
                                        { start = { row = 8, column = 1 }
                                        , end = { row = 10, column = 6 }
                                        }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Just (Gren.Syntax.Node.Node { start = { row = 8, column = 1 }, end = { row = 9, column = 3 } } "{-| doc\n-}")
                                            , signature = Nothing
                                            , declaration =
                                                Gren.Syntax.Node.Node { start = { row = 10, column = 1 }, end = { row = 10, column = 6 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 10, column = 1 }, end = { row = 10, column = 2 } } "b"
                                                    , arguments = []
                                                    , expression =
                                                        Gren.Syntax.Node.Node
                                                            { start = { row = 10, column = 5 }
                                                            , end = { row = 10, column = 6 }
                                                            }
                                                            (Gren.Syntax.Expression.Integer 3)
                                                    }
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "File with multiple imports"
                (\() ->
                    parseSource
                        """module TestModule exposing (..)
import A
import B

a = 1
"""
                        GrenParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 18 } } [ "TestModule" ]
                                            , exposingList =
                                                Gren.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 32 } }
                                                    (Gren.Syntax.Exposing.All { start = { row = 1, column = 29 }, end = { row = 1, column = 31 } })
                                            }
                                        )
                                , imports =
                                    [ Gren.Syntax.Node.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 9 } }
                                        { moduleName = Gren.Syntax.Node.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 9 } } [ "A" ]
                                        , moduleAlias = Nothing
                                        , exposingList = Nothing
                                        }
                                    , Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 9 } }
                                        { moduleName = Gren.Syntax.Node.Node { start = { row = 3, column = 8 }, end = { row = 3, column = 9 } } [ "B" ]
                                        , moduleAlias = Nothing
                                        , exposingList = Nothing
                                        }
                                    ]
                                , declarations =
                                    [ Gren.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 6 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Gren.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 6 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 2 } } "a"
                                                    , arguments = []
                                                    , expression = Gren.Syntax.Node.Node { start = { row = 5, column = 5 }, end = { row = 5, column = 6 } } (Gren.Syntax.Expression.Integer 1)
                                                    }
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "File with multiple declarations"
                (\() ->
                    parseSource
                        """module TestModule exposing (..)
type A = B | C
a = 1
type alias B = A
b : Int
b = 2
"""
                        GrenParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 18 } } [ "TestModule" ]
                                            , exposingList = Gren.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 32 } } (Gren.Syntax.Exposing.All { start = { row = 1, column = 29 }, end = { row = 1, column = 31 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ Gren.Syntax.Node.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 15 } }
                                        (GrenSyntax.CustomTypeDeclaration
                                            { documentation = Nothing
                                            , name = Gren.Syntax.Node.Node { start = { row = 2, column = 6 }, end = { row = 2, column = 7 } } "A"
                                            , generics = []
                                            , constructors =
                                                [ Gren.Syntax.Node.Node { start = { row = 2, column = 10 }, end = { row = 2, column = 11 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 2, column = 10 }, end = { row = 2, column = 11 } } "B"
                                                    , arguments = []
                                                    }
                                                , Gren.Syntax.Node.Node { start = { row = 2, column = 14 }, end = { row = 2, column = 15 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 2, column = 14 }, end = { row = 2, column = 15 } } "C"
                                                    , arguments = []
                                                    }
                                                ]
                                            }
                                        )
                                    , Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 6 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 6 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 2 } } "a"
                                                    , arguments = []
                                                    , expression = Gren.Syntax.Node.Node { start = { row = 3, column = 5 }, end = { row = 3, column = 6 } } (Gren.Syntax.Expression.Integer 1)
                                                    }
                                            }
                                        )
                                    , Gren.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 17 } }
                                        (GrenSyntax.AliasDeclaration
                                            { documentation = Nothing
                                            , name = Gren.Syntax.Node.Node { start = { row = 4, column = 12 }, end = { row = 4, column = 13 } } "B"
                                            , generics = []
                                            , typeAnnotation =
                                                Gren.Syntax.Node.Node { start = { row = 4, column = 16 }, end = { row = 4, column = 17 } }
                                                    (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 4, column = 16 }, end = { row = 4, column = 17 } } ( [], "A" )) [])
                                            }
                                        )
                                    , Gren.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 6, column = 6 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature =
                                                Just
                                                    (Gren.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 8 } }
                                                        { name = Gren.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 2 } } "b"
                                                        , typeAnnotation = Gren.Syntax.Node.Node { start = { row = 5, column = 5 }, end = { row = 5, column = 8 } } (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 5, column = 5 }, end = { row = 5, column = 8 } } ( [], "Int" )) [])
                                                        }
                                                    )
                                            , declaration =
                                                Gren.Syntax.Node.Node { start = { row = 6, column = 1 }, end = { row = 6, column = 6 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 6, column = 1 }, end = { row = 6, column = 2 } } "b"
                                                    , arguments = []
                                                    , expression = Gren.Syntax.Node.Node { start = { row = 6, column = 5 }, end = { row = 6, column = 6 } } (Gren.Syntax.Expression.Integer 2)
                                                    }
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "should fail to parse two signatures in a row"
                (\() ->
                    """module TestModule exposing (..)
a : Int
b : Int
b = 2
"""
                        |> moduleExpectInvalid
                )
            , Test.test "should fail to parse signature for a different function"
                (\() ->
                    """module TestModule exposing (..)
a : Int
b = 2
"""
                        |> moduleExpectInvalid
                )
            , Test.test "trailing comments at the end of declarations"
                (\() ->
                    """module A exposing (fun1, fun2)

fun1 n =
  fun2 n
  + fun2 n  -- a

fun2 n =
  fun1 n    -- b
"""
                        |> GrenParserLenient.run GrenParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition = Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 31 } } (GrenSyntax.NormalModule { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } } [ "A" ], exposingList = Gren.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 31 } } (Gren.Syntax.Exposing.Explicit [ Gren.Syntax.Node.Node { start = { row = 1, column = 20 }, end = { row = 1, column = 24 } } (Gren.Syntax.Exposing.FunctionExpose "fun1"), Gren.Syntax.Node.Node { start = { row = 1, column = 26 }, end = { row = 1, column = 30 } } (Gren.Syntax.Exposing.FunctionExpose "fun2") ]) })
                                , imports = []
                                , declarations =
                                    [ Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 5, column = 11 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 5, column = 11 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } } "fun1"
                                                    , arguments = [ Gren.Syntax.Node.Node { start = { row = 3, column = 6 }, end = { row = 3, column = 7 } } (Gren.Syntax.Pattern.VarPattern "n") ]
                                                    , expression =
                                                        Gren.Syntax.Node.Node { start = { row = 4, column = 3 }, end = { row = 5, column = 11 } }
                                                            (Gren.Syntax.Expression.OperatorApplication "+"
                                                                (Gren.Syntax.Node.Node { start = { row = 4, column = 3 }, end = { row = 4, column = 9 } }
                                                                    (Gren.Syntax.Expression.Application
                                                                        [ Gren.Syntax.Node.Node { start = { row = 4, column = 3 }, end = { row = 4, column = 7 } } (Gren.Syntax.Expression.FunctionOrValue [] "fun2")
                                                                        , Gren.Syntax.Node.Node { start = { row = 4, column = 8 }, end = { row = 4, column = 9 } } (Gren.Syntax.Expression.FunctionOrValue [] "n")
                                                                        ]
                                                                    )
                                                                )
                                                                (Gren.Syntax.Node.Node { start = { row = 5, column = 5 }, end = { row = 5, column = 11 } }
                                                                    (Gren.Syntax.Expression.Application
                                                                        [ Gren.Syntax.Node.Node { start = { row = 5, column = 5 }, end = { row = 5, column = 9 } } (Gren.Syntax.Expression.FunctionOrValue [] "fun2")
                                                                        , Gren.Syntax.Node.Node { start = { row = 5, column = 10 }, end = { row = 5, column = 11 } } (Gren.Syntax.Expression.FunctionOrValue [] "n")
                                                                        ]
                                                                    )
                                                                )
                                                            )
                                                    }
                                            }
                                        )
                                    , Gren.Syntax.Node.Node { start = { row = 7, column = 1 }, end = { row = 8, column = 9 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Gren.Syntax.Node.Node { start = { row = 7, column = 1 }, end = { row = 8, column = 9 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 7, column = 1 }, end = { row = 7, column = 5 } } "fun2"
                                                    , arguments = [ Gren.Syntax.Node.Node { start = { row = 7, column = 6 }, end = { row = 7, column = 7 } } (Gren.Syntax.Pattern.VarPattern "n") ]
                                                    , expression =
                                                        Gren.Syntax.Node.Node { start = { row = 8, column = 3 }, end = { row = 8, column = 9 } }
                                                            (Gren.Syntax.Expression.Application
                                                                [ Gren.Syntax.Node.Node { start = { row = 8, column = 3 }, end = { row = 8, column = 7 } } (Gren.Syntax.Expression.FunctionOrValue [] "fun1")
                                                                , Gren.Syntax.Node.Node { start = { row = 8, column = 8 }, end = { row = 8, column = 9 } } (Gren.Syntax.Expression.FunctionOrValue [] "n")
                                                                ]
                                                            )
                                                    }
                                            }
                                        )
                                    ]
                                , comments = [ Gren.Syntax.Node.Node { start = { row = 5, column = 13 }, end = { row = 5, column = 17 } } "-- a", Gren.Syntax.Node.Node { start = { row = 8, column = 13 }, end = { row = 8, column = 17 } } "-- b" ]
                                }
                            )
                )
            , Test.test "import between declarations, no existing import"
                (\() ->
                    """module A exposing (fun1, fun2)

fun1 n =
  fun2 n
import B

fun2 n =
  fun1 n
"""
                        |> GrenParserLenient.run GrenParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition = Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 31 } } (GrenSyntax.NormalModule { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } } [ "A" ], exposingList = Gren.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 31 } } (Gren.Syntax.Exposing.Explicit [ Gren.Syntax.Node.Node { start = { row = 1, column = 20 }, end = { row = 1, column = 24 } } (Gren.Syntax.Exposing.FunctionExpose "fun1"), Gren.Syntax.Node.Node { start = { row = 1, column = 26 }, end = { row = 1, column = 30 } } (Gren.Syntax.Exposing.FunctionExpose "fun2") ]) })
                                , imports =
                                    [ Gren.Syntax.Node.Node { end = { column = 1, row = 3 }, start = { column = 1, row = 3 } }
                                        { exposingList = Nothing, moduleAlias = Nothing, moduleName = Gren.Syntax.Node.Node { end = { column = 9, row = 5 }, start = { column = 8, row = 5 } } [ "B" ] }
                                    ]
                                , declarations =
                                    [ Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 4, column = 9 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 4, column = 9 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } } "fun1"
                                                    , arguments = [ Gren.Syntax.Node.Node { start = { row = 3, column = 6 }, end = { row = 3, column = 7 } } (Gren.Syntax.Pattern.VarPattern "n") ]
                                                    , expression =
                                                        Gren.Syntax.Node.Node { start = { row = 4, column = 3 }, end = { row = 4, column = 9 } }
                                                            (Gren.Syntax.Expression.Application
                                                                [ Gren.Syntax.Node.Node { start = { row = 4, column = 3 }, end = { row = 4, column = 7 } } (Gren.Syntax.Expression.FunctionOrValue [] "fun2")
                                                                , Gren.Syntax.Node.Node { start = { row = 4, column = 8 }, end = { row = 4, column = 9 } } (Gren.Syntax.Expression.FunctionOrValue [] "n")
                                                                ]
                                                            )
                                                    }
                                            }
                                        )
                                    , Gren.Syntax.Node.Node { start = { row = 7, column = 1 }, end = { row = 8, column = 9 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Gren.Syntax.Node.Node { start = { row = 7, column = 1 }, end = { row = 8, column = 9 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 7, column = 1 }, end = { row = 7, column = 5 } } "fun2"
                                                    , arguments = [ Gren.Syntax.Node.Node { start = { row = 7, column = 6 }, end = { row = 7, column = 7 } } (Gren.Syntax.Pattern.VarPattern "n") ]
                                                    , expression =
                                                        Gren.Syntax.Node.Node { start = { row = 8, column = 3 }, end = { row = 8, column = 9 } }
                                                            (Gren.Syntax.Expression.Application
                                                                [ Gren.Syntax.Node.Node { start = { row = 8, column = 3 }, end = { row = 8, column = 7 } } (Gren.Syntax.Expression.FunctionOrValue [] "fun1")
                                                                , Gren.Syntax.Node.Node { start = { row = 8, column = 8 }, end = { row = 8, column = 9 } } (Gren.Syntax.Expression.FunctionOrValue [] "n")
                                                                ]
                                                            )
                                                    }
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "import between declarations, existing import"
                (\() ->
                    """module A exposing (fun1, fun2)
import C
fun1 n =
  fun2 n
import B

fun2 n =
  fun1 n
"""
                        |> GrenParserLenient.run GrenParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition = Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 31 } } (GrenSyntax.NormalModule { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } } [ "A" ], exposingList = Gren.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 31 } } (Gren.Syntax.Exposing.Explicit [ Gren.Syntax.Node.Node { start = { row = 1, column = 20 }, end = { row = 1, column = 24 } } (Gren.Syntax.Exposing.FunctionExpose "fun1"), Gren.Syntax.Node.Node { start = { row = 1, column = 26 }, end = { row = 1, column = 30 } } (Gren.Syntax.Exposing.FunctionExpose "fun2") ]) })
                                , imports =
                                    [ Gren.Syntax.Node.Node { end = { column = 1, row = 2 }, start = { column = 1, row = 2 } }
                                        { exposingList = Nothing, moduleAlias = Nothing, moduleName = Gren.Syntax.Node.Node { end = { column = 9, row = 5 }, start = { column = 8, row = 5 } } [ "B" ] }
                                    , Gren.Syntax.Node.Node { end = { column = 9, row = 2 }, start = { column = 1, row = 2 } }
                                        { exposingList = Nothing, moduleAlias = Nothing, moduleName = Gren.Syntax.Node.Node { end = { column = 9, row = 2 }, start = { column = 8, row = 2 } } [ "C" ] }
                                    ]
                                , declarations =
                                    [ Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 4, column = 9 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 4, column = 9 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } } "fun1"
                                                    , arguments = [ Gren.Syntax.Node.Node { start = { row = 3, column = 6 }, end = { row = 3, column = 7 } } (Gren.Syntax.Pattern.VarPattern "n") ]
                                                    , expression =
                                                        Gren.Syntax.Node.Node { start = { row = 4, column = 3 }, end = { row = 4, column = 9 } }
                                                            (Gren.Syntax.Expression.Application
                                                                [ Gren.Syntax.Node.Node { start = { row = 4, column = 3 }, end = { row = 4, column = 7 } } (Gren.Syntax.Expression.FunctionOrValue [] "fun2")
                                                                , Gren.Syntax.Node.Node { start = { row = 4, column = 8 }, end = { row = 4, column = 9 } } (Gren.Syntax.Expression.FunctionOrValue [] "n")
                                                                ]
                                                            )
                                                    }
                                            }
                                        )
                                    , Gren.Syntax.Node.Node { start = { row = 7, column = 1 }, end = { row = 8, column = 9 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Gren.Syntax.Node.Node { start = { row = 7, column = 1 }, end = { row = 8, column = 9 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 7, column = 1 }, end = { row = 7, column = 5 } } "fun2"
                                                    , arguments = [ Gren.Syntax.Node.Node { start = { row = 7, column = 6 }, end = { row = 7, column = 7 } } (Gren.Syntax.Pattern.VarPattern "n") ]
                                                    , expression =
                                                        Gren.Syntax.Node.Node { start = { row = 8, column = 3 }, end = { row = 8, column = 9 } }
                                                            (Gren.Syntax.Expression.Application
                                                                [ Gren.Syntax.Node.Node { start = { row = 8, column = 3 }, end = { row = 8, column = 7 } } (Gren.Syntax.Expression.FunctionOrValue [] "fun1")
                                                                , Gren.Syntax.Node.Node { start = { row = 8, column = 8 }, end = { row = 8, column = 9 } } (Gren.Syntax.Expression.FunctionOrValue [] "n")
                                                                ]
                                                            )
                                                    }
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            ]
        , Test.describe "import"
            [ Test.test "import with explicits"
                (\() ->
                    "import Foo exposing (Model, Msg(..))"
                        |> expectSyntaxWithoutComments GrenParserLenient.import_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 37 } }
                                { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Foo" ]
                                , moduleAlias = Nothing
                                , exposingList =
                                    Just
                                        (Gren.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 37 } }
                                            (Gren.Syntax.Exposing.Explicit
                                                [ Gren.Syntax.Node.Node { start = { row = 1, column = 22 }, end = { row = 1, column = 27 } } (Gren.Syntax.Exposing.TypeOrAliasExpose "Model"), Gren.Syntax.Node.Node { start = { row = 1, column = 29 }, end = { row = 1, column = 36 } } (Gren.Syntax.Exposing.TypeExpose { name = "Msg", open = Just { start = { row = 1, column = 32 }, end = { row = 1, column = 36 } } }) ]
                                            )
                                        )
                                }
                            )
                )
            , Test.test "import with explicits 2"
                (\() ->
                    "import Html exposing (text)"
                        |> expectSyntaxWithoutComments GrenParserLenient.import_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 28 } }
                                { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 12 } } [ "Html" ]
                                , moduleAlias = Nothing
                                , exposingList =
                                    Just
                                        (Gren.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 28 } }
                                            (Gren.Syntax.Exposing.Explicit
                                                [ Gren.Syntax.Node.Node { start = { row = 1, column = 23 }, end = { row = 1, column = 27 } } (Gren.Syntax.Exposing.FunctionExpose "text") ]
                                            )
                                        )
                                }
                            )
                )
            , Test.test "import with exposing ()"
                (\() ->
                    "import Html exposing ()"
                        |> expectSyntaxWithoutComments GrenParserLenient.import_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 12 } } [ "Html" ]
                                , moduleAlias = Nothing
                                , exposingList = Nothing
                                }
                            )
                )
            , Test.test "import with alias and exposing ()"
                (\() ->
                    "import Html as H exposing ()"
                        |> expectSyntaxWithoutComments GrenParserLenient.import_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 17 } }
                                { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 12 } } [ "Html" ]
                                , moduleAlias = Just (Gren.Syntax.Node.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 17 } } [ "H" ])
                                , exposingList = Nothing
                                }
                            )
                )
            , Test.test "import minimal"
                (\() ->
                    "import Foo"
                        |> expectSyntaxWithoutComments GrenParserLenient.import_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                                { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Foo" ]
                                , moduleAlias = Nothing
                                , exposingList = Nothing
                                }
                            )
                )
            , Test.test "import with alias"
                (\() ->
                    "import Foo as Bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.import_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 18 } }
                                { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Foo" ]
                                , moduleAlias = Just (Gren.Syntax.Node.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 18 } } [ "Bar" ])
                                , exposingList = Nothing
                                }
                            )
                )
            , Test.test "import with alias and exposing .."
                (\() ->
                    "import Foo as Bar exposing (..)"
                        |> expectSyntaxWithoutComments GrenParserLenient.import_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                                { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Foo" ]
                                , moduleAlias = Just (Gren.Syntax.Node.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 18 } } [ "Bar" ])
                                , exposingList =
                                    Just
                                        (Gren.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 32 } }
                                            (Gren.Syntax.Exposing.All { start = { row = 1, column = 29 }, end = { row = 1, column = 31 } })
                                        )
                                }
                            )
                )
            , Test.test "import with alias and exposing ..."
                (\() ->
                    "import Foo as Bar exposing (...)"
                        |> expectSyntaxWithoutComments GrenParserLenient.import_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 33 } }
                                { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Foo" ]
                                , moduleAlias = Just (Gren.Syntax.Node.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 18 } } [ "Bar" ])
                                , exposingList =
                                    Just
                                        (Gren.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 33 } }
                                            (Gren.Syntax.Exposing.All { start = { row = 1, column = 29 }, end = { row = 1, column = 32 } })
                                        )
                                }
                            )
                )
            , Test.test "import with invalid alias containing ."
                (\() ->
                    "import Foo as Bar.Buzz"
                        |> expectFailsToParse GrenParserLenient.import_
                )
            ]
        , Test.test "Comments ordering"
            (\() ->
                let
                    input : String
                    input =
                        """
module Foo exposing (..)

{-| Module documentation
-}

import A

-- 1
{- 2 -}
-- 3

{-| Function declaration
-}
f =
    -- 4
    identity

-- 5
{- 6 -}
"""
                in
                GrenParserLenient.run GrenParserLenient.module_ input
                    |> Maybe.map .comments
                    |> Expect.equal
                        (Just
                            [ Gren.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 5, column = 3 } } "{-| Module documentation\n-}"
                            , Gren.Syntax.Node.Node { start = { row = 9, column = 1 }, end = { row = 9, column = 5 } } "-- 1"
                            , Gren.Syntax.Node.Node { start = { row = 10, column = 1 }, end = { row = 10, column = 8 } } "{- 2 -}"
                            , Gren.Syntax.Node.Node { start = { row = 11, column = 1 }, end = { row = 11, column = 5 } } "-- 3"
                            , Gren.Syntax.Node.Node { start = { row = 16, column = 5 }, end = { row = 16, column = 9 } } "-- 4"
                            , Gren.Syntax.Node.Node { start = { row = 19, column = 1 }, end = { row = 19, column = 5 } } "-- 5"
                            , Gren.Syntax.Node.Node { start = { row = 20, column = 1 }, end = { row = 20, column = 8 } } "{- 6 -}"
                            ]
                        )
            )
        , Test.test "declarations with comments"
            (\() ->
                """module Foo exposing (b, fn)

fn a =
    case a of
        X ->
            1
                -- 1
                + 2

        Y ->
            1

-- 2

b =
    1

"""
                    |> GrenParserLenient.run GrenParserLenient.module_
                    |> Maybe.map .comments
                    |> Expect.equal
                        (Just
                            [ Gren.Syntax.Node.Node { start = { row = 7, column = 17 }, end = { row = 7, column = 21 } } "-- 1"
                            , Gren.Syntax.Node.Node { start = { row = 13, column = 1 }, end = { row = 13, column = 5 } } "-- 2"
                            ]
                        )
            )
        , Test.test "function declaration with a case and trailing whitespace"
            (\() ->
                """
module Trailing.Whitespace exposing (..)

caseWhitespace f = case f   of
  True     -> 
    1   
  False   
     -> 
     
     
         2    

   --some comment

    
    """
                    |> GrenParserLenient.run GrenParserLenient.module_
                    |> Expect.equal
                        (Just
                            { moduleDefinition =
                                Gren.Syntax.Node.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 41 } }
                                    (GrenSyntax.NormalModule
                                        { moduleName = Gren.Syntax.Node.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 27 } } [ "Trailing", "Whitespace" ]
                                        , exposingList =
                                            Gren.Syntax.Node.Node { start = { row = 2, column = 28 }, end = { row = 2, column = 41 } }
                                                (Gren.Syntax.Exposing.All { start = { row = 2, column = 38 }, end = { row = 2, column = 40 } })
                                        }
                                    )
                            , imports = []
                            , declarations =
                                [ Gren.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 11, column = 11 } }
                                    (GrenSyntax.FunctionDeclaration
                                        { documentation = Nothing
                                        , signature = Nothing
                                        , declaration =
                                            Gren.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 11, column = 11 } }
                                                { name = Gren.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 15 } } "caseWhitespace"
                                                , arguments = [ Gren.Syntax.Node.Node { start = { row = 4, column = 16 }, end = { row = 4, column = 17 } } (Gren.Syntax.Pattern.VarPattern "f") ]
                                                , expression =
                                                    Gren.Syntax.Node.Node { start = { row = 4, column = 20 }, end = { row = 11, column = 11 } }
                                                        (Gren.Syntax.Expression.CaseExpression
                                                            { expression = Gren.Syntax.Node.Node { start = { row = 4, column = 25 }, end = { row = 4, column = 26 } } (Gren.Syntax.Expression.FunctionOrValue [] "f")
                                                            , cases =
                                                                [ ( Gren.Syntax.Node.Node { start = { row = 5, column = 3 }, end = { row = 5, column = 7 } } (Gren.Syntax.Pattern.NamedPattern { moduleName = [], name = "True" } [])
                                                                  , Gren.Syntax.Node.Node { start = { row = 6, column = 5 }, end = { row = 6, column = 6 } } (Gren.Syntax.Expression.Integer 1)
                                                                  )
                                                                , ( Gren.Syntax.Node.Node { start = { row = 7, column = 3 }, end = { row = 7, column = 8 } } (Gren.Syntax.Pattern.NamedPattern { moduleName = [], name = "False" } [])
                                                                  , Gren.Syntax.Node.Node { start = { row = 11, column = 10 }, end = { row = 11, column = 11 } } (Gren.Syntax.Expression.Integer 2)
                                                                  )
                                                                ]
                                                            }
                                                        )
                                                }
                                        }
                                    )
                                ]
                            , comments = [ Gren.Syntax.Node.Node { start = { row = 13, column = 4 }, end = { row = 13, column = 18 } } "--some comment" ]
                            }
                        )
            )
        , Test.test "function declaration with lambda and trailing whitespace"
            (\() ->
                """
module Trailing.Whitespace exposing (..)

lambdaWhitespace =   \\ a b ->    a    

       + 

    b 


--some comment

    """
                    |> GrenParserLenient.run GrenParserLenient.module_
                    |> Expect.equal
                        (Just
                            { moduleDefinition =
                                Gren.Syntax.Node.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 41 } }
                                    (GrenSyntax.NormalModule
                                        { moduleName = Gren.Syntax.Node.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 27 } } [ "Trailing", "Whitespace" ]
                                        , exposingList =
                                            Gren.Syntax.Node.Node { start = { row = 2, column = 28 }, end = { row = 2, column = 41 } }
                                                (Gren.Syntax.Exposing.All { start = { row = 2, column = 38 }, end = { row = 2, column = 40 } })
                                        }
                                    )
                            , imports = []
                            , declarations =
                                [ Gren.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 8, column = 6 } }
                                    (GrenSyntax.FunctionDeclaration
                                        { documentation = Nothing
                                        , signature = Nothing
                                        , declaration =
                                            Gren.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 8, column = 6 } }
                                                { name = Gren.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 17 } } "lambdaWhitespace"
                                                , arguments = []
                                                , expression =
                                                    Gren.Syntax.Node.Node { start = { row = 4, column = 22 }, end = { row = 8, column = 6 } }
                                                        (Gren.Syntax.Expression.LambdaExpression
                                                            { args =
                                                                [ Gren.Syntax.Node.Node { start = { row = 4, column = 24 }, end = { row = 4, column = 25 } } (Gren.Syntax.Pattern.VarPattern "a")
                                                                , Gren.Syntax.Node.Node { start = { row = 4, column = 26 }, end = { row = 4, column = 27 } } (Gren.Syntax.Pattern.VarPattern "b")
                                                                ]
                                                            , expression =
                                                                Gren.Syntax.Node.Node { start = { row = 4, column = 34 }, end = { row = 8, column = 6 } }
                                                                    (Gren.Syntax.Expression.OperatorApplication "+"
                                                                        (Gren.Syntax.Node.Node { start = { row = 4, column = 34 }, end = { row = 4, column = 35 } } (Gren.Syntax.Expression.FunctionOrValue [] "a"))
                                                                        (Gren.Syntax.Node.Node { start = { row = 8, column = 5 }, end = { row = 8, column = 6 } } (Gren.Syntax.Expression.FunctionOrValue [] "b"))
                                                                    )
                                                            }
                                                        )
                                                }
                                        }
                                    )
                                ]
                            , comments = [ Gren.Syntax.Node.Node { start = { row = 11, column = 1 }, end = { row = 11, column = 15 } } "--some comment" ]
                            }
                        )
            )
        , Test.test "function declaration with let and trailing whitespace"
            (\() ->
                """
module Trailing.Whitespace exposing (..)

letWhitespace = let
                  b =   1

 in
 b


--some comment

    """
                    |> GrenParserLenient.run GrenParserLenient.module_
                    |> Expect.equal
                        (Just
                            { moduleDefinition =
                                Gren.Syntax.Node.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 41 } }
                                    (GrenSyntax.NormalModule
                                        { moduleName = Gren.Syntax.Node.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 27 } } [ "Trailing", "Whitespace" ]
                                        , exposingList =
                                            Gren.Syntax.Node.Node { start = { row = 2, column = 28 }, end = { row = 2, column = 41 } }
                                                (Gren.Syntax.Exposing.All { start = { row = 2, column = 38 }, end = { row = 2, column = 40 } })
                                        }
                                    )
                            , imports = []
                            , declarations =
                                [ Gren.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 8, column = 3 } }
                                    (GrenSyntax.FunctionDeclaration
                                        { documentation = Nothing
                                        , signature = Nothing
                                        , declaration =
                                            Gren.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 8, column = 3 } }
                                                { name = Gren.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 14 } } "letWhitespace"
                                                , arguments = []
                                                , expression =
                                                    Gren.Syntax.Node.Node { start = { row = 4, column = 17 }, end = { row = 8, column = 3 } }
                                                        (Gren.Syntax.Expression.LetExpression
                                                            { declarations =
                                                                [ Gren.Syntax.Node.Node { start = { row = 5, column = 19 }, end = { row = 5, column = 26 } }
                                                                    (Gren.Syntax.Expression.LetFunction
                                                                        { documentation = Nothing
                                                                        , signature = Nothing
                                                                        , declaration =
                                                                            Gren.Syntax.Node.Node { start = { row = 5, column = 19 }, end = { row = 5, column = 26 } }
                                                                                { name = Gren.Syntax.Node.Node { start = { row = 5, column = 19 }, end = { row = 5, column = 20 } } "b"
                                                                                , arguments = []
                                                                                , expression = Gren.Syntax.Node.Node { start = { row = 5, column = 25 }, end = { row = 5, column = 26 } } (Gren.Syntax.Expression.Integer 1)
                                                                                }
                                                                        }
                                                                    )
                                                                ]
                                                            , expression = Gren.Syntax.Node.Node { start = { row = 8, column = 2 }, end = { row = 8, column = 3 } } (Gren.Syntax.Expression.FunctionOrValue [] "b")
                                                            }
                                                        )
                                                }
                                        }
                                    )
                                ]
                            , comments = [ Gren.Syntax.Node.Node { start = { row = 11, column = 1 }, end = { row = 11, column = 15 } } "--some comment" ]
                            }
                        )
            )
        , Test.test "type declaration with documentation after imports"
            (\() ->
                """
module Foo exposing (..)

import Dict

{-| Config goes here
-}
type Configuration
    = Configuration
"""
                    |> GrenParserLenient.run GrenParserLenient.module_
                    |> Expect.equal
                        (Just
                            { moduleDefinition =
                                Gren.Syntax.Node.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 25 } }
                                    (GrenSyntax.NormalModule
                                        { moduleName = Gren.Syntax.Node.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } [ "Foo" ]
                                        , exposingList =
                                            Gren.Syntax.Node.Node { start = { row = 2, column = 12 }, end = { row = 2, column = 25 } }
                                                (Gren.Syntax.Exposing.All { start = { row = 2, column = 22 }, end = { row = 2, column = 24 } })
                                        }
                                    )
                            , imports =
                                [ Gren.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 12 } }
                                    { moduleName = Gren.Syntax.Node.Node { start = { row = 4, column = 8 }, end = { row = 4, column = 12 } } [ "Dict" ]
                                    , moduleAlias = Nothing
                                    , exposingList = Nothing
                                    }
                                ]
                            , declarations =
                                [ Gren.Syntax.Node.Node { start = { row = 6, column = 1 }, end = { row = 9, column = 20 } }
                                    (GrenSyntax.CustomTypeDeclaration
                                        { documentation = Just (Gren.Syntax.Node.Node { start = { row = 6, column = 1 }, end = { row = 7, column = 3 } } "{-| Config goes here\n-}")
                                        , name = Gren.Syntax.Node.Node { start = { row = 8, column = 6 }, end = { row = 8, column = 19 } } "Configuration"
                                        , generics = []
                                        , constructors =
                                            [ Gren.Syntax.Node.Node { start = { row = 9, column = 7 }, end = { row = 9, column = 20 } }
                                                { name = Gren.Syntax.Node.Node { start = { row = 9, column = 7 }, end = { row = 9, column = 20 } } "Configuration"
                                                , arguments = []
                                                }
                                            ]
                                        }
                                    )
                                ]
                            , comments = []
                            }
                        )
            )
        , Test.test "module documentation formatted like a type documentation"
            (\() ->
                """
module Foo exposing (..)

{-| actually module doc
-}
type Configuration
    = Configuration
"""
                    |> GrenParserLenient.run GrenParserLenient.module_
                    |> Expect.equal
                        (Just
                            { moduleDefinition =
                                Gren.Syntax.Node.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 25 } }
                                    (GrenSyntax.NormalModule
                                        { moduleName = Gren.Syntax.Node.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } [ "Foo" ]
                                        , exposingList =
                                            Gren.Syntax.Node.Node { start = { row = 2, column = 12 }, end = { row = 2, column = 25 } }
                                                (Gren.Syntax.Exposing.All { start = { row = 2, column = 22 }, end = { row = 2, column = 24 } })
                                        }
                                    )
                            , imports = []
                            , declarations =
                                [ Gren.Syntax.Node.Node { start = { row = 6, column = 1 }, end = { row = 7, column = 20 } }
                                    (GrenSyntax.CustomTypeDeclaration
                                        { documentation = Nothing
                                        , name = Gren.Syntax.Node.Node { start = { row = 6, column = 6 }, end = { row = 6, column = 19 } } "Configuration"
                                        , generics = []
                                        , constructors =
                                            [ Gren.Syntax.Node.Node { start = { row = 7, column = 7 }, end = { row = 7, column = 20 } }
                                                { name = Gren.Syntax.Node.Node { start = { row = 7, column = 7 }, end = { row = 7, column = 20 } } "Configuration"
                                                , arguments = []
                                                }
                                            ]
                                        }
                                    )
                                ]
                            , comments = [ Gren.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 5, column = 3 } } "{-| actually module doc\n-}" ]
                            }
                        )
            )
        , Test.test "documentation on a port declaration is treated as a normal comment"
            (\() ->
                """
port module Foo exposing (..)

import String

{-| foo
-}
port sendResponse : String -> Cmd msg
"""
                    |> GrenParserLenient.run GrenParserLenient.module_
                    |> Expect.equal
                        (Just
                            { moduleDefinition =
                                Gren.Syntax.Node.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 30 } }
                                    (GrenSyntax.PortModule
                                        { moduleName = Gren.Syntax.Node.Node { start = { row = 2, column = 13 }, end = { row = 2, column = 16 } } [ "Foo" ]
                                        , exposingList =
                                            Gren.Syntax.Node.Node { start = { row = 2, column = 17 }, end = { row = 2, column = 30 } }
                                                (Gren.Syntax.Exposing.All { start = { row = 2, column = 27 }, end = { row = 2, column = 29 } })
                                        }
                                    )
                            , imports =
                                [ Gren.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 14 } }
                                    { moduleName = Gren.Syntax.Node.Node { start = { row = 4, column = 8 }, end = { row = 4, column = 14 } } [ "String" ]
                                    , moduleAlias = Nothing
                                    , exposingList = Nothing
                                    }
                                ]
                            , declarations =
                                [ Gren.Syntax.Node.Node { start = { row = 8, column = 1 }, end = { row = 8, column = 38 } }
                                    (GrenSyntax.PortDeclaration
                                        { name = Gren.Syntax.Node.Node { start = { row = 8, column = 6 }, end = { row = 8, column = 18 } } "sendResponse"
                                        , typeAnnotation =
                                            Gren.Syntax.Node.Node { start = { row = 8, column = 21 }, end = { row = 8, column = 38 } }
                                                (Gren.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                                    (Gren.Syntax.Node.Node { start = { row = 8, column = 21 }, end = { row = 8, column = 27 } }
                                                        (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 8, column = 21 }, end = { row = 8, column = 27 } } ( [], "String" )) [])
                                                    )
                                                    (Gren.Syntax.Node.Node { start = { row = 8, column = 31 }, end = { row = 8, column = 38 } }
                                                        (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 8, column = 31 }, end = { row = 8, column = 34 } } ( [], "Cmd" ))
                                                            [ Gren.Syntax.Node.Node { start = { row = 8, column = 35 }, end = { row = 8, column = 38 } } (Gren.Syntax.TypeAnnotation.GenericType "msg") ]
                                                        )
                                                    )
                                                )
                                        }
                                    )
                                ]
                            , comments = [ Gren.Syntax.Node.Node { start = { row = 6, column = 1 }, end = { row = 7, column = 3 } } "{-| foo\n-}" ]
                            }
                        )
            )
        , Test.test "port module without port gets parsed to normal module"
            (\() ->
                """
port module Foo exposing (..)

sendResponse =
    Cmd.none
"""
                    |> GrenParserLenient.run GrenParserLenient.module_
                    |> Expect.equal
                        (Just
                            { comments = []
                            , declarations =
                                [ Gren.Syntax.Node.Node { end = { column = 13, row = 5 }, start = { column = 1, row = 4 } }
                                    (GrenSyntax.FunctionDeclaration
                                        { declaration =
                                            Gren.Syntax.Node.Node { end = { column = 13, row = 5 }, start = { column = 1, row = 4 } }
                                                { arguments = []
                                                , expression =
                                                    Gren.Syntax.Node.Node { end = { column = 13, row = 5 }, start = { column = 5, row = 5 } }
                                                        (Gren.Syntax.Expression.FunctionOrValue [ "Cmd" ] "none")
                                                , name = Gren.Syntax.Node.Node { end = { column = 13, row = 4 }, start = { column = 1, row = 4 } } "sendResponse"
                                                }
                                        , documentation = Nothing
                                        , signature = Nothing
                                        }
                                    )
                                ]
                            , imports = []
                            , moduleDefinition =
                                Gren.Syntax.Node.Node { end = { column = 30, row = 2 }, start = { column = 1, row = 2 } }
                                    (GrenSyntax.NormalModule
                                        { exposingList =
                                            Gren.Syntax.Node.Node { end = { column = 30, row = 2 }, start = { column = 17, row = 2 } }
                                                (Gren.Syntax.Exposing.All { end = { column = 29, row = 2 }, start = { column = 27, row = 2 } })
                                        , moduleName = Gren.Syntax.Node.Node { end = { column = 16, row = 2 }, start = { column = 13, row = 2 } } [ "Foo" ]
                                        }
                                    )
                            }
                        )
            )
        , Test.test "normal module with port gets parsed to port module"
            (\() ->
                """
module Foo exposing (..)

port sendResponse : String -> Cmd msg
"""
                    |> GrenParserLenient.run GrenParserLenient.module_
                    |> Expect.equal
                        (Just
                            { comments = []
                            , declarations =
                                [ Gren.Syntax.Node.Node { end = { column = 38, row = 4 }, start = { column = 1, row = 4 } }
                                    (GrenSyntax.PortDeclaration
                                        { name = Gren.Syntax.Node.Node { end = { column = 18, row = 4 }, start = { column = 6, row = 4 } } "sendResponse"
                                        , typeAnnotation =
                                            Gren.Syntax.Node.Node { end = { column = 38, row = 4 }, start = { column = 21, row = 4 } }
                                                (Gren.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                                    (Gren.Syntax.Node.Node { end = { column = 27, row = 4 }, start = { column = 21, row = 4 } }
                                                        (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { end = { column = 27, row = 4 }, start = { column = 21, row = 4 } } ( [], "String" )) [])
                                                    )
                                                    (Gren.Syntax.Node.Node { end = { column = 38, row = 4 }, start = { column = 31, row = 4 } }
                                                        (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { end = { column = 34, row = 4 }, start = { column = 31, row = 4 } } ( [], "Cmd" ))
                                                            [ Gren.Syntax.Node.Node { end = { column = 38, row = 4 }, start = { column = 35, row = 4 } }
                                                                (Gren.Syntax.TypeAnnotation.GenericType "msg")
                                                            ]
                                                        )
                                                    )
                                                )
                                        }
                                    )
                                ]
                            , imports = []
                            , moduleDefinition =
                                Gren.Syntax.Node.Node { end = { column = 25, row = 2 }, start = { column = 1, row = 2 } }
                                    (GrenSyntax.PortModule
                                        { exposingList =
                                            Gren.Syntax.Node.Node { end = { column = 25, row = 2 }, start = { column = 12, row = 2 } }
                                                (Gren.Syntax.Exposing.All { end = { column = 24, row = 2 }, start = { column = 22, row = 2 } })
                                        , moduleName = Gren.Syntax.Node.Node { end = { column = 11, row = 2 }, start = { column = 8, row = 2 } } [ "Foo" ]
                                        }
                                    )
                            }
                        )
            )
        , Test.test "A file with a large number of comments should not create a stack overflow"
            (\() ->
                let
                    comments : String
                    comments =
                        String.repeat 3000 "-- a\n"
                in
                ("""module Foo exposing (..)
a = 1
"""
                    ++ comments
                )
                    |> GrenParserLenient.run GrenParserLenient.module_
                    |> Maybe.map (\ast -> List.length ast.comments)
                    |> Expect.equal (Just 3000)
            )
        , Test.describe "declaration"
            [ Test.test "value/function declaration"
                (\() ->
                    "foo = bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                                (GrenSyntax.FunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                                            { name = Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "foo"
                                            , arguments = []
                                            , expression = Gren.Syntax.Node.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 10 } } (Gren.Syntax.Expression.FunctionOrValue [] "bar")
                                            }
                                    }
                                )
                            )
                )
            , Test.test "function declaration with documentation"
                (\() ->
                    """{-| Foo does bar -}
foo = bar"""
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 10 } }
                                (GrenSyntax.FunctionDeclaration
                                    { documentation = Just (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 20 } } "{-| Foo does bar -}")
                                    , signature = Nothing
                                    , declaration =
                                        Gren.Syntax.Node.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 10 } }
                                            { name = Gren.Syntax.Node.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 4 } } "foo"
                                            , arguments = []
                                            , expression = Gren.Syntax.Node.Node { start = { row = 2, column = 7 }, end = { row = 2, column = 10 } } (Gren.Syntax.Expression.FunctionOrValue [] "bar")
                                            }
                                    }
                                )
                            )
                )
            , Test.test "function declaration with empty record"
                (\() ->
                    "foo = {}"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 9 } }
                                (GrenSyntax.FunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 9 } }
                                            { name = Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "foo"
                                            , arguments = []
                                            , expression = Gren.Syntax.Node.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 9 } } (Gren.Syntax.Expression.RecordExpr [])
                                            }
                                    }
                                )
                            )
                )
            , Test.test "function with case in let"
                (\() ->
                    """inc x =
  let
    y =
      case x of
        True -> z
    a = b
  in a"""
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 7, column = 7 } }
                                (GrenSyntax.FunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 7, column = 7 } }
                                            { name = Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "inc"
                                            , arguments = [ Gren.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } (Gren.Syntax.Pattern.VarPattern "x") ]
                                            , expression =
                                                Gren.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 7, column = 7 } }
                                                    (Gren.Syntax.Expression.LetExpression
                                                        { declarations =
                                                            [ Gren.Syntax.Node.Node { start = { row = 3, column = 5 }, end = { row = 5, column = 18 } }
                                                                (Gren.Syntax.Expression.LetFunction
                                                                    { documentation = Nothing
                                                                    , signature = Nothing
                                                                    , declaration =
                                                                        Gren.Syntax.Node.Node { start = { row = 3, column = 5 }, end = { row = 5, column = 18 } }
                                                                            { name = Gren.Syntax.Node.Node { start = { row = 3, column = 5 }, end = { row = 3, column = 6 } } "y"
                                                                            , arguments = []
                                                                            , expression =
                                                                                Gren.Syntax.Node.Node { start = { row = 4, column = 7 }, end = { row = 5, column = 18 } }
                                                                                    (Gren.Syntax.Expression.CaseExpression
                                                                                        { expression = Gren.Syntax.Node.Node { start = { row = 4, column = 12 }, end = { row = 4, column = 13 } } (Gren.Syntax.Expression.FunctionOrValue [] "x")
                                                                                        , cases =
                                                                                            [ ( Gren.Syntax.Node.Node { start = { row = 5, column = 9 }, end = { row = 5, column = 13 } } (Gren.Syntax.Pattern.NamedPattern { moduleName = [], name = "True" } [])
                                                                                              , Gren.Syntax.Node.Node { start = { row = 5, column = 17 }, end = { row = 5, column = 18 } } (Gren.Syntax.Expression.FunctionOrValue [] "z")
                                                                                              )
                                                                                            ]
                                                                                        }
                                                                                    )
                                                                            }
                                                                    }
                                                                )
                                                            , Gren.Syntax.Node.Node { start = { row = 6, column = 5 }, end = { row = 6, column = 10 } }
                                                                (Gren.Syntax.Expression.LetFunction
                                                                    { documentation = Nothing
                                                                    , signature = Nothing
                                                                    , declaration =
                                                                        Gren.Syntax.Node.Node { start = { row = 6, column = 5 }, end = { row = 6, column = 10 } }
                                                                            { name = Gren.Syntax.Node.Node { start = { row = 6, column = 5 }, end = { row = 6, column = 6 } } "a"
                                                                            , arguments = []
                                                                            , expression = Gren.Syntax.Node.Node { start = { row = 6, column = 9 }, end = { row = 6, column = 10 } } (Gren.Syntax.Expression.FunctionOrValue [] "b")
                                                                            }
                                                                    }
                                                                )
                                                            ]
                                                        , expression = Gren.Syntax.Node.Node { start = { row = 7, column = 6 }, end = { row = 7, column = 7 } } (Gren.Syntax.Expression.FunctionOrValue [] "a")
                                                        }
                                                    )
                                            }
                                    }
                                )
                            )
                )
            , Test.test "function declaration with args"
                (\() ->
                    "inc x = x + 1"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 14 } }
                                (GrenSyntax.FunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 14 } }
                                            { name = Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "inc"
                                            , arguments = [ Gren.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } (Gren.Syntax.Pattern.VarPattern "x") ]
                                            , expression =
                                                Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 14 } }
                                                    (Gren.Syntax.Expression.OperatorApplication "+"
                                                        (Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (Gren.Syntax.Expression.FunctionOrValue [] "x"))
                                                        (Gren.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 14 } } (Gren.Syntax.Expression.Integer 1))
                                                    )
                                            }
                                    }
                                )
                            )
                )
            , Test.test "function declaration with let"
                (\() ->
                    """foo =
 let
  b = 1
 in
  b"""
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 5, column = 4 } }
                                (GrenSyntax.FunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 5, column = 4 } }
                                            { name = Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "foo"
                                            , arguments = []
                                            , expression =
                                                Gren.Syntax.Node.Node { start = { row = 2, column = 2 }, end = { row = 5, column = 4 } }
                                                    (Gren.Syntax.Expression.LetExpression
                                                        { declarations =
                                                            [ Gren.Syntax.Node.Node { start = { row = 3, column = 3 }, end = { row = 3, column = 8 } }
                                                                (Gren.Syntax.Expression.LetFunction
                                                                    { documentation = Nothing
                                                                    , signature = Nothing
                                                                    , declaration =
                                                                        Gren.Syntax.Node.Node { start = { row = 3, column = 3 }, end = { row = 3, column = 8 } }
                                                                            { name = Gren.Syntax.Node.Node { start = { row = 3, column = 3 }, end = { row = 3, column = 4 } } "b"
                                                                            , arguments = []
                                                                            , expression = Gren.Syntax.Node.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 8 } } (Gren.Syntax.Expression.Integer 1)
                                                                            }
                                                                    }
                                                                )
                                                            ]
                                                        , expression = Gren.Syntax.Node.Node { start = { row = 5, column = 3 }, end = { row = 5, column = 4 } } (Gren.Syntax.Expression.FunctionOrValue [] "b")
                                                        }
                                                    )
                                            }
                                    }
                                )
                            )
                )
            , Test.test "documentation comment inside a let is invalid"
                (\() ->
                    expectFailsToParse GrenParserLenient.declaration
                        """foo =
 let
  {-| b is one -}
  b = 1
 in
  b"""
                )
            , Test.test "let destructuring with no spaces around '='"
                (\() ->
                    """foo =
 let
  (b, c)=(1, 2)
 in
  b"""
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 5, column = 4 } }
                                (GrenSyntax.FunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 5, column = 4 } }
                                            { name = Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "foo"
                                            , arguments = []
                                            , expression =
                                                Gren.Syntax.Node.Node { start = { row = 2, column = 2 }, end = { row = 5, column = 4 } }
                                                    (Gren.Syntax.Expression.LetExpression
                                                        { declarations =
                                                            [ Gren.Syntax.Node.Node { start = { row = 3, column = 3 }, end = { row = 3, column = 16 } }
                                                                (Gren.Syntax.Expression.LetDestructuring
                                                                    (Gren.Syntax.Node.Node { start = { row = 3, column = 3 }, end = { row = 3, column = 9 } }
                                                                        (Gren.Syntax.Pattern.TuplePattern
                                                                            [ Gren.Syntax.Node.Node { start = { row = 3, column = 4 }, end = { row = 3, column = 5 } } (Gren.Syntax.Pattern.VarPattern "b")
                                                                            , Gren.Syntax.Node.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 8 } } (Gren.Syntax.Pattern.VarPattern "c")
                                                                            ]
                                                                        )
                                                                    )
                                                                    (Gren.Syntax.Node.Node { start = { row = 3, column = 10 }, end = { row = 3, column = 16 } }
                                                                        (Gren.Syntax.Expression.TupledExpression
                                                                            [ Gren.Syntax.Node.Node { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } } (Gren.Syntax.Expression.Integer 1)
                                                                            , Gren.Syntax.Node.Node { start = { row = 3, column = 14 }, end = { row = 3, column = 15 } } (Gren.Syntax.Expression.Integer 2)
                                                                            ]
                                                                        )
                                                                    )
                                                                )
                                                            ]
                                                        , expression = Gren.Syntax.Node.Node { start = { row = 5, column = 3 }, end = { row = 5, column = 4 } } (Gren.Syntax.Expression.FunctionOrValue [] "b")
                                                        }
                                                    )
                                            }
                                    }
                                )
                            )
                )
            , Test.test "declaration with record"
                (\() ->
                    """main =
  beginnerProgram { model = 0, view = view, update = update }"""
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 62 } }
                                (GrenSyntax.FunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 62 } }
                                            { name = Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } "main"
                                            , arguments = []
                                            , expression =
                                                Gren.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 62 } }
                                                    (Gren.Syntax.Expression.Application
                                                        [ Gren.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 18 } } (Gren.Syntax.Expression.FunctionOrValue [] "beginnerProgram")
                                                        , Gren.Syntax.Node.Node { start = { row = 2, column = 19 }, end = { row = 2, column = 62 } }
                                                            (Gren.Syntax.Expression.RecordExpr
                                                                [ Gren.Syntax.Node.Node { start = { row = 2, column = 21 }, end = { row = 2, column = 30 } }
                                                                    ( Gren.Syntax.Node.Node { start = { row = 2, column = 21 }, end = { row = 2, column = 26 } } "model"
                                                                    , Gren.Syntax.Node.Node { start = { row = 2, column = 29 }, end = { row = 2, column = 30 } } (Gren.Syntax.Expression.Integer 0)
                                                                    )
                                                                , Gren.Syntax.Node.Node { start = { row = 2, column = 32 }, end = { row = 2, column = 43 } }
                                                                    ( Gren.Syntax.Node.Node { start = { row = 2, column = 32 }, end = { row = 2, column = 36 } } "view"
                                                                    , Gren.Syntax.Node.Node { start = { row = 2, column = 39 }, end = { row = 2, column = 43 } } (Gren.Syntax.Expression.FunctionOrValue [] "view")
                                                                    )
                                                                , Gren.Syntax.Node.Node { start = { row = 2, column = 45 }, end = { row = 2, column = 61 } }
                                                                    ( Gren.Syntax.Node.Node { start = { row = 2, column = 45 }, end = { row = 2, column = 51 } } "update"
                                                                    , Gren.Syntax.Node.Node { start = { row = 2, column = 54 }, end = { row = 2, column = 60 } } (Gren.Syntax.Expression.FunctionOrValue [] "update")
                                                                    )
                                                                ]
                                                            )
                                                        ]
                                                    )
                                            }
                                    }
                                )
                            )
                )
            , Test.test "update function"
                (\() ->
                    """update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1"""
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 7, column = 16 } }
                                (GrenSyntax.FunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 7, column = 16 } }
                                            { name = Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } } "update"
                                            , arguments =
                                                [ Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } (Gren.Syntax.Pattern.VarPattern "msg")
                                                , Gren.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 17 } } (Gren.Syntax.Pattern.VarPattern "model")
                                                ]
                                            , expression =
                                                Gren.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 7, column = 16 } }
                                                    (Gren.Syntax.Expression.CaseExpression
                                                        { expression = Gren.Syntax.Node.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } (Gren.Syntax.Expression.FunctionOrValue [] "msg")
                                                        , cases =
                                                            [ ( Gren.Syntax.Node.Node { start = { row = 3, column = 5 }, end = { row = 3, column = 14 } } (Gren.Syntax.Pattern.NamedPattern { moduleName = [], name = "Increment" } [])
                                                              , Gren.Syntax.Node.Node { start = { row = 4, column = 7 }, end = { row = 4, column = 16 } }
                                                                    (Gren.Syntax.Expression.OperatorApplication "+"
                                                                        (Gren.Syntax.Node.Node { start = { row = 4, column = 7 }, end = { row = 4, column = 12 } } (Gren.Syntax.Expression.FunctionOrValue [] "model"))
                                                                        (Gren.Syntax.Node.Node { start = { row = 4, column = 15 }, end = { row = 4, column = 16 } } (Gren.Syntax.Expression.Integer 1))
                                                                    )
                                                              )
                                                            , ( Gren.Syntax.Node.Node { start = { row = 6, column = 5 }, end = { row = 6, column = 14 } } (Gren.Syntax.Pattern.NamedPattern { moduleName = [], name = "Decrement" } [])
                                                              , Gren.Syntax.Node.Node { start = { row = 7, column = 7 }, end = { row = 7, column = 16 } }
                                                                    (Gren.Syntax.Expression.OperatorApplication "-"
                                                                        (Gren.Syntax.Node.Node { start = { row = 7, column = 7 }, end = { row = 7, column = 12 } } (Gren.Syntax.Expression.FunctionOrValue [] "model"))
                                                                        (Gren.Syntax.Node.Node { start = { row = 7, column = 15 }, end = { row = 7, column = 16 } } (Gren.Syntax.Expression.Integer 1))
                                                                    )
                                                              )
                                                            ]
                                                        }
                                                    )
                                            }
                                    }
                                )
                            )
                )
            , Test.test "port declaration for command"
                (\() ->
                    "port parseResponse : ( String, String ) -> Cmd msg"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 51 } }
                                (GrenSyntax.PortDeclaration
                                    { name = Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 19 } } "parseResponse"
                                    , typeAnnotation =
                                        Gren.Syntax.Node.Node { start = { row = 1, column = 22 }, end = { row = 1, column = 51 } }
                                            (Gren.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                                (Gren.Syntax.Node.Node { start = { row = 1, column = 22 }, end = { row = 1, column = 40 } }
                                                    (Gren.Syntax.TypeAnnotation.Tupled
                                                        [ Gren.Syntax.Node.Node { start = { row = 1, column = 24 }, end = { row = 1, column = 30 } } (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 24 }, end = { row = 1, column = 30 } } ( [], "String" )) [])
                                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 32 }, end = { row = 1, column = 38 } }
                                                            (Gren.Syntax.TypeAnnotation.Typed
                                                                (Gren.Syntax.Node.Node { start = { row = 1, column = 32 }, end = { row = 1, column = 38 } } ( [], "String" ))
                                                                []
                                                            )
                                                        ]
                                                    )
                                                )
                                                (Gren.Syntax.Node.Node { start = { row = 1, column = 44 }, end = { row = 1, column = 51 } }
                                                    (Gren.Syntax.TypeAnnotation.Typed
                                                        (Gren.Syntax.Node.Node { start = { row = 1, column = 44 }, end = { row = 1, column = 47 } } ( [], "Cmd" ))
                                                        [ Gren.Syntax.Node.Node { start = { row = 1, column = 48 }, end = { row = 1, column = 51 } } (Gren.Syntax.TypeAnnotation.GenericType "msg") ]
                                                    )
                                                )
                                            )
                                    }
                                )
                            )
                )
            , Test.test "port declaration for subscription"
                (\() ->
                    "port scroll : (Move -> msg) -> Sub msg"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 39 } }
                                (GrenSyntax.PortDeclaration
                                    { name = Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 12 } } "scroll"
                                    , typeAnnotation =
                                        Gren.Syntax.Node.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 39 } }
                                            (Gren.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                                (Gren.Syntax.Node.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 28 } }
                                                    (Gren.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                                        (Gren.Syntax.Node.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 20 } }
                                                            (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 20 } } ( [], "Move" )) [])
                                                        )
                                                        (Gren.Syntax.Node.Node { start = { row = 1, column = 24 }, end = { row = 1, column = 27 } } (Gren.Syntax.TypeAnnotation.GenericType "msg"))
                                                    )
                                                )
                                                (Gren.Syntax.Node.Node { start = { row = 1, column = 32 }, end = { row = 1, column = 39 } }
                                                    (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 32 }, end = { row = 1, column = 35 } } ( [], "Sub" ))
                                                        [ Gren.Syntax.Node.Node { start = { row = 1, column = 36 }, end = { row = 1, column = 39 } } (Gren.Syntax.TypeAnnotation.GenericType "msg")
                                                        ]
                                                    )
                                                )
                                            )
                                    }
                                )
                            )
                )
            , Test.test "should fail to parse destructuring declaration at the top-level"
                (\() ->
                    "_ = b"
                        |> expectFailsToParse GrenParserLenient.declaration
                )
            , Test.test "simple main declaration"
                (\() ->
                    """main =
  text "Hello, World!\""""
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 23 } }
                                (GrenSyntax.FunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 23 } }
                                            { name = Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } "main"
                                            , arguments = []
                                            , expression =
                                                Gren.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 23 } }
                                                    (Gren.Syntax.Expression.Application
                                                        [ Gren.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 7 } } (Gren.Syntax.Expression.FunctionOrValue [] "text")
                                                        , Gren.Syntax.Node.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 23 } } (Gren.Syntax.Expression.Literal "Hello, World!")
                                                        ]
                                                    )
                                            }
                                    }
                                )
                            )
                )
            , Test.test "value/function declaration with signature"
                (\() ->
                    """main : Html msg
main =
  text "Hello, World!\""""
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { end = { column = 23, row = 3 }, start = { column = 1, row = 1 } }
                                (GrenSyntax.FunctionDeclaration
                                    { declaration =
                                        Gren.Syntax.Node.Node { end = { column = 23, row = 3 }, start = { column = 1, row = 2 } }
                                            { arguments = []
                                            , expression =
                                                Gren.Syntax.Node.Node { end = { column = 23, row = 3 }, start = { column = 3, row = 3 } }
                                                    (Gren.Syntax.Expression.Application
                                                        [ Gren.Syntax.Node.Node { end = { column = 7, row = 3 }, start = { column = 3, row = 3 } }
                                                            (Gren.Syntax.Expression.FunctionOrValue [] "text")
                                                        , Gren.Syntax.Node.Node { end = { column = 23, row = 3 }, start = { column = 8, row = 3 } }
                                                            (Gren.Syntax.Expression.Literal "Hello, World!")
                                                        ]
                                                    )
                                            , name = Gren.Syntax.Node.Node { end = { column = 5, row = 2 }, start = { column = 1, row = 2 } } "main"
                                            }
                                    , documentation = Nothing
                                    , signature =
                                        Just
                                            (Gren.Syntax.Node.Node { end = { column = 16, row = 1 }, start = { column = 1, row = 1 } }
                                                { name = Gren.Syntax.Node.Node { end = { column = 5, row = 1 }, start = { column = 1, row = 1 } } "main"
                                                , typeAnnotation =
                                                    Gren.Syntax.Node.Node { end = { column = 16, row = 1 }, start = { column = 8, row = 1 } }
                                                        (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { end = { column = 12, row = 1 }, start = { column = 8, row = 1 } } ( [], "Html" ))
                                                            [ Gren.Syntax.Node.Node { end = { column = 16, row = 1 }, start = { column = 13, row = 1 } }
                                                                (Gren.Syntax.TypeAnnotation.GenericType "msg")
                                                            ]
                                                        )
                                                }
                                            )
                                    }
                                )
                            )
                )
            , Test.test "value/function declaration with signature omitting start name"
                (\() ->
                    """: Html msg
main =
  text "Hello, World!\""""
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { end = { column = 23, row = 3 }, start = { column = 1, row = 1 } }
                                (GrenSyntax.FunctionDeclaration
                                    { declaration =
                                        Gren.Syntax.Node.Node { end = { column = 23, row = 3 }, start = { column = 1, row = 2 } }
                                            { arguments = []
                                            , expression =
                                                Gren.Syntax.Node.Node { end = { column = 23, row = 3 }, start = { column = 3, row = 3 } }
                                                    (Gren.Syntax.Expression.Application
                                                        [ Gren.Syntax.Node.Node { end = { column = 7, row = 3 }, start = { column = 3, row = 3 } }
                                                            (Gren.Syntax.Expression.FunctionOrValue [] "text")
                                                        , Gren.Syntax.Node.Node { end = { column = 23, row = 3 }, start = { column = 8, row = 3 } }
                                                            (Gren.Syntax.Expression.Literal "Hello, World!")
                                                        ]
                                                    )
                                            , name = Gren.Syntax.Node.Node { end = { column = 5, row = 2 }, start = { column = 1, row = 2 } } "main"
                                            }
                                    , documentation = Nothing
                                    , signature =
                                        Just
                                            (Gren.Syntax.Node.Node { end = { column = 11, row = 1 }, start = { column = 1, row = 1 } }
                                                { name = Gren.Syntax.Node.Node { end = { column = 1, row = 1 }, start = { column = 1, row = 1 } } "main"
                                                , typeAnnotation =
                                                    Gren.Syntax.Node.Node { end = { column = 11, row = 1 }, start = { column = 3, row = 1 } }
                                                        (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { end = { column = 7, row = 1 }, start = { column = 3, row = 1 } } ( [], "Html" ))
                                                            [ Gren.Syntax.Node.Node { end = { column = 11, row = 1 }, start = { column = 8, row = 1 } }
                                                                (Gren.Syntax.TypeAnnotation.GenericType "msg")
                                                            ]
                                                        )
                                                }
                                            )
                                    }
                                )
                            )
                )
            , Test.test "function with -> instead of ="
                (\() ->
                    """main ->
  text "Hello, World!\""""
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 23 } }
                                (GrenSyntax.FunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 23 } }
                                            { name = Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } "main"
                                            , arguments = []
                                            , expression =
                                                Gren.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 23 } }
                                                    (Gren.Syntax.Expression.Application
                                                        [ Gren.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 7 } } (Gren.Syntax.Expression.FunctionOrValue [] "text")
                                                        , Gren.Syntax.Node.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 23 } } (Gren.Syntax.Expression.Literal "Hello, World!")
                                                        ]
                                                    )
                                            }
                                    }
                                )
                            )
                )
            , Test.test "function starting with multi line comment"
                (\() ->
                    """main =
  {- y -} x"""
                        |> expectSyntaxWithComments GrenParserLenient.declaration
                            { syntax =
                                Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 12 } }
                                    (GrenSyntax.FunctionDeclaration
                                        { documentation = Nothing
                                        , signature = Nothing
                                        , declaration =
                                            Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 12 } }
                                                { name = Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } "main"
                                                , arguments = []
                                                , expression = Gren.Syntax.Node.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 12 } } (Gren.Syntax.Expression.FunctionOrValue [] "x")
                                                }
                                        }
                                    )
                            , comments = [ Gren.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } } "{- y -}" ]
                            }
                )
            , Test.test "function with a lot of symbols"
                (\() ->
                    "updateState update sendPort = curry <| (uncurry update) >> batchStateCmds sendPort"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 83 } }
                                (GrenSyntax.FunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 83 } }
                                            { name = Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } "updateState"
                                            , arguments = [ Gren.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 19 } } (Gren.Syntax.Pattern.VarPattern "update"), Gren.Syntax.Node.Node { start = { row = 1, column = 20 }, end = { row = 1, column = 28 } } (Gren.Syntax.Pattern.VarPattern "sendPort") ]
                                            , expression =
                                                Gren.Syntax.Node.Node { start = { row = 1, column = 31 }, end = { row = 1, column = 83 } }
                                                    (Gren.Syntax.Expression.OperatorApplication "<|"
                                                        (Gren.Syntax.Node.Node { start = { row = 1, column = 31 }, end = { row = 1, column = 36 } } (Gren.Syntax.Expression.FunctionOrValue [] "curry"))
                                                        (Gren.Syntax.Node.Node { start = { row = 1, column = 40 }, end = { row = 1, column = 83 } }
                                                            (Gren.Syntax.Expression.OperatorApplication ">>"
                                                                (Gren.Syntax.Node.Node { start = { row = 1, column = 40 }, end = { row = 1, column = 56 } }
                                                                    (Gren.Syntax.Expression.ParenthesizedExpression
                                                                        (Gren.Syntax.Node.Node { start = { row = 1, column = 41 }, end = { row = 1, column = 55 } }
                                                                            (Gren.Syntax.Expression.Application
                                                                                [ Gren.Syntax.Node.Node { start = { row = 1, column = 41 }, end = { row = 1, column = 48 } } (Gren.Syntax.Expression.FunctionOrValue [] "uncurry")
                                                                                , Gren.Syntax.Node.Node { start = { row = 1, column = 49 }, end = { row = 1, column = 55 } } (Gren.Syntax.Expression.FunctionOrValue [] "update")
                                                                                ]
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                                (Gren.Syntax.Node.Node { start = { row = 1, column = 60 }, end = { row = 1, column = 83 } }
                                                                    (Gren.Syntax.Expression.Application
                                                                        [ Gren.Syntax.Node.Node { start = { row = 1, column = 60 }, end = { row = 1, column = 74 } } (Gren.Syntax.Expression.FunctionOrValue [] "batchStateCmds")
                                                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 75 }, end = { row = 1, column = 83 } } (Gren.Syntax.Expression.FunctionOrValue [] "sendPort")
                                                                        ]
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    )
                                            }
                                    }
                                )
                            )
                )
            , Test.test "Some function"
                (\() ->
                    """update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1"""
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 7, column = 16 } }
                                (GrenSyntax.FunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 7, column = 16 } }
                                            { name = Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } } "update"
                                            , arguments =
                                                [ Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } (Gren.Syntax.Pattern.VarPattern "msg")
                                                , Gren.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 17 } } (Gren.Syntax.Pattern.VarPattern "model")
                                                ]
                                            , expression =
                                                Gren.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 7, column = 16 } }
                                                    (Gren.Syntax.Expression.CaseExpression
                                                        { expression = Gren.Syntax.Node.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } (Gren.Syntax.Expression.FunctionOrValue [] "msg")
                                                        , cases =
                                                            [ ( Gren.Syntax.Node.Node { start = { row = 3, column = 5 }, end = { row = 3, column = 14 } } (Gren.Syntax.Pattern.NamedPattern { moduleName = [], name = "Increment" } [])
                                                              , Gren.Syntax.Node.Node { start = { row = 4, column = 7 }, end = { row = 4, column = 16 } }
                                                                    (Gren.Syntax.Expression.OperatorApplication "+"
                                                                        (Gren.Syntax.Node.Node { start = { row = 4, column = 7 }, end = { row = 4, column = 12 } } (Gren.Syntax.Expression.FunctionOrValue [] "model"))
                                                                        (Gren.Syntax.Node.Node { start = { row = 4, column = 15 }, end = { row = 4, column = 16 } } (Gren.Syntax.Expression.Integer 1))
                                                                    )
                                                              )
                                                            , ( Gren.Syntax.Node.Node { start = { row = 6, column = 5 }, end = { row = 6, column = 14 } } (Gren.Syntax.Pattern.NamedPattern { moduleName = [], name = "Decrement" } [])
                                                              , Gren.Syntax.Node.Node { start = { row = 7, column = 7 }, end = { row = 7, column = 16 } }
                                                                    (Gren.Syntax.Expression.OperatorApplication "-"
                                                                        (Gren.Syntax.Node.Node { start = { row = 7, column = 7 }, end = { row = 7, column = 12 } } (Gren.Syntax.Expression.FunctionOrValue [] "model"))
                                                                        (Gren.Syntax.Node.Node { start = { row = 7, column = 15 }, end = { row = 7, column = 16 } } (Gren.Syntax.Expression.Integer 1))
                                                                    )
                                                              )
                                                            ]
                                                        }
                                                    )
                                            }
                                    }
                                )
                            )
                )
            , Test.test "some other function"
                (\() ->
                    """update : Model
update msg model =
    msg"""
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 3, column = 8 } }
                                (GrenSyntax.FunctionDeclaration
                                    { documentation = Nothing
                                    , signature =
                                        Just
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                                                { name = Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } } "update"
                                                , typeAnnotation = Gren.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } } (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } } ( [], "Model" )) [])
                                                }
                                            )
                                    , declaration =
                                        Gren.Syntax.Node.Node { start = { row = 2, column = 1 }, end = { row = 3, column = 8 } }
                                            { name = Gren.Syntax.Node.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 7 } } "update"
                                            , arguments =
                                                [ Gren.Syntax.Node.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } (Gren.Syntax.Pattern.VarPattern "msg")
                                                , Gren.Syntax.Node.Node { start = { row = 2, column = 12 }, end = { row = 2, column = 17 } } (Gren.Syntax.Pattern.VarPattern "model")
                                                ]
                                            , expression = Gren.Syntax.Node.Node { start = { row = 3, column = 5 }, end = { row = 3, column = 8 } } (Gren.Syntax.Expression.FunctionOrValue [] "msg")
                                            }
                                    }
                                )
                            )
                )
            , Test.test "type alias"
                (\() ->
                    "type alias Foo = {color: String }"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 34 } }
                                (GrenSyntax.AliasDeclaration
                                    { documentation = Nothing
                                    , name = Gren.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 15 } } "Foo"
                                    , generics = []
                                    , typeAnnotation =
                                        Gren.Syntax.Node.Node { start = { row = 1, column = 18 }, end = { row = 1, column = 34 } }
                                            (Gren.Syntax.TypeAnnotation.Record
                                                [ Gren.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 32 } }
                                                    ( Gren.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 24 } } "color"
                                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 26 }, end = { row = 1, column = 32 } }
                                                        (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 26 }, end = { row = 1, column = 32 } } ( [], "String" )) [])
                                                    )
                                                ]
                                            )
                                    }
                                )
                            )
                )
            , Test.test "type alias with documentation"
                (\() ->
                    """{-| Foo is colorful -}
type alias Foo = {color: String }"""
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 34 } }
                                (GrenSyntax.AliasDeclaration
                                    { documentation = Just (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 23 } } "{-| Foo is colorful -}")
                                    , name = Gren.Syntax.Node.Node { start = { row = 2, column = 12 }, end = { row = 2, column = 15 } } "Foo"
                                    , generics = []
                                    , typeAnnotation =
                                        Gren.Syntax.Node.Node { start = { row = 2, column = 18 }, end = { row = 2, column = 34 } }
                                            (Gren.Syntax.TypeAnnotation.Record
                                                [ Gren.Syntax.Node.Node { start = { row = 2, column = 19 }, end = { row = 2, column = 32 } }
                                                    ( Gren.Syntax.Node.Node { start = { row = 2, column = 19 }, end = { row = 2, column = 24 } } "color"
                                                    , Gren.Syntax.Node.Node { start = { row = 2, column = 26 }, end = { row = 2, column = 32 } }
                                                        (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 2, column = 26 }, end = { row = 2, column = 32 } } ( [], "String" )) [])
                                                    )
                                                ]
                                            )
                                    }
                                )
                            )
                )
            , Test.test "type alias without spacings around '='"
                (\() ->
                    "type alias Foo={color: String }"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                                (GrenSyntax.AliasDeclaration
                                    { documentation = Nothing
                                    , name = Gren.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 15 } } "Foo"
                                    , generics = []
                                    , typeAnnotation =
                                        Gren.Syntax.Node.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 32 } }
                                            (Gren.Syntax.TypeAnnotation.Record
                                                [ Gren.Syntax.Node.Node { start = { row = 1, column = 17 }, end = { row = 1, column = 30 } }
                                                    ( Gren.Syntax.Node.Node { start = { row = 1, column = 17 }, end = { row = 1, column = 22 } } "color"
                                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 24 }, end = { row = 1, column = 30 } }
                                                        (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 24 }, end = { row = 1, column = 30 } } ( [], "String" )) [])
                                                    )
                                                ]
                                            )
                                    }
                                )
                            )
                )
            , Test.test "type alias with GenericType "
                (\() ->
                    "type alias Foo a = {some : a }"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 31 } }
                                (GrenSyntax.AliasDeclaration
                                    { documentation = Nothing
                                    , name = Gren.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 15 } } "Foo"
                                    , generics = [ Gren.Syntax.Node.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 17 } } "a" ]
                                    , typeAnnotation =
                                        Gren.Syntax.Node.Node { start = { row = 1, column = 20 }, end = { row = 1, column = 31 } }
                                            (Gren.Syntax.TypeAnnotation.Record
                                                [ Gren.Syntax.Node.Node { start = { row = 1, column = 21 }, end = { row = 1, column = 29 } }
                                                    ( Gren.Syntax.Node.Node { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } } "some"
                                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 28 }, end = { row = 1, column = 29 } } (Gren.Syntax.TypeAnnotation.GenericType "a")
                                                    )
                                                ]
                                            )
                                    }
                                )
                            )
                )
            , Test.test "type"
                (\() ->
                    "type Color = Blue String | Red | Green"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 39 } }
                                (GrenSyntax.CustomTypeDeclaration
                                    { documentation = Nothing
                                    , name = Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 11 } } "Color"
                                    , generics = []
                                    , constructors =
                                        [ Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } }
                                            { name = Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 18 } } "Blue"
                                            , arguments =
                                                [ Gren.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 25 } }
                                                    (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 25 } } ( [], "String" )) [])
                                                ]
                                            }
                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } }
                                            { name = Gren.Syntax.Node.Node { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } } "Red"
                                            , arguments = []
                                            }
                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 34 }, end = { row = 1, column = 39 } }
                                            { name = Gren.Syntax.Node.Node { start = { row = 1, column = 34 }, end = { row = 1, column = 39 } } "Green"
                                            , arguments = []
                                            }
                                        ]
                                    }
                                )
                            )
                )
            , Test.test "type with leading | before first variant"
                (\() ->
                    "type Color=| Blue String | Red | Green"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 39 } }
                                (GrenSyntax.CustomTypeDeclaration
                                    { documentation = Nothing
                                    , name = Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 11 } } "Color"
                                    , generics = []
                                    , constructors =
                                        [ Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } }
                                            { name = Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 18 } } "Blue"
                                            , arguments =
                                                [ Gren.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 25 } }
                                                    (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 25 } } ( [], "String" )) [])
                                                ]
                                            }
                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } }
                                            { name = Gren.Syntax.Node.Node { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } } "Red"
                                            , arguments = []
                                            }
                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 34 }, end = { row = 1, column = 39 } }
                                            { name = Gren.Syntax.Node.Node { start = { row = 1, column = 34 }, end = { row = 1, column = 39 } } "Green"
                                            , arguments = []
                                            }
                                        ]
                                    }
                                )
                            )
                )
            , Test.test "type with extra | between variants"
                (\() ->
                    "type Color = Blue String ||Red | Green"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 39 } }
                                (GrenSyntax.CustomTypeDeclaration
                                    { documentation = Nothing
                                    , name = Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 11 } } "Color"
                                    , generics = []
                                    , constructors =
                                        [ Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } }
                                            { name = Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 18 } } "Blue"
                                            , arguments =
                                                [ Gren.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 25 } }
                                                    (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 25 } } ( [], "String" )) [])
                                                ]
                                            }
                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } }
                                            { name = Gren.Syntax.Node.Node { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } } "Red"
                                            , arguments = []
                                            }
                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 34 }, end = { row = 1, column = 39 } }
                                            { name = Gren.Syntax.Node.Node { start = { row = 1, column = 34 }, end = { row = 1, column = 39 } } "Green"
                                            , arguments = []
                                            }
                                        ]
                                    }
                                )
                            )
                )
            , Test.test "type with documentation"
                (\() ->
                    """{-| Classic RGB -}
type Color = Blue String | Red | Green"""
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 39 } }
                                (GrenSyntax.CustomTypeDeclaration
                                    { documentation = Just (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 19 } } "{-| Classic RGB -}")
                                    , name = Gren.Syntax.Node.Node { start = { row = 2, column = 6 }, end = { row = 2, column = 11 } } "Color"
                                    , generics = []
                                    , constructors =
                                        [ Gren.Syntax.Node.Node
                                            { start = { row = 2, column = 14 }
                                            , end = { row = 2, column = 25 }
                                            }
                                            { name = Gren.Syntax.Node.Node { start = { row = 2, column = 14 }, end = { row = 2, column = 18 } } "Blue"
                                            , arguments =
                                                [ Gren.Syntax.Node.Node { start = { row = 2, column = 19 }, end = { row = 2, column = 25 } }
                                                    (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 2, column = 19 }, end = { row = 2, column = 25 } } ( [], "String" )) [])
                                                ]
                                            }
                                        , Gren.Syntax.Node.Node { start = { row = 2, column = 28 }, end = { row = 2, column = 31 } }
                                            { name = Gren.Syntax.Node.Node { start = { row = 2, column = 28 }, end = { row = 2, column = 31 } } "Red"
                                            , arguments = []
                                            }
                                        , Gren.Syntax.Node.Node { start = { row = 2, column = 34 }, end = { row = 2, column = 39 } }
                                            { name = Gren.Syntax.Node.Node { start = { row = 2, column = 34 }, end = { row = 2, column = 39 } } "Green"
                                            , arguments = []
                                            }
                                        ]
                                    }
                                )
                            )
                )
            , Test.test "type with multiple args"
                (\() ->
                    "type D = C a B"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                                (GrenSyntax.CustomTypeDeclaration
                                    { documentation = Nothing
                                    , name = Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } "D"
                                    , generics = []
                                    , constructors =
                                        [ Gren.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } }
                                            { name = Gren.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 11 } } "C"
                                            , arguments =
                                                [ Gren.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 13 } } (Gren.Syntax.TypeAnnotation.GenericType "a")
                                                , Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } }
                                                    (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } } ( [], "B" )) [])
                                                ]
                                            }
                                        ]
                                    }
                                )
                            )
                )
            , Test.test "type with multiple args and correct distribution of args"
                (\() ->
                    "type D = C B a"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                                (GrenSyntax.CustomTypeDeclaration
                                    { documentation = Nothing
                                    , name = Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } "D"
                                    , generics = []
                                    , constructors =
                                        [ Gren.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } }
                                            { name = Gren.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 11 } } "C"
                                            , arguments =
                                                [ Gren.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 13 } }
                                                    (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 13 } } ( [], "B" )) [])
                                                , Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } } (Gren.Syntax.TypeAnnotation.GenericType "a")
                                                ]
                                            }
                                        ]
                                    }
                                )
                            )
                )
            , Test.test "type args should not continue on next line"
                (\() ->
                    """type D = C B
a"""
                        |> expectFailsToParse GrenParserLenient.declaration
                )
            , Test.test "type with GenericType"
                (\() ->
                    "type Maybe a = Just a | Nothing"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                                (GrenSyntax.CustomTypeDeclaration
                                    { documentation = Nothing
                                    , name = Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 11 } } "Maybe"
                                    , generics = [ Gren.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 13 } } "a" ]
                                    , constructors =
                                        [ Gren.Syntax.Node.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 22 } }
                                            { name = Gren.Syntax.Node.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 20 } } "Just"
                                            , arguments = [ Gren.Syntax.Node.Node { start = { row = 1, column = 21 }, end = { row = 1, column = 22 } } (Gren.Syntax.TypeAnnotation.GenericType "a") ]
                                            }
                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 25 }, end = { row = 1, column = 32 } }
                                            { name = Gren.Syntax.Node.Node { start = { row = 1, column = 25 }, end = { row = 1, column = 32 } } "Nothing"
                                            , arguments = []
                                            }
                                        ]
                                    }
                                )
                            )
                )
            , Test.test "allow type with variant name without indentation"
                (\() ->
                    """type Maybe a = Just a |
Nothing"""
                        |> GrenParserLenient.run GrenParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "fail if declarations not on module-level"
                (\() ->
                    """a = f
    3
    b = 4"""
                        |> expectFailsToParse GrenParserLenient.declaration
                )
            , Test.test "fail if function declaration argument is `as` without parenthesis"
                (\() ->
                    """a foo as bar = f3"""
                        |> expectFailsToParse GrenParserLenient.declaration
                )
            , Test.test "regression test for disallowing ( +)"
                (\() ->
                    "a = ( +)"
                        |> expectFailsToParse GrenParserLenient.declaration
                )
            , Test.test "regression test for disallowing (+ )"
                (\() ->
                    "a = (+ )"
                        |> expectFailsToParse GrenParserLenient.declaration
                )
            , Test.test "right infix"
                (\() ->
                    "infix right 7 (</>) = slash"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 28 } }
                                (GrenSyntax.InfixDeclaration
                                    { direction = Gren.Syntax.Node.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 12 } } Gren.Syntax.Infix.Right
                                    , precedence = Gren.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 14 } } 7
                                    , operator = Gren.Syntax.Node.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 20 } } "</>"
                                    , function = Gren.Syntax.Node.Node { start = { row = 1, column = 23 }, end = { row = 1, column = 28 } } "slash"
                                    }
                                )
                            )
                )
            , Test.test "left infix"
                (\() ->
                    "infix left  8 (<?>) = questionMark"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 35 } }
                                (GrenSyntax.InfixDeclaration
                                    { direction = Gren.Syntax.Node.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 11 } } Gren.Syntax.Infix.Left
                                    , precedence = Gren.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 14 } } 8
                                    , operator = Gren.Syntax.Node.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 20 } } "<?>"
                                    , function = Gren.Syntax.Node.Node { start = { row = 1, column = 23 }, end = { row = 1, column = 35 } } "questionMark"
                                    }
                                )
                            )
                )
            , Test.test "non infix"
                (\() ->
                    "infix non   4 (==) = eq"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 24 } }
                                (GrenSyntax.InfixDeclaration
                                    { direction = Gren.Syntax.Node.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 10 } } Gren.Syntax.Infix.Non
                                    , precedence = Gren.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 14 } } 4
                                    , operator = Gren.Syntax.Node.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 19 } } "=="
                                    , function = Gren.Syntax.Node.Node { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } } "eq"
                                    }
                                )
                            )
                )
            ]
        , Test.describe "type"
            [ Test.test "unitTypeReference"
                (\() ->
                    "()"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } Gren.Syntax.TypeAnnotation.Unit)
                )
            , Test.test "unitTypeReference with spaces"
                (\() ->
                    "( )"
                        |> expectFailsToParse GrenParserLenient.type_
                )
            , Test.test "tupledTypeReference"
                (\() ->
                    "( (), ())"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                                (Gren.Syntax.TypeAnnotation.Tupled
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 5 } } Gren.Syntax.TypeAnnotation.Unit
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 9 } } Gren.Syntax.TypeAnnotation.Unit
                                    ]
                                )
                            )
                )
            , Test.test "4-tuple type annotation is invalid"
                (\() ->
                    "(Int,String,(),a)"
                        |> expectFailsToParse GrenParserLenient.type_
                )
            , Test.test "tupledTypeReference 2"
                (\() ->
                    "( () )"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } } Gren.Syntax.TypeAnnotation.Unit)
                )
            , Test.test "tupledTypeReference 3"
                (\() ->
                    "( () , Maybe m )"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 17 } }
                                (Gren.Syntax.TypeAnnotation.Tupled
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 5 } } Gren.Syntax.TypeAnnotation.Unit
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 15 } }
                                        (Gren.Syntax.TypeAnnotation.Typed
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 13 } } ( [], "Maybe" ))
                                            [ Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } } (Gren.Syntax.TypeAnnotation.GenericType "m") ]
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "qualified type reference"
                (\() ->
                    "Foo.Bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                                (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } } ( [ "Foo" ], "Bar" )) [])
                            )
                )
            , Test.test "typeAnnotationNoFn"
                (\() ->
                    "Bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } ( [], "Bar" )) [])
                            )
                )
            , Test.test "typedTypeReference 1"
                (\() ->
                    "Foo () a Bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                                (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } ( [], "Foo" ))
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 7 } } Gren.Syntax.TypeAnnotation.Unit
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } } (Gren.Syntax.TypeAnnotation.GenericType "a")
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 13 } }
                                        (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 13 } } ( [], "Bar" )) [])
                                    ]
                                )
                            )
                )
            , Test.test "typedTypeReference 2"
                (\() ->
                    "Foo () a Bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                                (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } ( [], "Foo" ))
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 7 } } Gren.Syntax.TypeAnnotation.Unit
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } } (Gren.Syntax.TypeAnnotation.GenericType "a")
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 13 } }
                                        (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 13 } } ( [], "Bar" )) [])
                                    ]
                                )
                            )
                )
            , Test.test "recordTypeReference empty"
                (\() ->
                    "{}"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } (Gren.Syntax.TypeAnnotation.Record []))
                )
            , Test.test "recordTypeReference one field"
                (\() ->
                    "{color: String }"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 17 } }
                                (Gren.Syntax.TypeAnnotation.Record
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 15 } }
                                        ( Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } "color"
                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 15 } }
                                            (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 15 } } ( [], "String" )) [])
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "recordTypeReference one field with name-value separator ="
                (\() ->
                    "{color= String }"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 17 } }
                                (Gren.Syntax.TypeAnnotation.Record
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 15 } }
                                        ( Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } "color"
                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 15 } }
                                            (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 15 } } ( [], "String" )) [])
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "recordTypeReference one field with name-value separator empty"
                (\() ->
                    "{color  String }"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 17 } }
                                (Gren.Syntax.TypeAnnotation.Record
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 15 } }
                                        ( Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } "color"
                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 15 } }
                                            (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 15 } } ( [], "String" )) [])
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "type record with field name colliding with keyword"
                (\() ->
                    "{  as : Int, b : Int }"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (Gren.Syntax.Node.Node { end = { column = 23, row = 1 }, start = { column = 1, row = 1 } }
                                (Gren.Syntax.TypeAnnotation.Record
                                    [ Gren.Syntax.Node.Node { end = { column = 12, row = 1 }, start = { column = 4, row = 1 } }
                                        ( Gren.Syntax.Node.Node { end = { column = 6, row = 1 }, start = { column = 4, row = 1 } } "as_"
                                        , Gren.Syntax.Node.Node { end = { column = 12, row = 1 }, start = { column = 9, row = 1 } }
                                            (Gren.Syntax.TypeAnnotation.Typed
                                                (Gren.Syntax.Node.Node { end = { column = 12, row = 1 }, start = { column = 9, row = 1 } }
                                                    ( [], "Int" )
                                                )
                                                []
                                            )
                                        )
                                    , Gren.Syntax.Node.Node { end = { column = 22, row = 1 }, start = { column = 14, row = 1 } }
                                        ( Gren.Syntax.Node.Node { end = { column = 15, row = 1 }, start = { column = 14, row = 1 } } "b"
                                        , Gren.Syntax.Node.Node { end = { column = 21, row = 1 }, start = { column = 18, row = 1 } }
                                            (Gren.Syntax.TypeAnnotation.Typed
                                                (Gren.Syntax.Node.Node { end = { column = 21, row = 1 }, start = { column = 18, row = 1 } }
                                                    ( [], "Int" )
                                                )
                                                []
                                            )
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "type record with prefixed comma"
                (\() ->
                    "{ , a : Int, b : Int }"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (Gren.Syntax.Node.Node { end = { column = 23, row = 1 }, start = { column = 1, row = 1 } }
                                (Gren.Syntax.TypeAnnotation.Record
                                    [ Gren.Syntax.Node.Node { end = { column = 12, row = 1 }, start = { column = 5, row = 1 } }
                                        ( Gren.Syntax.Node.Node { end = { column = 6, row = 1 }, start = { column = 5, row = 1 } } "a"
                                        , Gren.Syntax.Node.Node { end = { column = 12, row = 1 }, start = { column = 9, row = 1 } }
                                            (Gren.Syntax.TypeAnnotation.Typed
                                                (Gren.Syntax.Node.Node { end = { column = 12, row = 1 }, start = { column = 9, row = 1 } }
                                                    ( [], "Int" )
                                                )
                                                []
                                            )
                                        )
                                    , Gren.Syntax.Node.Node { end = { column = 22, row = 1 }, start = { column = 14, row = 1 } }
                                        ( Gren.Syntax.Node.Node { end = { column = 15, row = 1 }, start = { column = 14, row = 1 } } "b"
                                        , Gren.Syntax.Node.Node { end = { column = 21, row = 1 }, start = { column = 18, row = 1 } }
                                            (Gren.Syntax.TypeAnnotation.Typed
                                                (Gren.Syntax.Node.Node { end = { column = 21, row = 1 }, start = { column = 18, row = 1 } }
                                                    ( [], "Int" )
                                                )
                                                []
                                            )
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "type record with extra comma between fields"
                (\() ->
                    "{   a : Int,,b : Int }"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (Gren.Syntax.Node.Node { end = { column = 23, row = 1 }, start = { column = 1, row = 1 } }
                                (Gren.Syntax.TypeAnnotation.Record
                                    [ Gren.Syntax.Node.Node { end = { column = 12, row = 1 }, start = { column = 5, row = 1 } }
                                        ( Gren.Syntax.Node.Node { end = { column = 6, row = 1 }, start = { column = 5, row = 1 } } "a"
                                        , Gren.Syntax.Node.Node { end = { column = 12, row = 1 }, start = { column = 9, row = 1 } }
                                            (Gren.Syntax.TypeAnnotation.Typed
                                                (Gren.Syntax.Node.Node { end = { column = 12, row = 1 }, start = { column = 9, row = 1 } }
                                                    ( [], "Int" )
                                                )
                                                []
                                            )
                                        )
                                    , Gren.Syntax.Node.Node { end = { column = 22, row = 1 }, start = { column = 14, row = 1 } }
                                        ( Gren.Syntax.Node.Node { end = { column = 15, row = 1 }, start = { column = 14, row = 1 } } "b"
                                        , Gren.Syntax.Node.Node { end = { column = 21, row = 1 }, start = { column = 18, row = 1 } }
                                            (Gren.Syntax.TypeAnnotation.Typed
                                                (Gren.Syntax.Node.Node { end = { column = 21, row = 1 }, start = { column = 18, row = 1 } }
                                                    ( [], "Int" )
                                                )
                                                []
                                            )
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "record with generic"
                (\() ->
                    "{ attr | position : Vec2, texture : Vec2 }"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 43 } }
                                (Gren.Syntax.TypeAnnotation.GenericRecord (Gren.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 7 } } "attr")
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 42 } }
                                        [ Gren.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 25 } }
                                            ( Gren.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 18 } } "position"
                                            , Gren.Syntax.Node.Node { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } }
                                                (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } } ( [], "Vec2" )) [])
                                            )
                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 27 }, end = { row = 1, column = 42 } }
                                            ( Gren.Syntax.Node.Node { start = { row = 1, column = 27 }, end = { row = 1, column = 34 } } "texture"
                                            , Gren.Syntax.Node.Node { start = { row = 1, column = 37 }, end = { row = 1, column = 41 } }
                                                (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 37 }, end = { row = 1, column = 41 } } ( [], "Vec2" )) [])
                                            )
                                        ]
                                    )
                                )
                            )
                )
            , Test.test "record with generic with extra comma between fields"
                (\() ->
                    "{ attr | position : Vec2,,texture : Vec2 }"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 43 } }
                                (Gren.Syntax.TypeAnnotation.GenericRecord (Gren.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 7 } } "attr")
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 42 } }
                                        [ Gren.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 25 } }
                                            ( Gren.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 18 } } "position"
                                            , Gren.Syntax.Node.Node { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } }
                                                (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } } ( [], "Vec2" )) [])
                                            )
                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 27 }, end = { row = 1, column = 42 } }
                                            ( Gren.Syntax.Node.Node { start = { row = 1, column = 27 }, end = { row = 1, column = 34 } } "texture"
                                            , Gren.Syntax.Node.Node { start = { row = 1, column = 37 }, end = { row = 1, column = 41 } }
                                                (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 37 }, end = { row = 1, column = 41 } } ( [], "Vec2" )) [])
                                            )
                                        ]
                                    )
                                )
                            )
                )
            , Test.test "record with generic, with name-value separator = and name-value separator empty"
                (\() ->
                    "{ attr | position   Vec2, texture = Vec2 }"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 43 } }
                                (Gren.Syntax.TypeAnnotation.GenericRecord (Gren.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 7 } } "attr")
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 42 } }
                                        [ Gren.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 25 } }
                                            ( Gren.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 18 } } "position"
                                            , Gren.Syntax.Node.Node { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } }
                                                (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } } ( [], "Vec2" )) [])
                                            )
                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 27 }, end = { row = 1, column = 42 } }
                                            ( Gren.Syntax.Node.Node { start = { row = 1, column = 27 }, end = { row = 1, column = 34 } } "texture"
                                            , Gren.Syntax.Node.Node { start = { row = 1, column = 37 }, end = { row = 1, column = 41 } }
                                                (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 37 }, end = { row = 1, column = 41 } } ( [], "Vec2" )) [])
                                            )
                                        ]
                                    )
                                )
                            )
                )
            , Test.test "generic record with no fields"
                (\() ->
                    "{ attr |}"
                        |> expectFailsToParse GrenParserLenient.type_
                )
            , Test.test "recordTypeReference nested record"
                (\() ->
                    "{color: {r : Int, g :Int, b: Int } }"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 37 } }
                                (Gren.Syntax.TypeAnnotation.Record
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 35 } }
                                        ( Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } "color"
                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 35 } }
                                            (Gren.Syntax.TypeAnnotation.Record
                                                [ Gren.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 17 } }
                                                    ( Gren.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 11 } } "r"
                                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 17 } }
                                                        (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 17 } } ( [], "Int" )) [])
                                                    )
                                                , Gren.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 25 } }
                                                    ( Gren.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } } "g"
                                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 22 }, end = { row = 1, column = 25 } }
                                                        (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 22 }, end = { row = 1, column = 25 } } ( [], "Int" )) [])
                                                    )
                                                , Gren.Syntax.Node.Node { start = { row = 1, column = 27 }, end = { row = 1, column = 34 } }
                                                    ( Gren.Syntax.Node.Node { start = { row = 1, column = 27 }, end = { row = 1, column = 28 } } "b"
                                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 30 }, end = { row = 1, column = 33 } }
                                                        (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 30 }, end = { row = 1, column = 33 } } ( [], "Int" )) [])
                                                    )
                                                ]
                                            )
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "record field ranges"
                (\() ->
                    "{ foo : Int, bar : Int, baz : Int }"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 36 } }
                                (Gren.Syntax.TypeAnnotation.Record
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 12 } }
                                        ( Gren.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 6 } } "foo"
                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } ( [], "Int" )) [])
                                        )
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 23 } }
                                        ( Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 17 } } "bar"
                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 20 }, end = { row = 1, column = 23 } } (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 20 }, end = { row = 1, column = 23 } } ( [], "Int" )) [])
                                        )
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 25 }, end = { row = 1, column = 35 } }
                                        ( Gren.Syntax.Node.Node { start = { row = 1, column = 25 }, end = { row = 1, column = 28 } } "baz"
                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 31 }, end = { row = 1, column = 34 } } (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 31 }, end = { row = 1, column = 34 } } ( [], "Int" )) [])
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "recordTypeReference with generic"
                (\() ->
                    "{color: s }"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                (Gren.Syntax.TypeAnnotation.Record
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 10 } }
                                        ( Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } "color"
                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (Gren.Syntax.TypeAnnotation.GenericType "s")
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "function type reference"
                (\() ->
                    "Foo -> Bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                                (Gren.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                        (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } ( [], "Foo" )) [])
                                    )
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }
                                        (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } ( [], "Bar" )) [])
                                    )
                                )
                            )
                )
            , Test.test "function type reference multiple"
                (\() ->
                    "Foo -> Bar -> baz"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 18 } }
                                (Gren.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                        (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } ( [], "Foo" )) [])
                                    )
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 18 } }
                                        (Gren.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }
                                                (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } ( [], "Bar" )) [])
                                            )
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 18 } } (Gren.Syntax.TypeAnnotation.GenericType "baz"))
                                        )
                                    )
                                )
                            )
                )
            , Test.test "function type reference multiple with consecutive ->"
                (\() ->
                    "Foo->->Bar -> baz"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 18 } }
                                (Gren.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                        (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } ( [], "Foo" )) [])
                                    )
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 18 } }
                                        (Gren.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }
                                                (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } ( [], "Bar" )) [])
                                            )
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 18 } } (Gren.Syntax.TypeAnnotation.GenericType "baz"))
                                        )
                                    )
                                )
                            )
                )
            , Test.test "function type reference generics"
                (\() ->
                    "cMsg -> cModel -> a"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 20 } }
                                (Gren.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } (Gren.Syntax.TypeAnnotation.GenericType "cMsg"))
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 20 } }
                                        (Gren.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 15 } } (Gren.Syntax.TypeAnnotation.GenericType "cModel"))
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } } (Gren.Syntax.TypeAnnotation.GenericType "a"))
                                        )
                                    )
                                )
                            )
                )
            , Test.test "annotation with parens"
                (\() ->
                    "Msg -> Model -> (Model, Cmd Msg)"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 33 } }
                                (Gren.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                        (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } ( [], "Msg" )) [])
                                    )
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 33 } }
                                        (Gren.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 13 } }
                                                (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 13 } } ( [], "Model" )) [])
                                            )
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 17 }, end = { row = 1, column = 33 } }
                                                (Gren.Syntax.TypeAnnotation.Tupled
                                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 18 }, end = { row = 1, column = 23 } }
                                                        (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 18 }, end = { row = 1, column = 23 } } ( [], "Model" )) [])
                                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 25 }, end = { row = 1, column = 32 } }
                                                        (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 25 }, end = { row = 1, column = 28 } } ( [], "Cmd" ))
                                                            [ Gren.Syntax.Node.Node { start = { row = 1, column = 29 }, end = { row = 1, column = 32 } }
                                                                (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 29 }, end = { row = 1, column = 32 } } ( [], "Msg" )) [])
                                                            ]
                                                        )
                                                    ]
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                )
            , Test.test "function as argument"
                (\() ->
                    "( cMsg -> cModel -> a ) -> b"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 29 } }
                                (Gren.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 24 } }
                                        (Gren.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 7 } } (Gren.Syntax.TypeAnnotation.GenericType "cMsg"))
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 22 } }
                                                (Gren.Syntax.TypeAnnotation.FunctionTypeAnnotation (Gren.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 17 } } (Gren.Syntax.TypeAnnotation.GenericType "cModel"))
                                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 21 }, end = { row = 1, column = 22 } } (Gren.Syntax.TypeAnnotation.GenericType "a"))
                                                )
                                            )
                                        )
                                    )
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 28 }, end = { row = 1, column = 29 } } (Gren.Syntax.TypeAnnotation.GenericType "b"))
                                )
                            )
                )
            , Test.test "type with params"
                (\() ->
                    "(Foo -> Bar)"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                                (Gren.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 5 } }
                                        (Gren.Syntax.TypeAnnotation.Typed
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 5 } } ( [], "Foo" ))
                                            []
                                        )
                                    )
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } }
                                        (Gren.Syntax.TypeAnnotation.Typed
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } ( [], "Bar" ))
                                            []
                                        )
                                    )
                                )
                            )
                )
            , Test.test "function type reference multiple and parens"
                (\() ->
                    "(Foo -> Bar) -> baz"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 20 } }
                                (Gren.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                                        (Gren.Syntax.TypeAnnotation.FunctionTypeAnnotation
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 5 } }
                                                (Gren.Syntax.TypeAnnotation.Typed
                                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 5 } } ( [], "Foo" ))
                                                    []
                                                )
                                            )
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } }
                                                (Gren.Syntax.TypeAnnotation.Typed
                                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } ( [], "Bar" ))
                                                    []
                                                )
                                            )
                                        )
                                    )
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 17 }, end = { row = 1, column = 20 } } (Gren.Syntax.TypeAnnotation.GenericType "baz"))
                                )
                            )
                )
            , Test.test "parseTypeWith wrong indent"
                (\() ->
                    "Maybe\na"
                        |> expectFailsToParse GrenParserLenient.type_
                )
            , Test.test "parseTypeWith good indent"
                (\() ->
                    "Maybe\n a"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 3 } }
                                (Gren.Syntax.TypeAnnotation.Typed
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } ( [], "Maybe" ))
                                    [ Gren.Syntax.Node.Node { start = { row = 2, column = 2 }, end = { row = 2, column = 3 } } (Gren.Syntax.TypeAnnotation.GenericType "a") ]
                                )
                            )
                )
            , Test.test "issue #5 - no spaces between type and generic with parens"
                (\() ->
                    "List(String)"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                                (Gren.Syntax.TypeAnnotation.Typed
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } ( [], "List" ))
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 13 } }
                                        (Gren.Syntax.TypeAnnotation.Typed
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 12 } } ( [], "String" ))
                                            []
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "parse type with multiple params"
                (\() ->
                    "Dict String Int"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 16 } }
                                (Gren.Syntax.TypeAnnotation.Typed
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } ( [], "Dict" ))
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 12 } }
                                        (Gren.Syntax.TypeAnnotation.Typed
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 12 } } ( [], "String" ))
                                            []
                                        )
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 16 } }
                                        (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 16 } } ( [], "Int" )) [])
                                    ]
                                )
                            )
                )
            ]
        , Test.describe "expression"
            [ Test.test "operatorToken 11 -- is not an operator"
                (\() ->
                    "a = (--)"
                        |> GrenParserLenient.run GrenParserLenient.declaration
                        |> Expect.equal Nothing
                )
            , Test.test "operatorToken 14"
                (\() ->
                    "a = (=)"
                        |> GrenParserLenient.run GrenParserLenient.declaration
                        |> Expect.equal Nothing
                )
            , Test.test "operatorToken 15"
                (\() ->
                    "a = (?)"
                        |> GrenParserLenient.run GrenParserLenient.declaration
                        |> Expect.equal Nothing
                )
            , Test.test "empty"
                (\() ->
                    "a = "
                        |> expectFailsToParse GrenParserLenient.declaration
                )
            , Test.describe "number"
                [ Test.test "long hex"
                    (\() ->
                        "0x03FFFFFF"
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> Gren.Syntax.Node.value)
                            |> Expect.equal
                                (Just (Gren.Syntax.Expression.Hex 67108863))
                    )
                , Test.test "hex FF"
                    (\() ->
                        "0xFF"
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> Gren.Syntax.Node.value)
                            |> Expect.equal
                                (Just (Gren.Syntax.Expression.Hex 255))
                    )
                , Test.test "hex"
                    (\() ->
                        "0x2A"
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> Gren.Syntax.Node.value)
                            |> Expect.equal
                                (Just (Gren.Syntax.Expression.Hex 42))
                    )
                , Test.test "Hex integer literal"
                    (\() ->
                        "0x56"
                            |> expectSyntaxWithoutComments GrenParserLenient.expression (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } (Gren.Syntax.Expression.Hex 86))
                    )
                , Test.test "Integer literal"
                    (\() ->
                        "101"
                            |> expectSyntaxWithoutComments GrenParserLenient.expression (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Gren.Syntax.Expression.Integer 101))
                    )
                , Test.test "float"
                    (\() ->
                        "2.0"
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> Gren.Syntax.Node.value)
                            |> Expect.equal
                                (Just (Gren.Syntax.Expression.Floatable 2.0))
                    )
                , Test.test "integer with negative exponent"
                    (\() ->
                        "2e-2"
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> Gren.Syntax.Node.value)
                            |> Expect.equal
                                (Just (Gren.Syntax.Expression.Floatable 0.02))
                    )
                , Test.test "literal e is not a number"
                    (\() ->
                        "e"
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> Gren.Syntax.Node.value)
                            |> Expect.equal
                                (Just (Gren.Syntax.Expression.FunctionOrValue [] "e"))
                    )
                , Test.test "integer with negative exponent (uppercase E)"
                    (\() ->
                        "2E-2"
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> Gren.Syntax.Node.value)
                            |> Expect.equal
                                (Just (Gren.Syntax.Expression.Floatable 0.02))
                    )
                , Test.test "integer with positive exponent"
                    (\() ->
                        "2e+2"
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> Gren.Syntax.Node.value)
                            |> Expect.equal
                                (Just (Gren.Syntax.Expression.Floatable 200.0))
                    )
                , Test.test "float with negative exponent"
                    (\() ->
                        "2.0e-2"
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> Gren.Syntax.Node.value)
                            |> Expect.equal
                                (Just (Gren.Syntax.Expression.Floatable 0.02))
                    )
                , Test.test "float with negative exponent (uppercase E)"
                    (\() ->
                        "2.0E-2"
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> Gren.Syntax.Node.value)
                            |> Expect.equal
                                (Just (Gren.Syntax.Expression.Floatable 0.02))
                    )
                , Test.test "float with positive exponent"
                    (\() ->
                        "2.0e+2"
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> Gren.Syntax.Node.value)
                            |> Expect.equal
                                (Just (Gren.Syntax.Expression.Floatable 200.0))
                    )
                ]
            , Test.test "String literal"
                (\() ->
                    "\"Bar\""
                        |> expectSyntaxWithoutComments GrenParserLenient.expression (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } (Gren.Syntax.Expression.Literal "Bar"))
                )
            , Test.test "multiline string"
                (\() ->
                    "\"\"\"Bar foo \n a\"\"\""
                        |> GrenParserLenient.run GrenParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> Gren.Syntax.Node.value)
                        |> Expect.equal (Just (Gren.Syntax.Expression.Literal "Bar foo \n a"))
                )
            , Test.test "multiline string escape"
                (\() ->
                    """\"\"\" \\\"\"\" \"\"\""""
                        |> GrenParserLenient.run GrenParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> Gren.Syntax.Node.value)
                        |> Expect.equal (Just (Gren.Syntax.Expression.Literal """ \"\"\" """))
                )
            , Test.test "character escaped"
                (\() ->
                    "'\\''"
                        |> GrenParserLenient.run GrenParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> Gren.Syntax.Node.value)
                        |> Expect.equal (Just (Gren.Syntax.Expression.CharLiteral '\''))
                )
            , Test.test "character escaped - 2"
                (\() ->
                    "'\\r'"
                        |> GrenParserLenient.run GrenParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> Gren.Syntax.Node.value)
                        |> Expect.equal (Just (Gren.Syntax.Expression.CharLiteral '\u{000D}'))
                )
            , Test.test "unicode char"
                (\() ->
                    "'\\u{000D}'"
                        |> GrenParserLenient.run GrenParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> Gren.Syntax.Node.value)
                        |> Expect.equal (Just (Gren.Syntax.Expression.CharLiteral '\u{000D}'))
                )
            , Test.test "unicode char with lowercase hex"
                (\() ->
                    "'\\u{000d}'"
                        |> GrenParserLenient.run GrenParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> Gren.Syntax.Node.value)
                        |> Expect.equal (Just (Gren.Syntax.Expression.CharLiteral '\u{000D}'))
                )
            , Test.test "string escaped 3"
                (\() ->
                    "\"\\\"\""
                        |> GrenParserLenient.run GrenParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> Gren.Syntax.Node.value)
                        |> Expect.equal (Just (Gren.Syntax.Expression.Literal "\""))
                )
            , Test.test "string escaped"
                (\() ->
                    "\"foo\\\\\""
                        |> GrenParserLenient.run GrenParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> Gren.Syntax.Node.value)
                        |> Expect.equal (Just (Gren.Syntax.Expression.Literal "foo\\"))
                )
            , Test.test "character escaped 3"
                (\() ->
                    "'\\n'"
                        |> GrenParserLenient.run GrenParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> Gren.Syntax.Node.value)
                        |> Expect.equal (Just (Gren.Syntax.Expression.CharLiteral '\n'))
                )
            , Test.test "long string"
                (\() ->
                    longString
                        |> GrenParserLenient.run GrenParserLenient.expression
                        |> Expect.notEqual Nothing
                )
            , Test.test "long multi line string"
                (\() ->
                    longMultiLineString
                        |> GrenParserLenient.run GrenParserLenient.expression
                        |> Expect.notEqual Nothing
                )
            , Test.test "character literal"
                (\() ->
                    "'c'"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Gren.Syntax.Expression.CharLiteral 'c'))
                )
            , Test.test "tuple expression"
                (\() ->
                    "(1,2)"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                                (Gren.Syntax.Expression.TupledExpression
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (Gren.Syntax.Expression.Integer 1)
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 4 }, end = { row = 1, column = 5 } } (Gren.Syntax.Expression.Integer 2)
                                    ]
                                )
                            )
                )
            , Test.test "triple expression"
                (\() ->
                    "(1,2,3)"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                                (Gren.Syntax.Expression.TupledExpression
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (Gren.Syntax.Expression.Integer 1)
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 4 }, end = { row = 1, column = 5 } } (Gren.Syntax.Expression.Integer 2)
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } (Gren.Syntax.Expression.Integer 3)
                                    ]
                                )
                            )
                )
            , Test.test "tuple expression with spaces"
                (\() ->
                    "( 1  ,  2 )"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                (Gren.Syntax.Expression.TupledExpression
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (Gren.Syntax.Expression.Integer 1)
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (Gren.Syntax.Expression.Integer 2)
                                    ]
                                )
                            )
                )
            , Test.test "4-tuple expression is invalid"
                (\() ->
                    "a = (1,2,3,4)"
                        |> expectFailsToParse GrenParserLenient.declaration
                )
            , Test.test "String literal multiline"
                (\() ->
                    "\"\"\"Bar foo \n a\"\"\""
                        |> expectSyntaxWithoutComments GrenParserLenient.expression (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } (Gren.Syntax.Expression.Literal "Bar foo \n a"))
                )
            , Test.test "Regression test for multiline strings with backslashes"
                (\() ->
                    "a = \"\"\"\\{\\}\"\"\""
                        |> expectFailsToParse GrenParserLenient.declaration
                )
            , Test.test "Regression test 2 for multiline strings with backslashes"
                (\() ->
                    "\"\"\"\\\\{\\\\}\"\"\""
                        |> expectSyntaxWithoutComments GrenParserLenient.expression (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } } (Gren.Syntax.Expression.Literal "\\{\\}"))
                )
            , Test.test "Regression test 3 for multiline strings with backslashes"
                (\() ->
                    "\"\"\"\\\\a-blablabla-\\\\b\"\"\""
                        |> expectSyntaxWithoutComments GrenParserLenient.expression (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 24 } } (Gren.Syntax.Expression.Literal "\\a-blablabla-\\b"))
                )
            , Test.test "Type expression for upper case"
                (\() ->
                    "Bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Gren.Syntax.Expression.FunctionOrValue [] "Bar"))
                )
            , Test.test "Type expression for lower case"
                (\() ->
                    "bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Gren.Syntax.Expression.FunctionOrValue [] "bar"))
                )
            , Test.test "Type expression for lower case but qualified"
                (\() ->
                    "Bar.foo"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } } (Gren.Syntax.Expression.FunctionOrValue [ "Bar" ] "foo"))
                )
            , Test.test "parenthesizedExpression"
                (\() ->
                    "(bar)"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                                (Gren.Syntax.Expression.ParenthesizedExpression
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 5 } } (Gren.Syntax.Expression.FunctionOrValue [] "bar"))
                                )
                            )
                )
            , Test.test "parenthesized expression starting with a negation"
                (\() ->
                    "(-1 * sign)"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                (Gren.Syntax.Expression.ParenthesizedExpression
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 11 } }
                                        (Gren.Syntax.Expression.OperatorApplication "*"
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } }
                                                (Gren.Syntax.Expression.Negation (Gren.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (Gren.Syntax.Expression.Integer 1)))
                                            )
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 11 } } (Gren.Syntax.Expression.FunctionOrValue [] "sign"))
                                        )
                                    )
                                )
                            )
                )
            , Test.test "application expression"
                (\() ->
                    "List.concat []"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                                (Gren.Syntax.Expression.Application
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } (Gren.Syntax.Expression.FunctionOrValue [ "List" ] "concat")
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 15 } } (Gren.Syntax.Expression.ListExpr [])
                                    ]
                                )
                            )
                )
            , Test.test "Binary operation"
                (\() ->
                    "model + 1"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                                (Gren.Syntax.Expression.OperatorApplication "+"
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } (Gren.Syntax.Expression.FunctionOrValue [] "model"))
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (Gren.Syntax.Expression.Integer 1))
                                )
                            )
                )
            , Test.test "Nested binary operations (+ and ==)"
                (\() ->
                    "count + 1 == 1"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                                (Gren.Syntax.Expression.OperatorApplication "=="
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                                        (Gren.Syntax.Expression.OperatorApplication "+"
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } (Gren.Syntax.Expression.FunctionOrValue [] "count"))
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (Gren.Syntax.Expression.Integer 1))
                                        )
                                    )
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } } (Gren.Syntax.Expression.Integer 1))
                                )
                            )
                )
            , Test.test "Nested binary operations (+ and /=)"
                (\() ->
                    "count + 1 /= 1"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                                (Gren.Syntax.Expression.OperatorApplication "/="
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                                        (Gren.Syntax.Expression.OperatorApplication "+"
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } (Gren.Syntax.Expression.FunctionOrValue [] "count"))
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (Gren.Syntax.Expression.Integer 1))
                                        )
                                    )
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } } (Gren.Syntax.Expression.Integer 1))
                                )
                            )
                )
            , Test.test "Nested binary operations (+ and //)"
                (\() ->
                    "count + 1 // 2"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                                (Gren.Syntax.Expression.OperatorApplication "+"
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } (Gren.Syntax.Expression.FunctionOrValue [] "count"))
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 15 } }
                                        (Gren.Syntax.Expression.OperatorApplication "//"
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (Gren.Syntax.Expression.Integer 1))
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } } (Gren.Syntax.Expression.Integer 2))
                                        )
                                    )
                                )
                            )
                )
            , Test.test "Nested binary operations (&& and <|)"
                (\() ->
                    "condition && condition <| f"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 28 } }
                                (Gren.Syntax.Expression.OperatorApplication "<|"
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 23 } }
                                        (Gren.Syntax.Expression.OperatorApplication "&&"
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } } (Gren.Syntax.Expression.FunctionOrValue [] "condition"))
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 23 } } (Gren.Syntax.Expression.FunctionOrValue [] "condition"))
                                        )
                                    )
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 27 }, end = { row = 1, column = 28 } } (Gren.Syntax.Expression.FunctionOrValue [] "f"))
                                )
                            )
                )
            , Test.test "application expression 2"
                (\() ->
                    "(\"\", always (List.concat [ [ fileName ], [] ]))"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 48 } }
                                (Gren.Syntax.Expression.TupledExpression
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } } (Gren.Syntax.Expression.Literal "")
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 47 } }
                                        (Gren.Syntax.Expression.Application
                                            [ Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 12 } } (Gren.Syntax.Expression.FunctionOrValue [] "always")
                                            , Gren.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 47 } }
                                                (Gren.Syntax.Expression.ParenthesizedExpression
                                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 46 } }
                                                        (Gren.Syntax.Expression.Application
                                                            [ Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } } (Gren.Syntax.Expression.FunctionOrValue [ "List" ] "concat")
                                                            , Gren.Syntax.Node.Node { start = { row = 1, column = 26 }, end = { row = 1, column = 46 } }
                                                                (Gren.Syntax.Expression.ListExpr
                                                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 28 }, end = { row = 1, column = 40 } }
                                                                        (Gren.Syntax.Expression.ListExpr
                                                                            [ Gren.Syntax.Node.Node { start = { row = 1, column = 30 }, end = { row = 1, column = 38 } } (Gren.Syntax.Expression.FunctionOrValue [] "fileName")
                                                                            ]
                                                                        )
                                                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 42 }, end = { row = 1, column = 44 } } (Gren.Syntax.Expression.ListExpr [])
                                                                    ]
                                                                )
                                                            ]
                                                        )
                                                    )
                                                )
                                            ]
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "expressionNotApplication simple"
                (\() ->
                    "foo"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Gren.Syntax.Expression.FunctionOrValue [] "foo"))
                )
            , Test.test "unit application"
                (\() ->
                    "Task.succeed ()"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 16 } }
                                (Gren.Syntax.Expression.Application
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } } (Gren.Syntax.Expression.FunctionOrValue [ "Task" ] "succeed")
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 16 } } Gren.Syntax.Expression.UnitExpr
                                    ]
                                )
                            )
                )
            , Test.test "Function call"
                (\() ->
                    "foo bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                                (Gren.Syntax.Expression.Application
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Gren.Syntax.Expression.FunctionOrValue [] "foo")
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 8 } } (Gren.Syntax.Expression.FunctionOrValue [] "bar")
                                    ]
                                )
                            )
                )
            , Test.test "Function call with argument badly indented"
                (\() ->
                    "a = foo\nbar"
                        |> expectFailsToParse GrenParserLenient.declaration
                )
            , Test.test "ifBlockExpression"
                (\() ->
                    "if True then foo else bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 26 } }
                                (Gren.Syntax.Expression.IfBlock
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 4 }, end = { row = 1, column = 8 } } (Gren.Syntax.Expression.FunctionOrValue [] "True"))
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 17 } } (Gren.Syntax.Expression.FunctionOrValue [] "foo"))
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 23 }, end = { row = 1, column = 26 } } (Gren.Syntax.Expression.FunctionOrValue [] "bar"))
                                )
                            )
                )
            , Test.test "if-then-else with -> instead of then"
                (\() ->
                    "if True ->   foo else bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 26 } }
                                (Gren.Syntax.Expression.IfBlock
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 4 }, end = { row = 1, column = 8 } } (Gren.Syntax.Expression.FunctionOrValue [] "True"))
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 17 } } (Gren.Syntax.Expression.FunctionOrValue [] "foo"))
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 23 }, end = { row = 1, column = 26 } } (Gren.Syntax.Expression.FunctionOrValue [] "bar"))
                                )
                            )
                )
            , Test.test "nestedIfExpression"
                (\() ->
                    "if True then if False then foo else baz else bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 49 } }
                                (Gren.Syntax.Expression.IfBlock
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 4 }, end = { row = 1, column = 8 } } (Gren.Syntax.Expression.FunctionOrValue [] "True"))
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 40 } }
                                        (Gren.Syntax.Expression.IfBlock
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 17 }, end = { row = 1, column = 22 } } (Gren.Syntax.Expression.FunctionOrValue [] "False"))
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } } (Gren.Syntax.Expression.FunctionOrValue [] "foo"))
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 37 }, end = { row = 1, column = 40 } } (Gren.Syntax.Expression.FunctionOrValue [] "baz"))
                                        )
                                    )
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 46 }, end = { row = 1, column = 49 } } (Gren.Syntax.Expression.FunctionOrValue [] "bar"))
                                )
                            )
                )
            , Test.test "recordExpression"
                (\() ->
                    "{ model = 0, view = view, update = update }"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 44 } }
                                (Gren.Syntax.Expression.RecordExpr
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 12 } }
                                        ( Gren.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 8 } } "model"
                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 12 } } (Gren.Syntax.Expression.Integer 0)
                                        )
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } }
                                        ( Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 18 } } "view"
                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } } (Gren.Syntax.Expression.FunctionOrValue [] "view")
                                        )
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 27 }, end = { row = 1, column = 43 } }
                                        ( Gren.Syntax.Node.Node { start = { row = 1, column = 27 }, end = { row = 1, column = 33 } } "update"
                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 36 }, end = { row = 1, column = 42 } } (Gren.Syntax.Expression.FunctionOrValue [] "update")
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "recordExpression with comment"
                (\() ->
                    "{ foo = 1 -- bar\n , baz = 2 }"
                        |> expectSyntaxWithComments GrenParserLenient.expression
                            { syntax =
                                Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 13 } }
                                    (Gren.Syntax.Expression.RecordExpr
                                        [ Gren.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 10 } }
                                            ( Gren.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 6 } } "foo"
                                            , Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (Gren.Syntax.Expression.Integer 1)
                                            )
                                        , Gren.Syntax.Node.Node { start = { row = 2, column = 4 }, end = { row = 2, column = 12 } }
                                            ( Gren.Syntax.Node.Node { start = { row = 2, column = 4 }, end = { row = 2, column = 7 } } "baz"
                                            , Gren.Syntax.Node.Node { start = { row = 2, column = 10 }, end = { row = 2, column = 11 } } (Gren.Syntax.Expression.Integer 2)
                                            )
                                        ]
                                    )
                            , comments = [ Gren.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 17 } } "-- bar" ]
                            }
                )
            , Test.test "listExpression"
                (\() ->
                    "[ class \"a\", text \"Foo\"]"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                (Gren.Syntax.Expression.ListExpr
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 12 } }
                                        (Gren.Syntax.Expression.Application
                                            [ Gren.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 8 } } (Gren.Syntax.Expression.FunctionOrValue [] "class")
                                            , Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } (Gren.Syntax.Expression.Literal "a")
                                            ]
                                        )
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 24 } }
                                        (Gren.Syntax.Expression.Application
                                            [ Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 18 } } (Gren.Syntax.Expression.FunctionOrValue [] "text")
                                            , Gren.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 24 } } (Gren.Syntax.Expression.Literal "Foo")
                                            ]
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "list with extra comma between elements"
                (\() ->
                    "[ class \"a\",,text \"Foo\"]"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                (Gren.Syntax.Expression.ListExpr
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 12 } }
                                        (Gren.Syntax.Expression.Application
                                            [ Gren.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 8 } } (Gren.Syntax.Expression.FunctionOrValue [] "class")
                                            , Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } (Gren.Syntax.Expression.Literal "a")
                                            ]
                                        )
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 24 } }
                                        (Gren.Syntax.Expression.Application
                                            [ Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 18 } } (Gren.Syntax.Expression.FunctionOrValue [] "text")
                                            , Gren.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 24 } } (Gren.Syntax.Expression.Literal "Foo")
                                            ]
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "listExpression singleton with comment"
                (\() ->
                    "[ 1 {- Foo-} ]"
                        |> expectSyntaxWithComments GrenParserLenient.expression
                            { syntax =
                                Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                                    (Gren.Syntax.Expression.ListExpr
                                        [ Gren.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (Gren.Syntax.Expression.Integer 1)
                                        ]
                                    )
                            , comments = [ Gren.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 13 } } "{- Foo-}" ]
                            }
                )
            , Test.test "list with extra prefix comma and comment"
                (\() ->
                    "[,1 {- Foo-} ]"
                        |> expectSyntaxWithComments GrenParserLenient.expression
                            { syntax =
                                Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                                    (Gren.Syntax.Expression.ListExpr
                                        [ Gren.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (Gren.Syntax.Expression.Integer 1)
                                        ]
                                    )
                            , comments = [ Gren.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 13 } } "{- Foo-}" ]
                            }
                )
            , Test.test "listExpression empty with comment"
                (\() ->
                    "[{- Foo -}]"
                        |> expectSyntaxWithComments GrenParserLenient.expression
                            { syntax = Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } (Gren.Syntax.Expression.ListExpr [])
                            , comments = [ Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 11 } } "{- Foo -}" ]
                            }
                )
            , Test.test "qualified expression"
                (\() ->
                    "Html.text"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                                (Gren.Syntax.Expression.FunctionOrValue [ "Html" ] "text")
                            )
                )
            , Test.test "qualified expression with name colliding with keyword"
                (\() ->
                    "Html.Attributes.type"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 21 } }
                                (Gren.Syntax.Expression.FunctionOrValue [ "Html", "Attributes" ] "type_")
                            )
                )
            , Test.test "record access"
                (\() ->
                    "foo.bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                                (Gren.Syntax.Expression.RecordAccess
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Gren.Syntax.Expression.FunctionOrValue [] "foo"))
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 8 } } "bar")
                                )
                            )
                )
            , Test.test "record access with field name colliding with keyword"
                (\() ->
                    "foo.exposing"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                                (Gren.Syntax.Expression.RecordAccess
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Gren.Syntax.Expression.FunctionOrValue [] "foo"))
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 13 } } "exposing_")
                                )
                            )
                )
            , Test.test "multiple record access operations"
                (\() ->
                    "foo.bar.baz"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                (Gren.Syntax.Expression.RecordAccess
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                                        (Gren.Syntax.Expression.RecordAccess
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Gren.Syntax.Expression.FunctionOrValue [] "foo"))
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 8 } } "bar")
                                        )
                                    )
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } "baz")
                                )
                            )
                )
            , Test.test "multiple record access operations with module name"
                (\() ->
                    "A.B.foo.bar.baz"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 16 } }
                                (Gren.Syntax.Expression.RecordAccess
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                        (Gren.Syntax.Expression.RecordAccess
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } } (Gren.Syntax.Expression.FunctionOrValue [ "A", "B" ] "foo"))
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } "bar")
                                        )
                                    )
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 16 } } "baz")
                                )
                            )
                )
            , Test.test "record with name-value separator : and name-value separator = and name-value separator empty and punned fields with each of those"
                (\() ->
                    "{ a 1, b = 2, c : 3, aPunned, bPunned =, cPunned : }"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { end = { column = 53, row = 1 }, start = { column = 1, row = 1 } }
                                (Gren.Syntax.Expression.RecordExpr
                                    [ Gren.Syntax.Node.Node { end = { column = 6, row = 1 }, start = { column = 3, row = 1 } }
                                        ( Gren.Syntax.Node.Node { end = { column = 4, row = 1 }, start = { column = 3, row = 1 } } "a"
                                        , Gren.Syntax.Node.Node { end = { column = 6, row = 1 }, start = { column = 5, row = 1 } }
                                            (Gren.Syntax.Expression.Integer 1)
                                        )
                                    , Gren.Syntax.Node.Node { end = { column = 13, row = 1 }, start = { column = 8, row = 1 } }
                                        ( Gren.Syntax.Node.Node { end = { column = 9, row = 1 }, start = { column = 8, row = 1 } } "b"
                                        , Gren.Syntax.Node.Node { end = { column = 13, row = 1 }, start = { column = 12, row = 1 } }
                                            (Gren.Syntax.Expression.Integer 2)
                                        )
                                    , Gren.Syntax.Node.Node { end = { column = 20, row = 1 }, start = { column = 15, row = 1 } }
                                        ( Gren.Syntax.Node.Node { end = { column = 16, row = 1 }, start = { column = 15, row = 1 } } "c"
                                        , Gren.Syntax.Node.Node { end = { column = 20, row = 1 }, start = { column = 19, row = 1 } }
                                            (Gren.Syntax.Expression.Integer 3)
                                        )
                                    , Gren.Syntax.Node.Node { end = { column = 29, row = 1 }, start = { column = 22, row = 1 } }
                                        ( Gren.Syntax.Node.Node { end = { column = 29, row = 1 }, start = { column = 22, row = 1 } } "aPunned"
                                        , Gren.Syntax.Node.Node { end = { column = 29, row = 1 }, start = { column = 29, row = 1 } }
                                            (Gren.Syntax.Expression.FunctionOrValue [] "aPunned")
                                        )
                                    , Gren.Syntax.Node.Node { end = { column = 40, row = 1 }, start = { column = 31, row = 1 } }
                                        ( Gren.Syntax.Node.Node { end = { column = 38, row = 1 }, start = { column = 31, row = 1 } } "bPunned"
                                        , Gren.Syntax.Node.Node { end = { column = 38, row = 1 }, start = { column = 38, row = 1 } }
                                            (Gren.Syntax.Expression.FunctionOrValue [] "bPunned")
                                        )
                                    , Gren.Syntax.Node.Node { end = { column = 52, row = 1 }, start = { column = 42, row = 1 } }
                                        ( Gren.Syntax.Node.Node { end = { column = 49, row = 1 }, start = { column = 42, row = 1 } } "cPunned"
                                        , Gren.Syntax.Node.Node { end = { column = 49, row = 1 }, start = { column = 49, row = 1 } }
                                            (Gren.Syntax.Expression.FunctionOrValue [] "cPunned")
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "record with prefix comma"
                (\() ->
                    "{ , a = 1, b = 2 }"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { end = { column = 19, row = 1 }, start = { column = 1, row = 1 } }
                                (Gren.Syntax.Expression.RecordExpr
                                    [ Gren.Syntax.Node.Node { end = { column = 10, row = 1 }, start = { column = 5, row = 1 } }
                                        ( Gren.Syntax.Node.Node { end = { column = 6, row = 1 }, start = { column = 5, row = 1 } } "a"
                                        , Gren.Syntax.Node.Node { end = { column = 10, row = 1 }, start = { column = 9, row = 1 } }
                                            (Gren.Syntax.Expression.Integer 1)
                                        )
                                    , Gren.Syntax.Node.Node { end = { column = 18, row = 1 }, start = { column = 12, row = 1 } }
                                        ( Gren.Syntax.Node.Node { end = { column = 13, row = 1 }, start = { column = 12, row = 1 } } "b"
                                        , Gren.Syntax.Node.Node { end = { column = 17, row = 1 }, start = { column = 16, row = 1 } }
                                            (Gren.Syntax.Expression.Integer 2)
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "record with extra comma between fields"
                (\() ->
                    "{   a = 1,,b = 2 }"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { end = { column = 19, row = 1 }, start = { column = 1, row = 1 } }
                                (Gren.Syntax.Expression.RecordExpr
                                    [ Gren.Syntax.Node.Node { end = { column = 10, row = 1 }, start = { column = 5, row = 1 } }
                                        ( Gren.Syntax.Node.Node { end = { column = 6, row = 1 }, start = { column = 5, row = 1 } } "a"
                                        , Gren.Syntax.Node.Node { end = { column = 10, row = 1 }, start = { column = 9, row = 1 } }
                                            (Gren.Syntax.Expression.Integer 1)
                                        )
                                    , Gren.Syntax.Node.Node { end = { column = 18, row = 1 }, start = { column = 12, row = 1 } }
                                        ( Gren.Syntax.Node.Node { end = { column = 13, row = 1 }, start = { column = 12, row = 1 } } "b"
                                        , Gren.Syntax.Node.Node { end = { column = 17, row = 1 }, start = { column = 16, row = 1 } }
                                            (Gren.Syntax.Expression.Integer 2)
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "record update"
                (\() ->
                    "{ model | count = 1, loading = True }"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 38 } }
                                (Gren.Syntax.Expression.RecordUpdateExpression
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 8 } } "model")
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 20 } }
                                        ( Gren.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 16 } } "count"
                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } } (Gren.Syntax.Expression.Integer 1)
                                        )
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 22 }, end = { row = 1, column = 37 } }
                                        ( Gren.Syntax.Node.Node { start = { row = 1, column = 22 }, end = { row = 1, column = 29 } } "loading"
                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 32 }, end = { row = 1, column = 36 } } (Gren.Syntax.Expression.FunctionOrValue [] "True")
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "record update with extra comma between fields"
                (\() ->
                    "{ model | count = 1,,loading = True }"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 38 } }
                                (Gren.Syntax.Expression.RecordUpdateExpression
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 8 } } "model")
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 20 } }
                                        ( Gren.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 16 } } "count"
                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } } (Gren.Syntax.Expression.Integer 1)
                                        )
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 22 }, end = { row = 1, column = 37 } }
                                        ( Gren.Syntax.Node.Node { start = { row = 1, column = 22 }, end = { row = 1, column = 29 } } "loading"
                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 32 }, end = { row = 1, column = 36 } } (Gren.Syntax.Expression.FunctionOrValue [] "True")
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "record update with name-value separator : and name-value separator empty"
                (\() ->
                    "{ model | count : 1, loading   True }"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 38 } }
                                (Gren.Syntax.Expression.RecordUpdateExpression
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 8 } } "model")
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 20 } }
                                        ( Gren.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 16 } } "count"
                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } } (Gren.Syntax.Expression.Integer 1)
                                        )
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 22 }, end = { row = 1, column = 37 } }
                                        ( Gren.Syntax.Node.Node { start = { row = 1, column = 22 }, end = { row = 1, column = 29 } } "loading"
                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 32 }, end = { row = 1, column = 36 } } (Gren.Syntax.Expression.FunctionOrValue [] "True")
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "record update no spacing"
                (\() ->
                    "{model| count = 1, loading = True }"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 36 } }
                                (Gren.Syntax.Expression.RecordUpdateExpression
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } "model")
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 18 } }
                                        ( Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 14 } } "count"
                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 17 }, end = { row = 1, column = 18 } } (Gren.Syntax.Expression.Integer 1)
                                        )
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 20 }, end = { row = 1, column = 35 } }
                                        ( Gren.Syntax.Node.Node { start = { row = 1, column = 20 }, end = { row = 1, column = 27 } } "loading"
                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 30 }, end = { row = 1, column = 34 } } (Gren.Syntax.Expression.FunctionOrValue [] "True")
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "record access as function"
                (\() ->
                    "List.map .name people"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 22 } }
                                (Gren.Syntax.Expression.Application
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 9 } } (Gren.Syntax.Expression.FunctionOrValue [ "List" ] "map")
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } } (Gren.Syntax.Expression.RecordAccessFunction ".name")
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 22 } } (Gren.Syntax.Expression.FunctionOrValue [] "people")
                                    ]
                                )
                            )
                )
            , Test.test "record access direct"
                (\() ->
                    "(.spaceEvenly Internal.Style.classes)"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 38 } }
                                (Gren.Syntax.Expression.ParenthesizedExpression
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 37 } }
                                        (Gren.Syntax.Expression.Application
                                            [ Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 14 } } (Gren.Syntax.Expression.RecordAccessFunction ".spaceEvenly")
                                            , Gren.Syntax.Node.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 37 } } (Gren.Syntax.Expression.FunctionOrValue [ "Internal", "Style" ] "classes")
                                            ]
                                        )
                                    )
                                )
                            )
                )
            , Test.test "positive integer should be invalid"
                (\() ->
                    "a = +1"
                        |> expectFailsToParse GrenParserLenient.declaration
                )
            , Test.test "expression ending with an operator should not be valid"
                (\() ->
                    "a = 1++"
                        |> expectFailsToParse GrenParserLenient.declaration
                )
            , Test.test "multiple < in a row should not be valid"
                (\() ->
                    "z = a < b < c"
                        |> expectFailsToParse GrenParserLenient.declaration
                )
            , Test.test "multiple > in a row should not be valid"
                (\() ->
                    "z = a > b > c"
                        |> expectFailsToParse GrenParserLenient.declaration
                )
            , Test.test "multiple == in a row should not be valid"
                (\() ->
                    "z = a == b == c"
                        |> expectFailsToParse GrenParserLenient.declaration
                )
            , Test.test "multiple /= in a row should not be valid"
                (\() ->
                    "z = a /= b /= c"
                        |> expectFailsToParse GrenParserLenient.declaration
                )
            , Test.test "multiple >= in a row should not be valid"
                (\() ->
                    "z = a >= b >= c"
                        |> expectFailsToParse GrenParserLenient.declaration
                )
            , Test.test "multiple <= in a row should not be valid"
                (\() ->
                    "z = a <= b <= c"
                        |> expectFailsToParse GrenParserLenient.declaration
                )
            , Test.test "mixing comparison operators without parenthesis should not be valid"
                (\() ->
                    "z = a < b == c"
                        |> expectFailsToParse GrenParserLenient.declaration
                )
            , -- TODO introduce validation step for
              -- , test "<| followed by |> operation without parenthesis should not be valid" <|
              --     \() ->
              --         "z = a <| b |> c"
              --             |> ParserWithCommentsUtil.expectInvalid GrenParserLenient.declaration
              -- , test "|> followed by <| operation without parenthesis should not be valid" <|
              --     \() ->
              --         "z = a |> b <| c"
              --             |> ParserWithCommentsUtil.expectInvalid GrenParserLenient.declaration
              -- , test "<< followed by >> operation without parenthesis should not be valid" <|
              --     \() ->
              --         "z = a << b >> c"
              --             |> ParserWithCommentsUtil.expectInvalid GrenParserLenient.declaration
              -- , test ">> followed by << operation without parenthesis should not be valid" <|
              --     \() ->
              --         "z = a >> b << c"
              --             |> ParserWithCommentsUtil.expectInvalid GrenParserLenient.declaration
              Test.test "prefix notation"
                (\() ->
                    "(::) x"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                                (Gren.Syntax.Expression.Application
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } (Gren.Syntax.Expression.PrefixOperator "::")
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } (Gren.Syntax.Expression.FunctionOrValue [] "x")
                                    ]
                                )
                            )
                )
            , Test.test "subtraction without spaces"
                (\() ->
                    "2-1"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                (Gren.Syntax.Expression.OperatorApplication "-"
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } (Gren.Syntax.Expression.Integer 2))
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (Gren.Syntax.Expression.Integer 1))
                                )
                            )
                )
            , Test.test "negated expression after comma"
                (\() ->
                    "a = (0,-x)"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                                (GrenSyntax.FunctionDeclaration
                                    { signature = Nothing
                                    , documentation = Nothing
                                    , declaration =
                                        Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                                            { name = Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } "a"
                                            , arguments = []
                                            , expression =
                                                Gren.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 11 } }
                                                    (Gren.Syntax.Expression.TupledExpression
                                                        [ Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } }
                                                            (Gren.Syntax.Expression.Integer 0)
                                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 10 } }
                                                            (Gren.Syntax.Expression.Negation
                                                                (Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } }
                                                                    (Gren.Syntax.Expression.FunctionOrValue [] "x")
                                                                )
                                                            )
                                                        ]
                                                    )
                                            }
                                    }
                                )
                            )
                )
            , Test.test "negated expression after = in record"
                (\() ->
                    "{a=-x}"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                                (Gren.Syntax.Expression.RecordExpr
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 6 } }
                                        ( Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } "a"
                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 4 }, end = { row = 1, column = 6 } }
                                            (Gren.Syntax.Expression.Negation
                                                (Gren.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } }
                                                    (Gren.Syntax.Expression.FunctionOrValue [] "x")
                                                )
                                            )
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "negated expression after list"
                (\() ->
                    "List.sum [a,b]-x"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 17 } }
                                (Gren.Syntax.Expression.OperatorApplication "-"
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                                        (Gren.Syntax.Expression.Application
                                            [ Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 9 } }
                                                (Gren.Syntax.Expression.FunctionOrValue [ "List" ] "sum")
                                            , Gren.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } }
                                                (Gren.Syntax.Expression.ListExpr
                                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 12 } }
                                                        (Gren.Syntax.Expression.FunctionOrValue [] "a")
                                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 14 } }
                                                        (Gren.Syntax.Expression.FunctionOrValue [] "b")
                                                    ]
                                                )
                                            ]
                                        )
                                    )
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 17 } }
                                        (Gren.Syntax.Expression.FunctionOrValue [] "x")
                                    )
                                )
                            )
                )
            , Test.test "negated expression after = in let declaration"
                (\() ->
                    "let a=-x in a"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 14 } }
                                (Gren.Syntax.Expression.LetExpression
                                    { declarations =
                                        [ Gren.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 9 } }
                                            (Gren.Syntax.Expression.LetFunction
                                                { documentation = Nothing
                                                , signature = Nothing
                                                , declaration =
                                                    Gren.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 9 } }
                                                        { name = Gren.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } "a"
                                                        , arguments = []
                                                        , expression =
                                                            Gren.Syntax.Node.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 9 } }
                                                                (Gren.Syntax.Expression.Negation
                                                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } }
                                                                        (Gren.Syntax.Expression.FunctionOrValue [] "x")
                                                                    )
                                                                )
                                                        }
                                                }
                                            )
                                        ]
                                    , expression =
                                        Gren.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 14 } }
                                            (Gren.Syntax.Expression.FunctionOrValue [] "a")
                                    }
                                )
                            )
                )
            , Test.test "negated expression after -> in case branch"
                (\() ->
                    "case 1 of\n    _->-a"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 10 } }
                                (Gren.Syntax.Expression.CaseExpression
                                    { expression =
                                        Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } }
                                            (Gren.Syntax.Expression.Integer 1)
                                    , cases =
                                        [ ( Gren.Syntax.Node.Node { start = { row = 2, column = 5 }, end = { row = 2, column = 6 } }
                                                Gren.Syntax.Pattern.AllPattern
                                          , Gren.Syntax.Node.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 10 } }
                                                (Gren.Syntax.Expression.Negation
                                                    (Gren.Syntax.Node.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } }
                                                        (Gren.Syntax.Expression.FunctionOrValue [] "a")
                                                    )
                                                )
                                          )
                                        ]
                                    }
                                )
                            )
                )
            , Test.test "negated expression after `in` in let body"
                (\() ->
                    "let a=-x in-a"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { end = { column = 14, row = 1 }, start = { column = 1, row = 1 } }
                                (Gren.Syntax.Expression.LetExpression
                                    { declarations =
                                        [ Gren.Syntax.Node.Node { end = { column = 9, row = 1 }, start = { column = 5, row = 1 } }
                                            (Gren.Syntax.Expression.LetFunction
                                                { declaration =
                                                    Gren.Syntax.Node.Node { end = { column = 9, row = 1 }, start = { column = 5, row = 1 } }
                                                        { arguments = []
                                                        , expression =
                                                            Gren.Syntax.Node.Node { end = { column = 9, row = 1 }, start = { column = 7, row = 1 } }
                                                                (Gren.Syntax.Expression.Negation
                                                                    (Gren.Syntax.Node.Node { end = { column = 9, row = 1 }, start = { column = 8, row = 1 } }
                                                                        (Gren.Syntax.Expression.FunctionOrValue [] "x")
                                                                    )
                                                                )
                                                        , name = Gren.Syntax.Node.Node { end = { column = 6, row = 1 }, start = { column = 5, row = 1 } } "a"
                                                        }
                                                , documentation = Nothing
                                                , signature = Nothing
                                                }
                                            )
                                        ]
                                    , expression =
                                        Gren.Syntax.Node.Node { end = { column = 14, row = 1 }, start = { column = 12, row = 1 } }
                                            (Gren.Syntax.Expression.Negation
                                                (Gren.Syntax.Node.Node { end = { column = 14, row = 1 }, start = { column = 13, row = 1 } }
                                                    (Gren.Syntax.Expression.FunctionOrValue [] "a")
                                                )
                                            )
                                    }
                                )
                            )
                )
            , Test.test "negated expression for value"
                (\() ->
                    "a = -x"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                                (GrenSyntax.FunctionDeclaration
                                    { signature = Nothing
                                    , documentation = Nothing
                                    , declaration =
                                        Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                                            { name = Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } "a"
                                            , arguments = []
                                            , expression =
                                                Gren.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 7 } }
                                                    (Gren.Syntax.Expression.Negation
                                                        (Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } }
                                                            (Gren.Syntax.Expression.FunctionOrValue [] "x")
                                                        )
                                                    )
                                            }
                                    }
                                )
                            )
                )
            , Test.test "function declaration with negation sign after ="
                (\() ->
                    "foo=-1"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                                (GrenSyntax.FunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                                            { name = Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "foo"
                                            , arguments = []
                                            , expression =
                                                Gren.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 7 } }
                                                    (Gren.Syntax.Expression.Negation
                                                        (Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } }
                                                            (Gren.Syntax.Expression.Integer 1)
                                                        )
                                                    )
                                            }
                                    }
                                )
                            )
                )
            , Test.test "negated expression in application"
                (\() ->
                    "toFloat -5"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                                (Gren.Syntax.Expression.Application
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } } (Gren.Syntax.Expression.FunctionOrValue [] "toFloat")
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 11 } }
                                        (Gren.Syntax.Expression.Negation (Gren.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 11 } } (Gren.Syntax.Expression.Integer 5)))
                                    ]
                                )
                            )
                )
            , Test.test "negated expression for parenthesized"
                (\() ->
                    "a = -(x - y)"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                                (GrenSyntax.FunctionDeclaration
                                    { signature = Nothing
                                    , documentation = Nothing
                                    , declaration =
                                        Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                                            { name = Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } "a"
                                            , arguments = []
                                            , expression =
                                                Gren.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 13 } }
                                                    (Gren.Syntax.Expression.Negation
                                                        (Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 13 } }
                                                            (Gren.Syntax.Expression.ParenthesizedExpression
                                                                (Gren.Syntax.Node.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 12 } }
                                                                    (Gren.Syntax.Expression.OperatorApplication "-"
                                                                        (Gren.Syntax.Node.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 8 } }
                                                                            (Gren.Syntax.Expression.FunctionOrValue [] "x")
                                                                        )
                                                                        (Gren.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 12 } }
                                                                            (Gren.Syntax.Expression.FunctionOrValue [] "y")
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    )
                                            }
                                    }
                                )
                            )
                )
            , Test.test "negated expression with other operations"
                (\() ->
                    "a = -1 + -10 * -100^2 == -100001"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 33 } }
                                (GrenSyntax.FunctionDeclaration
                                    { signature = Nothing
                                    , documentation = Nothing
                                    , declaration =
                                        Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 33 } }
                                            { name = Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } "a"
                                            , arguments = []
                                            , expression =
                                                Gren.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 33 } }
                                                    (Gren.Syntax.Expression.OperatorApplication "=="
                                                        (Gren.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 22 } }
                                                            (Gren.Syntax.Expression.OperatorApplication "+"
                                                                (Gren.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 7 } }
                                                                    (Gren.Syntax.Expression.Negation
                                                                        (Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } }
                                                                            (Gren.Syntax.Expression.Integer 1)
                                                                        )
                                                                    )
                                                                )
                                                                (Gren.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 22 } }
                                                                    (Gren.Syntax.Expression.OperatorApplication "*"
                                                                        (Gren.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 13 } }
                                                                            (Gren.Syntax.Expression.Negation
                                                                                (Gren.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 13 } }
                                                                                    (Gren.Syntax.Expression.Integer 10)
                                                                                )
                                                                            )
                                                                        )
                                                                        (Gren.Syntax.Node.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 22 } }
                                                                            (Gren.Syntax.Expression.OperatorApplication "^"
                                                                                (Gren.Syntax.Node.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 20 } }
                                                                                    (Gren.Syntax.Expression.Negation
                                                                                        (Gren.Syntax.Node.Node { start = { row = 1, column = 17 }, end = { row = 1, column = 20 } }
                                                                                            (Gren.Syntax.Expression.Integer 100)
                                                                                        )
                                                                                    )
                                                                                )
                                                                                (Gren.Syntax.Node.Node { start = { row = 1, column = 21 }, end = { row = 1, column = 22 } }
                                                                                    (Gren.Syntax.Expression.Integer 2)
                                                                                )
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                        )
                                                        (Gren.Syntax.Node.Node { start = { row = 1, column = 26 }, end = { row = 1, column = 33 } }
                                                            (Gren.Syntax.Expression.Negation
                                                                (Gren.Syntax.Node.Node { start = { row = 1, column = 27 }, end = { row = 1, column = 33 } }
                                                                    (Gren.Syntax.Expression.Integer 100001)
                                                                )
                                                            )
                                                        )
                                                    )
                                            }
                                    }
                                )
                            )
                )
            , Test.test "plus and minus in the same expression"
                (\() ->
                    "1 + 2 - 3"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node
                                { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                                (Gren.Syntax.Expression.OperatorApplication "-"
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                                        (Gren.Syntax.Expression.OperatorApplication "+"
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } (Gren.Syntax.Expression.Integer 1))
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } (Gren.Syntax.Expression.Integer 2))
                                        )
                                    )
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (Gren.Syntax.Expression.Integer 3))
                                )
                            )
                )
            , Test.test "pipe operation"
                (\() ->
                    "a |> b"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                                (Gren.Syntax.Expression.OperatorApplication "|>"
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } (Gren.Syntax.Expression.FunctionOrValue [] "a"))
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } (Gren.Syntax.Expression.FunctionOrValue [] "b"))
                                )
                            )
                )
            , Test.test "function with higher order"
                (\() ->
                    "chompWhile (\\c -> c == ' ' || c == '\\n' || c == '\\r')"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 54 } }
                                (Gren.Syntax.Expression.Application
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } } (Gren.Syntax.Expression.FunctionOrValue [] "chompWhile")
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 54 } }
                                        (Gren.Syntax.Expression.ParenthesizedExpression
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 53 } }
                                                (Gren.Syntax.Expression.LambdaExpression
                                                    { args = [ Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } } (Gren.Syntax.Pattern.VarPattern "c") ]
                                                    , expression =
                                                        Gren.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 53 } }
                                                            (Gren.Syntax.Expression.OperatorApplication "||"
                                                                (Gren.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 27 } }
                                                                    (Gren.Syntax.Expression.OperatorApplication "=="
                                                                        (Gren.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } } (Gren.Syntax.Expression.FunctionOrValue [] "c"))
                                                                        (Gren.Syntax.Node.Node { start = { row = 1, column = 24 }, end = { row = 1, column = 27 } } (Gren.Syntax.Expression.CharLiteral ' '))
                                                                    )
                                                                )
                                                                (Gren.Syntax.Node.Node { start = { row = 1, column = 31 }, end = { row = 1, column = 53 } }
                                                                    (Gren.Syntax.Expression.OperatorApplication "||"
                                                                        (Gren.Syntax.Node.Node { start = { row = 1, column = 31 }, end = { row = 1, column = 40 } }
                                                                            (Gren.Syntax.Expression.OperatorApplication "=="
                                                                                (Gren.Syntax.Node.Node { start = { row = 1, column = 31 }, end = { row = 1, column = 32 } } (Gren.Syntax.Expression.FunctionOrValue [] "c"))
                                                                                (Gren.Syntax.Node.Node { start = { row = 1, column = 36 }, end = { row = 1, column = 40 } }
                                                                                    (Gren.Syntax.Expression.CharLiteral '\n')
                                                                                )
                                                                            )
                                                                        )
                                                                        (Gren.Syntax.Node.Node { start = { row = 1, column = 44 }, end = { row = 1, column = 53 } }
                                                                            (Gren.Syntax.Expression.OperatorApplication "=="
                                                                                (Gren.Syntax.Node.Node { start = { row = 1, column = 44 }, end = { row = 1, column = 45 } } (Gren.Syntax.Expression.FunctionOrValue [] "c"))
                                                                                (Gren.Syntax.Node.Node { start = { row = 1, column = 49 }, end = { row = 1, column = 53 } }
                                                                                    (Gren.Syntax.Expression.CharLiteral '\u{000D}')
                                                                                )
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                    }
                                                )
                                            )
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "application should be lower-priority than field access"
                (\() ->
                    "foo { d | b = f x y }.b"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 24 } }
                                (Gren.Syntax.Expression.Application
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Gren.Syntax.Expression.FunctionOrValue [] "foo")
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 24 } }
                                        (Gren.Syntax.Expression.RecordAccess
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 22 } }
                                                (Gren.Syntax.Expression.RecordUpdateExpression (Gren.Syntax.Node.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 8 } } "d")
                                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 21 } }
                                                        ( Gren.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 12 } } "b"
                                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 20 } }
                                                            (Gren.Syntax.Expression.Application
                                                                [ Gren.Syntax.Node.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 16 } } (Gren.Syntax.Expression.FunctionOrValue [] "f")
                                                                , Gren.Syntax.Node.Node { start = { row = 1, column = 17 }, end = { row = 1, column = 18 } } (Gren.Syntax.Expression.FunctionOrValue [] "x")
                                                                , Gren.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } } (Gren.Syntax.Expression.FunctionOrValue [] "y")
                                                                ]
                                                            )
                                                        )
                                                    ]
                                                )
                                            )
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 23 }, end = { row = 1, column = 24 } } "b")
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "should not consider a negative number parameter as the start of a new application"
                (\() ->
                    "Random.list -1 generator"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                (Gren.Syntax.Expression.Application
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } (Gren.Syntax.Expression.FunctionOrValue [ "Random" ] "list")
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 15 } }
                                        (Gren.Syntax.Expression.Negation
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } }
                                                (Gren.Syntax.Expression.Integer 1)
                                            )
                                        )
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 25 } } (Gren.Syntax.Expression.FunctionOrValue [] "generator")
                                    ]
                                )
                            )
                )
            , Test.test "negation can be applied on record access"
                (\() ->
                    "1 + -{x = 10}.x"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 16 } }
                                (Gren.Syntax.Expression.OperatorApplication "+"
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } (Gren.Syntax.Expression.Integer 1))
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 16 } }
                                        (Gren.Syntax.Expression.Negation
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 16 } }
                                                (Gren.Syntax.Expression.RecordAccess
                                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 14 } }
                                                        (Gren.Syntax.Expression.RecordExpr
                                                            [ Gren.Syntax.Node.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 13 } }
                                                                ( Gren.Syntax.Node.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 8 } } "x"
                                                                , Gren.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 13 } } (Gren.Syntax.Expression.Integer 10)
                                                                )
                                                            ]
                                                        )
                                                    )
                                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 16 } } "x")
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                )
            , Test.describe "lambda"
                [ Test.test "unit lambda"
                    (\() ->
                        "\\() -> foo"
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                                    (Gren.Syntax.Expression.LambdaExpression
                                        { args = [ Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } } Gren.Syntax.Pattern.UnitPattern ]
                                        , expression = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } (Gren.Syntax.Expression.FunctionOrValue [] "foo")
                                        }
                                    )
                                )
                    )
                , Test.test "record lambda"
                    (\() ->
                        "\\{foo} -> foo"
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 14 } }
                                    (Gren.Syntax.Expression.LambdaExpression
                                        { args = [ Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } (Gren.Syntax.Pattern.RecordPattern [ Gren.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 6 } } "foo" ]) ]
                                        , expression = Gren.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 14 } } (Gren.Syntax.Expression.FunctionOrValue [] "foo")
                                        }
                                    )
                                )
                    )
                , Test.test "empty record lambda"
                    (\() ->
                        "\\{} -> foo"
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                                    (Gren.Syntax.Expression.LambdaExpression
                                        { args = [ Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } } (Gren.Syntax.Pattern.RecordPattern []) ]
                                        , expression = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } (Gren.Syntax.Expression.FunctionOrValue [] "foo")
                                        }
                                    )
                                )
                    )
                , Test.test "args lambda"
                    (\() ->
                        "\\a b -> a + b"
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 14 } }
                                    (Gren.Syntax.Expression.LambdaExpression
                                        { args =
                                            [ Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (Gren.Syntax.Pattern.VarPattern "a")
                                            , Gren.Syntax.Node.Node { start = { row = 1, column = 4 }, end = { row = 1, column = 5 } } (Gren.Syntax.Pattern.VarPattern "b")
                                            ]
                                        , expression =
                                            Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 14 } }
                                                (Gren.Syntax.Expression.OperatorApplication "+"
                                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (Gren.Syntax.Expression.FunctionOrValue [] "a"))
                                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 14 } } (Gren.Syntax.Expression.FunctionOrValue [] "b"))
                                                )
                                        }
                                    )
                                )
                    )
                , Test.test "tuple lambda"
                    (\() ->
                        "\\(a,b) -> a + b"
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 16 } }
                                    (Gren.Syntax.Expression.LambdaExpression
                                        { args =
                                            [ Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } }
                                                (Gren.Syntax.Pattern.TuplePattern
                                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (Gren.Syntax.Pattern.VarPattern "a")
                                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } (Gren.Syntax.Pattern.VarPattern "b")
                                                    ]
                                                )
                                            ]
                                        , expression =
                                            Gren.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 16 } }
                                                (Gren.Syntax.Expression.OperatorApplication "+"
                                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 12 } } (Gren.Syntax.Expression.FunctionOrValue [] "a"))
                                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 16 } } (Gren.Syntax.Expression.FunctionOrValue [] "b"))
                                                )
                                        }
                                    )
                                )
                    )
                , Test.test "lambda with => instead of ->"
                    (\() ->
                        "\\() => foo"
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                                    (Gren.Syntax.Expression.LambdaExpression
                                        { args = [ Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } } Gren.Syntax.Pattern.UnitPattern ]
                                        , expression = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } (Gren.Syntax.Expression.FunctionOrValue [] "foo")
                                        }
                                    )
                                )
                    )
                , Test.test "lambda with . instead of ->"
                    (\() ->
                        "\\().   foo"
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                                    (Gren.Syntax.Expression.LambdaExpression
                                        { args = [ Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } } Gren.Syntax.Pattern.UnitPattern ]
                                        , expression = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } (Gren.Syntax.Expression.FunctionOrValue [] "foo")
                                        }
                                    )
                                )
                    )
                ]
            , Test.describe "let-in"
                [ Test.test "let expression with multiple declarations"
                    (\() ->
                        """let
  foo = bar

  john n = n in 1"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 4, column = 18 } }
                                    (Gren.Syntax.Expression.LetExpression
                                        { declarations =
                                            [ Gren.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 12 } }
                                                (Gren.Syntax.Expression.LetFunction
                                                    { documentation = Nothing
                                                    , signature = Nothing
                                                    , declaration =
                                                        Gren.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 12 } }
                                                            { name = Gren.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 6 } } "foo"
                                                            , arguments = []
                                                            , expression = Gren.Syntax.Node.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 12 } } (Gren.Syntax.Expression.FunctionOrValue [] "bar")
                                                            }
                                                    }
                                                )
                                            , Gren.Syntax.Node.Node { start = { row = 4, column = 3 }, end = { row = 4, column = 13 } }
                                                (Gren.Syntax.Expression.LetFunction
                                                    { documentation = Nothing
                                                    , signature = Nothing
                                                    , declaration =
                                                        Gren.Syntax.Node.Node { start = { row = 4, column = 3 }, end = { row = 4, column = 13 } }
                                                            { name = Gren.Syntax.Node.Node { start = { row = 4, column = 3 }, end = { row = 4, column = 7 } } "john"
                                                            , arguments = [ Gren.Syntax.Node.Node { start = { row = 4, column = 8 }, end = { row = 4, column = 9 } } (Gren.Syntax.Pattern.VarPattern "n") ]
                                                            , expression = Gren.Syntax.Node.Node { start = { row = 4, column = 12 }, end = { row = 4, column = 13 } } (Gren.Syntax.Expression.FunctionOrValue [] "n")
                                                            }
                                                    }
                                                )
                                            ]
                                        , expression = Gren.Syntax.Node.Node { start = { row = 4, column = 17 }, end = { row = 4, column = 18 } } (Gren.Syntax.Expression.Integer 1)
                                        }
                                    )
                                )
                    )
                , Test.test "Let with `in` indented more than the body and let declarations"
                    (\() ->
                        """let
  bar = 1
            in
  bar"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 4, column = 6 } }
                                    (Gren.Syntax.Expression.LetExpression
                                        { declarations =
                                            [ Gren.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } }
                                                (Gren.Syntax.Expression.LetFunction
                                                    { documentation = Nothing
                                                    , signature = Nothing
                                                    , declaration =
                                                        Gren.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } }
                                                            { name = Gren.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 6 } } "bar"
                                                            , arguments = []
                                                            , expression = Gren.Syntax.Node.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } } (Gren.Syntax.Expression.Integer 1)
                                                            }
                                                    }
                                                )
                                            ]
                                        , expression = Gren.Syntax.Node.Node { start = { row = 4, column = 3 }, end = { row = 4, column = 6 } } (Gren.Syntax.Expression.FunctionOrValue [] "bar")
                                        }
                                    )
                                )
                    )
                , Test.test "should fail to parse if declaration is indented as much as `let`"
                    (\() ->
                        """  let
  bar = 1
  in
  bar"""
                            |> expectFailsToParse GrenParserLenient.expression
                    )
                , Test.test "should fail to parse if declarations are not indented the same way"
                    (\() ->
                        """  let
    bar = 1
      foo = 2
  in
  bar"""
                            |> expectFailsToParse GrenParserLenient.expression
                    )
                , Test.test "let with deindented expression in in"
                    (\() ->
                        """let
  bar = 1
 in
   bar"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 4, column = 7 } }
                                    (Gren.Syntax.Expression.LetExpression
                                        { declarations =
                                            [ Gren.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } }
                                                (Gren.Syntax.Expression.LetFunction
                                                    { documentation = Nothing
                                                    , signature = Nothing
                                                    , declaration =
                                                        Gren.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } }
                                                            { name = Gren.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 6 } } "bar"
                                                            , arguments = []
                                                            , expression = Gren.Syntax.Node.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } } (Gren.Syntax.Expression.Integer 1)
                                                            }
                                                    }
                                                )
                                            ]
                                        , expression = Gren.Syntax.Node.Node { start = { row = 4, column = 4 }, end = { row = 4, column = 7 } } (Gren.Syntax.Expression.FunctionOrValue [] "bar")
                                        }
                                    )
                                )
                    )
                , Test.test "Let function with type annotation"
                    (\() ->
                        """let
    bar : Int
    bar = 1
  in
  bar"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 5, column = 6 } }
                                    (Gren.Syntax.Expression.LetExpression
                                        { declarations =
                                            [ Gren.Syntax.Node.Node { start = { row = 2, column = 5 }, end = { row = 3, column = 12 } }
                                                (Gren.Syntax.Expression.LetFunction
                                                    { documentation = Nothing
                                                    , signature =
                                                        Just
                                                            (Gren.Syntax.Node.Node { start = { row = 2, column = 5 }, end = { row = 2, column = 14 } }
                                                                { name = Gren.Syntax.Node.Node { start = { row = 2, column = 5 }, end = { row = 2, column = 8 } } "bar"
                                                                , typeAnnotation =
                                                                    Gren.Syntax.Node.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 14 } }
                                                                        (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 14 } } ( [], "Int" )) [])
                                                                }
                                                            )
                                                    , declaration =
                                                        Gren.Syntax.Node.Node { start = { row = 3, column = 5 }, end = { row = 3, column = 12 } }
                                                            { name = Gren.Syntax.Node.Node { start = { row = 3, column = 5 }, end = { row = 3, column = 8 } } "bar"
                                                            , arguments = []
                                                            , expression = Gren.Syntax.Node.Node { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } } (Gren.Syntax.Expression.Integer 1)
                                                            }
                                                    }
                                                )
                                            ]
                                        , expression = Gren.Syntax.Node.Node { start = { row = 5, column = 3 }, end = { row = 5, column = 6 } } (Gren.Syntax.Expression.FunctionOrValue [] "bar")
                                        }
                                    )
                                )
                    )
                , Test.test "Let function with type annotation (separated by a few lines)"
                    (\() ->
                        """let
    bar : Int


    bar = 1
  in
  bar"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 7, column = 6 } }
                                    (Gren.Syntax.Expression.LetExpression
                                        { declarations =
                                            [ Gren.Syntax.Node.Node { start = { row = 2, column = 5 }, end = { row = 5, column = 12 } }
                                                (Gren.Syntax.Expression.LetFunction
                                                    { documentation = Nothing
                                                    , signature =
                                                        Just
                                                            (Gren.Syntax.Node.Node { start = { row = 2, column = 5 }, end = { row = 2, column = 14 } }
                                                                { name = Gren.Syntax.Node.Node { start = { row = 2, column = 5 }, end = { row = 2, column = 8 } } "bar"
                                                                , typeAnnotation = Gren.Syntax.Node.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 14 } } (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 14 } } ( [], "Int" )) [])
                                                                }
                                                            )
                                                    , declaration =
                                                        Gren.Syntax.Node.Node { start = { row = 5, column = 5 }, end = { row = 5, column = 12 } }
                                                            { name = Gren.Syntax.Node.Node { start = { row = 5, column = 5 }, end = { row = 5, column = 8 } } "bar"
                                                            , arguments = []
                                                            , expression = Gren.Syntax.Node.Node { start = { row = 5, column = 11 }, end = { row = 5, column = 12 } } (Gren.Syntax.Expression.Integer 1)
                                                            }
                                                    }
                                                )
                                            ]
                                        , expression = Gren.Syntax.Node.Node { start = { row = 7, column = 3 }, end = { row = 7, column = 6 } } (Gren.Syntax.Expression.FunctionOrValue [] "bar")
                                        }
                                    )
                                )
                    )
                , Test.test "Let function with type annotation, signature does not repeat the name"
                    (\() ->
                        """let
    : Int
    bar = 1
  in
  bar"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 5, column = 6 } }
                                    (Gren.Syntax.Expression.LetExpression
                                        { declarations =
                                            [ Gren.Syntax.Node.Node { start = { row = 2, column = 5 }, end = { row = 3, column = 12 } }
                                                (Gren.Syntax.Expression.LetFunction
                                                    { documentation = Nothing
                                                    , signature =
                                                        Just
                                                            (Gren.Syntax.Node.Node { start = { row = 2, column = 5 }, end = { row = 2, column = 10 } }
                                                                { name = Gren.Syntax.Node.Node { start = { row = 2, column = 5 }, end = { row = 2, column = 5 } } "bar"
                                                                , typeAnnotation =
                                                                    Gren.Syntax.Node.Node { start = { row = 2, column = 7 }, end = { row = 2, column = 10 } }
                                                                        (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 2, column = 7 }, end = { row = 2, column = 10 } } ( [], "Int" )) [])
                                                                }
                                                            )
                                                    , declaration =
                                                        Gren.Syntax.Node.Node { start = { row = 3, column = 5 }, end = { row = 3, column = 12 } }
                                                            { name = Gren.Syntax.Node.Node { start = { row = 3, column = 5 }, end = { row = 3, column = 8 } } "bar"
                                                            , arguments = []
                                                            , expression = Gren.Syntax.Node.Node { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } } (Gren.Syntax.Expression.Integer 1)
                                                            }
                                                    }
                                                )
                                            ]
                                        , expression = Gren.Syntax.Node.Node { start = { row = 5, column = 3 }, end = { row = 5, column = 6 } } (Gren.Syntax.Expression.FunctionOrValue [] "bar")
                                        }
                                    )
                                )
                    )
                , Test.test "should fail to parse when type annotation and declaration are not aligned (annotation earlier)"
                    (\() ->
                        """let
    bar : Int
      bar = 1
  in
  bar"""
                            |> expectFailsToParse GrenParserLenient.expression
                    )
                , Test.test "should fail to parse when type annotation and declaration are not aligned (annotation later)"
                    (\() ->
                        """let
       bar : Int
    bar = 1
  in
  bar"""
                            |> expectFailsToParse GrenParserLenient.expression
                    )
                , Test.test "should fail to parse `as` pattern not surrounded by parentheses"
                    (\() ->
                        """let
          bar n as m = 1
        in
        bar"""
                            |> expectFailsToParse GrenParserLenient.expression
                    )
                , Test.test "correctly parse variant + args pattern not surrounded by parentheses"
                    (\() ->
                        """let
          bar Bar m = 1
        in
        bar"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 4, column = 12 } }
                                    (Gren.Syntax.Expression.LetExpression
                                        { declarations =
                                            [ Gren.Syntax.Node.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 24 } }
                                                (Gren.Syntax.Expression.LetFunction
                                                    { documentation = Nothing
                                                    , signature = Nothing
                                                    , declaration =
                                                        Gren.Syntax.Node.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 24 } }
                                                            { name = Gren.Syntax.Node.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 14 } } "bar"
                                                            , arguments =
                                                                [ Gren.Syntax.Node.Node { start = { row = 2, column = 15 }, end = { row = 2, column = 18 } } (Gren.Syntax.Pattern.NamedPattern { moduleName = [], name = "Bar" } [])
                                                                , Gren.Syntax.Node.Node { start = { row = 2, column = 19 }, end = { row = 2, column = 20 } } (Gren.Syntax.Pattern.VarPattern "m")
                                                                ]
                                                            , expression = Gren.Syntax.Node.Node { start = { row = 2, column = 23 }, end = { row = 2, column = 24 } } (Gren.Syntax.Expression.Integer 1)
                                                            }
                                                    }
                                                )
                                            ]
                                        , expression = Gren.Syntax.Node.Node { start = { row = 4, column = 9 }, end = { row = 4, column = 12 } } (Gren.Syntax.Expression.FunctionOrValue [] "bar")
                                        }
                                    )
                                )
                    )
                , Test.test "should not parse let destructuring with a type annotation"
                    (\() ->
                        """let
    bar : Int
    (bar) = 1
  in
  bar"""
                            |> expectFailsToParse GrenParserLenient.expression
                    )
                , Test.test "should not parse let destructuring with `as` not surrounded by parentheses"
                    (\() ->
                        """let
    foo as bar = 1
  in
  bar"""
                            |> expectFailsToParse GrenParserLenient.expression
                    )
                , Test.test "should not parse let destructuring with variant + arguments not surrounded by parentheses"
                    (\() ->
                        """let
    Foo bar = 1
  in
  bar"""
                            |> expectFailsToParse GrenParserLenient.expression
                    )
                , Test.test "allow let destructuring = to be top indented"
                    (\() ->
                        """let
    (bar)
    =     1
  in
  bar"""
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\_ -> ())
                            |> Expect.equal (Just ())
                    )
                , Test.test "allow let destructuring with top indented expression"
                    (\() ->
                        """let
    (bar) =
    1
  in
  bar"""
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\_ -> ())
                            |> Expect.equal (Just ())
                    )
                , Test.test "should not parse let type annotation without a declaration"
                    (\() ->
                        """let
    bar : Int
  in
  bar"""
                            |> expectFailsToParse GrenParserLenient.expression
                    )
                , Test.test "Using destructuring"
                    (\() ->
                        """let
    _ = b
    {a} = b
    (c, d) = e
    (Node _ f) = g
 in
    1"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 7, column = 6 } }
                                    (Gren.Syntax.Expression.LetExpression
                                        { declarations =
                                            [ Gren.Syntax.Node.Node { start = { row = 2, column = 5 }, end = { row = 2, column = 10 } }
                                                (Gren.Syntax.Expression.LetDestructuring (Gren.Syntax.Node.Node { start = { row = 2, column = 5 }, end = { row = 2, column = 6 } } Gren.Syntax.Pattern.AllPattern) (Gren.Syntax.Node.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } } (Gren.Syntax.Expression.FunctionOrValue [] "b")))
                                            , Gren.Syntax.Node.Node { start = { row = 3, column = 5 }, end = { row = 3, column = 12 } }
                                                (Gren.Syntax.Expression.LetDestructuring
                                                    (Gren.Syntax.Node.Node { start = { row = 3, column = 5 }, end = { row = 3, column = 8 } }
                                                        (Gren.Syntax.Pattern.RecordPattern [ Gren.Syntax.Node.Node { start = { row = 3, column = 6 }, end = { row = 3, column = 7 } } "a" ])
                                                    )
                                                    (Gren.Syntax.Node.Node { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } } (Gren.Syntax.Expression.FunctionOrValue [] "b"))
                                                )
                                            , Gren.Syntax.Node.Node { start = { row = 4, column = 5 }, end = { row = 4, column = 15 } }
                                                (Gren.Syntax.Expression.LetDestructuring
                                                    (Gren.Syntax.Node.Node { start = { row = 4, column = 5 }, end = { row = 4, column = 11 } }
                                                        (Gren.Syntax.Pattern.TuplePattern
                                                            [ Gren.Syntax.Node.Node { start = { row = 4, column = 6 }, end = { row = 4, column = 7 } } (Gren.Syntax.Pattern.VarPattern "c")
                                                            , Gren.Syntax.Node.Node { start = { row = 4, column = 9 }, end = { row = 4, column = 10 } } (Gren.Syntax.Pattern.VarPattern "d")
                                                            ]
                                                        )
                                                    )
                                                    (Gren.Syntax.Node.Node { start = { row = 4, column = 14 }, end = { row = 4, column = 15 } } (Gren.Syntax.Expression.FunctionOrValue [] "e"))
                                                )
                                            , Gren.Syntax.Node.Node { start = { row = 5, column = 5 }, end = { row = 5, column = 19 } }
                                                (Gren.Syntax.Expression.LetDestructuring
                                                    (Gren.Syntax.Node.Node { start = { row = 5, column = 5 }, end = { row = 5, column = 15 } }
                                                        (Gren.Syntax.Pattern.ParenthesizedPattern
                                                            (Gren.Syntax.Node.Node { start = { row = 5, column = 6 }, end = { row = 5, column = 14 } } (Gren.Syntax.Pattern.NamedPattern { moduleName = [], name = "Node" } [ Gren.Syntax.Node.Node { start = { row = 5, column = 11 }, end = { row = 5, column = 12 } } Gren.Syntax.Pattern.AllPattern, Gren.Syntax.Node.Node { start = { row = 5, column = 13 }, end = { row = 5, column = 14 } } (Gren.Syntax.Pattern.VarPattern "f") ]))
                                                        )
                                                    )
                                                    (Gren.Syntax.Node.Node { start = { row = 5, column = 18 }, end = { row = 5, column = 19 } } (Gren.Syntax.Expression.FunctionOrValue [] "g"))
                                                )
                                            ]
                                        , expression = Gren.Syntax.Node.Node { start = { row = 7, column = 5 }, end = { row = 7, column = 6 } } (Gren.Syntax.Expression.Integer 1)
                                        }
                                    )
                                )
                    )
                , Test.test "On one line"
                    (\() ->
                        "let indent = String.length s in indent"
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 39 } }
                                    (Gren.Syntax.Expression.LetExpression
                                        { declarations =
                                            [ Gren.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 29 } }
                                                (Gren.Syntax.Expression.LetFunction
                                                    { documentation = Nothing
                                                    , signature = Nothing
                                                    , declaration =
                                                        Gren.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 29 } }
                                                            { name = Gren.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 11 } } "indent"
                                                            , arguments = []
                                                            , expression =
                                                                Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 29 } }
                                                                    (Gren.Syntax.Expression.Application
                                                                        [ Gren.Syntax.Node.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 27 } } (Gren.Syntax.Expression.FunctionOrValue [ "String" ] "length")
                                                                        , Gren.Syntax.Node.Node { start = { row = 1, column = 28 }, end = { row = 1, column = 29 } } (Gren.Syntax.Expression.FunctionOrValue [] "s")
                                                                        ]
                                                                    )
                                                            }
                                                    }
                                                )
                                            ]
                                        , expression = Gren.Syntax.Node.Node { start = { row = 1, column = 33 }, end = { row = 1, column = 39 } } (Gren.Syntax.Expression.FunctionOrValue [] "indent")
                                        }
                                    )
                                )
                    )
                , Test.test "let with list after in without space"
                    (\() ->
                        """let
        a = 1
    in[]"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 3, column = 9 } }
                                    (Gren.Syntax.Expression.LetExpression
                                        { declarations =
                                            [ Gren.Syntax.Node.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 14 } }
                                                (Gren.Syntax.Expression.LetFunction
                                                    { documentation = Nothing
                                                    , signature = Nothing
                                                    , declaration =
                                                        Gren.Syntax.Node.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 14 } }
                                                            { name = Gren.Syntax.Node.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } } "a"
                                                            , arguments = []
                                                            , expression = Gren.Syntax.Node.Node { start = { row = 2, column = 13 }, end = { row = 2, column = 14 } } (Gren.Syntax.Expression.Integer 1)
                                                            }
                                                    }
                                                )
                                            ]
                                        , expression = Gren.Syntax.Node.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 9 } } (Gren.Syntax.Expression.ListExpr [])
                                        }
                                    )
                                )
                    )
                , Test.test "let with record after in without space"
                    (\() ->
                        """let
        a = 1
    in{}"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 3, column = 9 } }
                                    (Gren.Syntax.Expression.LetExpression
                                        { declarations =
                                            [ Gren.Syntax.Node.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 14 } }
                                                (Gren.Syntax.Expression.LetFunction
                                                    { documentation = Nothing
                                                    , signature = Nothing
                                                    , declaration =
                                                        Gren.Syntax.Node.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 14 } }
                                                            { name = Gren.Syntax.Node.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } } "a"
                                                            , arguments = []
                                                            , expression = Gren.Syntax.Node.Node { start = { row = 2, column = 13 }, end = { row = 2, column = 14 } } (Gren.Syntax.Expression.Integer 1)
                                                            }
                                                    }
                                                )
                                            ]
                                        , expression = Gren.Syntax.Node.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 9 } } (Gren.Syntax.Expression.RecordExpr [])
                                        }
                                    )
                                )
                    )
                , Test.test "let with lambda after in without space"
                    (\() ->
                        """let
        a = 1
    in\\_ -> 1"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 3, column = 14 } }
                                    (Gren.Syntax.Expression.LetExpression
                                        { declarations =
                                            [ Gren.Syntax.Node.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 14 } }
                                                (Gren.Syntax.Expression.LetFunction
                                                    { documentation = Nothing
                                                    , signature = Nothing
                                                    , declaration =
                                                        Gren.Syntax.Node.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 14 } }
                                                            { name = Gren.Syntax.Node.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } } "a"
                                                            , arguments = []
                                                            , expression = Gren.Syntax.Node.Node { start = { row = 2, column = 13 }, end = { row = 2, column = 14 } } (Gren.Syntax.Expression.Integer 1)
                                                            }
                                                    }
                                                )
                                            ]
                                        , expression =
                                            Gren.Syntax.Node.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 14 } }
                                                (Gren.Syntax.Expression.LambdaExpression { args = [ Gren.Syntax.Node.Node { start = { row = 3, column = 8 }, end = { row = 3, column = 9 } } Gren.Syntax.Pattern.AllPattern ], expression = Gren.Syntax.Node.Node { start = { row = 3, column = 13 }, end = { row = 3, column = 14 } } (Gren.Syntax.Expression.Integer 1) })
                                        }
                                    )
                                )
                    )
                , Test.test "let is not confused by a variable name starting with let"
                    (\() ->
                        "letterbox"
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } } (Gren.Syntax.Expression.FunctionOrValue [] "letterbox"))
                    )
                ]
            , Test.describe "case-of"
                [ Test.test "allow the matched expression to be top indented"
                    (\() ->
                        """case
True
  of
    A -> 1"""
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\_ -> ())
                            |> Expect.equal (Just ())
                    )
                , Test.test "allow the `of` keyword to be top indented"
                    (\() ->
                        """case True
of
               A -> 1"""
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\_ -> ())
                            |> Expect.equal (Just ())
                    )
                , Test.test "allow a case first branch pattern to be top indented"
                    (\() ->
                        """case True of
True -> 1"""
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\_ -> ())
                            |> Expect.equal (Just ())
                    )
                , Test.test "allow case branch result to be top indented"
                    (\() ->
                        """case f of
  True ->
1"""
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\_ -> ())
                            |> Expect.equal (Just ())
                    )
                , Test.test "case expression"
                    (\() ->
                        """case f of
  True -> 1
  False -> 2"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 3, column = 13 } }
                                    (Gren.Syntax.Expression.CaseExpression
                                        { expression =
                                            Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } (Gren.Syntax.Expression.FunctionOrValue [] "f")
                                        , cases =
                                            [ ( Gren.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 7 } } (Gren.Syntax.Pattern.NamedPattern { moduleName = [], name = "True" } [])
                                              , Gren.Syntax.Node.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 12 } } (Gren.Syntax.Expression.Integer 1)
                                              )
                                            , ( Gren.Syntax.Node.Node { start = { row = 3, column = 3 }, end = { row = 3, column = 8 } } (Gren.Syntax.Pattern.NamedPattern { moduleName = [], name = "False" } [])
                                              , Gren.Syntax.Node.Node { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } } (Gren.Syntax.Expression.Integer 2)
                                              )
                                            ]
                                        }
                                    )
                                )
                    )
                , Test.test "allow . between case pattern and result"
                    (\() ->
                        """case [] of
    _ .  1"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (Gren.Syntax.Node.Node { end = { column = 11, row = 2 }, start = { column = 1, row = 1 } }
                                    (Gren.Syntax.Expression.CaseExpression
                                        { cases =
                                            [ ( Gren.Syntax.Node.Node { end = { column = 6, row = 2 }, start = { column = 5, row = 2 } }
                                                    Gren.Syntax.Pattern.AllPattern
                                              , Gren.Syntax.Node.Node { end = { column = 11, row = 2 }, start = { column = 10, row = 2 } }
                                                    (Gren.Syntax.Expression.Integer 1)
                                              )
                                            ]
                                        , expression =
                                            Gren.Syntax.Node.Node { end = { column = 8, row = 1 }, start = { column = 6, row = 1 } }
                                                (Gren.Syntax.Expression.ListExpr [])
                                        }
                                    )
                                )
                    )
                , Test.test "allow no symbol at all between case pattern and result"
                    (\() ->
                        """case [] of
    _    1"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (Gren.Syntax.Node.Node { end = { column = 11, row = 2 }, start = { column = 1, row = 1 } }
                                    (Gren.Syntax.Expression.CaseExpression
                                        { cases =
                                            [ ( Gren.Syntax.Node.Node { end = { column = 6, row = 2 }, start = { column = 5, row = 2 } }
                                                    Gren.Syntax.Pattern.AllPattern
                                              , Gren.Syntax.Node.Node { end = { column = 11, row = 2 }, start = { column = 10, row = 2 } }
                                                    (Gren.Syntax.Expression.Integer 1)
                                              )
                                            ]
                                        , expression =
                                            Gren.Syntax.Node.Node { end = { column = 8, row = 1 }, start = { column = 6, row = 1 } }
                                                (Gren.Syntax.Expression.ListExpr [])
                                        }
                                    )
                                )
                    )
                , Test.test "case expression with `case` after simple cased expression"
                    (\() ->
                        """f case
  True -> 1
  False -> 2"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 3, column = 13 } }
                                    (Gren.Syntax.Expression.CaseExpression
                                        { expression =
                                            Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } }
                                                (Gren.Syntax.Expression.FunctionOrValue [] "f")
                                        , cases =
                                            [ ( Gren.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 7 } }
                                                    (Gren.Syntax.Pattern.NamedPattern { moduleName = [], name = "True" } [])
                                              , Gren.Syntax.Node.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 12 } }
                                                    (Gren.Syntax.Expression.Integer 1)
                                              )
                                            , ( Gren.Syntax.Node.Node { start = { row = 3, column = 3 }, end = { row = 3, column = 8 } }
                                                    (Gren.Syntax.Pattern.NamedPattern { moduleName = [], name = "False" } [])
                                              , Gren.Syntax.Node.Node { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } }
                                                    (Gren.Syntax.Expression.Integer 2)
                                              )
                                            ]
                                        }
                                    )
                                )
                    )
                , Test.test "case expression with `case` after cased expression infix operation"
                    (\() ->
                        """f |> g case
  True -> 1
  False -> 2"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 3, column = 13 } }
                                    (Gren.Syntax.Expression.CaseExpression
                                        { expression =
                                            Gren.Syntax.Node.Node { end = { column = 7, row = 1 }, start = { column = 1, row = 1 } }
                                                (Gren.Syntax.Expression.OperatorApplication "|>"
                                                    (Gren.Syntax.Node.Node { end = { column = 2, row = 1 }, start = { column = 1, row = 1 } }
                                                        (Gren.Syntax.Expression.FunctionOrValue [] "f")
                                                    )
                                                    (Gren.Syntax.Node.Node { end = { column = 7, row = 1 }, start = { column = 6, row = 1 } }
                                                        (Gren.Syntax.Expression.FunctionOrValue [] "g")
                                                    )
                                                )
                                        , cases =
                                            [ ( Gren.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 7 } }
                                                    (Gren.Syntax.Pattern.NamedPattern { moduleName = [], name = "True" } [])
                                              , Gren.Syntax.Node.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 12 } }
                                                    (Gren.Syntax.Expression.Integer 1)
                                              )
                                            , ( Gren.Syntax.Node.Node { start = { row = 3, column = 3 }, end = { row = 3, column = 8 } }
                                                    (Gren.Syntax.Pattern.NamedPattern { moduleName = [], name = "False" } [])
                                              , Gren.Syntax.Node.Node { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } }
                                                    (Gren.Syntax.Expression.Integer 2)
                                              )
                                            ]
                                        }
                                    )
                                )
                    )
                , Test.test "case expression with qualified imports"
                    (\() ->
                        """case f of
  Foo.Bar -> 1"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 15 } }
                                    (Gren.Syntax.Expression.CaseExpression
                                        { expression =
                                            Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } (Gren.Syntax.Expression.FunctionOrValue [] "f")
                                        , cases =
                                            [ ( Gren.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } } (Gren.Syntax.Pattern.NamedPattern { moduleName = [ "Foo" ], name = "Bar" } [])
                                              , Gren.Syntax.Node.Node { start = { row = 2, column = 14 }, end = { row = 2, column = 15 } } (Gren.Syntax.Expression.Integer 1)
                                              )
                                            ]
                                        }
                                    )
                                )
                    )
                , Test.test "case expression with no space between pattern and value"
                    (\() ->
                        """case f of
  x->1"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 7 } }
                                    (Gren.Syntax.Expression.CaseExpression
                                        { expression =
                                            Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } (Gren.Syntax.Expression.FunctionOrValue [] "f")
                                        , cases =
                                            [ ( Gren.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 4 } } (Gren.Syntax.Pattern.VarPattern "x")
                                              , Gren.Syntax.Node.Node { start = { row = 2, column = 6 }, end = { row = 2, column = 7 } } (Gren.Syntax.Expression.Integer 1)
                                              )
                                            ]
                                        }
                                    )
                                )
                    )
                , Test.test "should parse case expression with first branch on the same line as case of"
                    (\() ->
                        """case x of True -> 1
          False -> 2"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 21 } }
                                    (Gren.Syntax.Expression.CaseExpression
                                        { expression = Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } (Gren.Syntax.Expression.FunctionOrValue [] "x")
                                        , cases =
                                            [ ( Gren.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 15 } } (Gren.Syntax.Pattern.NamedPattern { moduleName = [], name = "True" } [])
                                              , Gren.Syntax.Node.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } } (Gren.Syntax.Expression.Integer 1)
                                              )
                                            , ( Gren.Syntax.Node.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 16 } } (Gren.Syntax.Pattern.NamedPattern { moduleName = [], name = "False" } [])
                                              , Gren.Syntax.Node.Node { start = { row = 2, column = 20 }, end = { row = 2, column = 21 } } (Gren.Syntax.Expression.Integer 2)
                                              )
                                            ]
                                        }
                                    )
                                )
                    )
                , Test.test "should parse case expression with a multiline pattern"
                    (\() ->
                        """case x of
        \"\"\"single line triple quote\"\"\" ->
            1
        \"\"\"multi line
            triple quote\"\"\" ->
            2
        _ -> 3"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 7, column = 15 } }
                                    (Gren.Syntax.Expression.CaseExpression
                                        { expression =
                                            Gren.Syntax.Node.Node
                                                { start = { row = 1, column = 6 }
                                                , end = { row = 1, column = 7 }
                                                }
                                                (Gren.Syntax.Expression.FunctionOrValue [] "x")
                                        , cases =
                                            [ ( Gren.Syntax.Node.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 39 } } (Gren.Syntax.Pattern.StringPattern "single line triple quote")
                                              , Gren.Syntax.Node.Node { start = { row = 3, column = 13 }, end = { row = 3, column = 14 } } (Gren.Syntax.Expression.Integer 1)
                                              )
                                            , ( Gren.Syntax.Node.Node { start = { row = 4, column = 9 }, end = { row = 5, column = 28 } } (Gren.Syntax.Pattern.StringPattern "multi line\n            triple quote")
                                              , Gren.Syntax.Node.Node { start = { row = 6, column = 13 }, end = { row = 6, column = 14 } } (Gren.Syntax.Expression.Integer 2)
                                              )
                                            , ( Gren.Syntax.Node.Node { start = { row = 7, column = 9 }, end = { row = 7, column = 10 } } Gren.Syntax.Pattern.AllPattern
                                              , Gren.Syntax.Node.Node { start = { row = 7, column = 14 }, end = { row = 7, column = 15 } } (Gren.Syntax.Expression.Integer 3)
                                              )
                                            ]
                                        }
                                    )
                                )
                    )
                , Test.test "should fail to parse case expression with second branch indented differently than the first line (before)"
                    (\() ->
                        expectFailsToParse GrenParserLenient.expression
                            """case f of
  True -> 1
 False -> 2"""
                    )
                , Test.test "should fail to parse case expression with second branch indented differently than the first line (after)"
                    (\() ->
                        """case f of
  True -> 1
   False -> 2
"""
                            |> expectFailsToParse GrenParserLenient.expression
                    )
                , Test.test "should parse case expression when "
                    (\() ->
                        """case msg of
  Increment ->
    1

  Decrement ->
    2"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 6, column = 6 } }
                                    (Gren.Syntax.Expression.CaseExpression
                                        { expression = Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 9 } } (Gren.Syntax.Expression.FunctionOrValue [] "msg")
                                        , cases =
                                            [ ( Gren.Syntax.Node.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 12 } } (Gren.Syntax.Pattern.NamedPattern { moduleName = [], name = "Increment" } [])
                                              , Gren.Syntax.Node.Node { start = { row = 3, column = 5 }, end = { row = 3, column = 6 } } (Gren.Syntax.Expression.Integer 1)
                                              )
                                            , ( Gren.Syntax.Node.Node { start = { row = 5, column = 3 }, end = { row = 5, column = 12 } } (Gren.Syntax.Pattern.NamedPattern { moduleName = [], name = "Decrement" } [])
                                              , Gren.Syntax.Node.Node { start = { row = 6, column = 5 }, end = { row = 6, column = 6 } } (Gren.Syntax.Expression.Integer 2)
                                              )
                                            ]
                                        }
                                    )
                                )
                    )
                ]
            , Test.test "allow if condition to be top indented"
                (\() ->
                    """a =
    let
        x =
            if
        f y then  1 else 0
    in
    x"""
                        |> GrenParserLenient.run GrenParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "allow `then` indentation to be top indented"
                (\() ->
                    """a =
    let
        x =
            if True
        then  1 else 0
    in
    x"""
                        |> GrenParserLenient.run GrenParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "allow if `then` to not be positively indented if it doesn't overlap with existing indents"
                (\() ->
                    """a =
    let
        x =
            if True
       then  1 else 0
    in
    x"""
                        |> GrenParserLenient.run GrenParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "allow if-true-branch to be top indented"
                (\() ->
                    """a =
    let
        x =
            if True then
        1   else 0
    in
    x"""
                        |> GrenParserLenient.run GrenParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "allow if `else` to be top indented"
                (\() ->
                    """a =
    let
        x =
            if True then 1
        else 0
    in
    x"""
                        |> GrenParserLenient.run GrenParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "allow if `else` to not positively indented if it doesn't overlap with existing indents"
                (\() ->
                    """a =
    let
        x =
            if True then 1
       else 0
    in
    x"""
                        |> GrenParserLenient.run GrenParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "allow if if-false-branch to be top indented"
                (\() ->
                    """a =
    let
        x =
            if True then 1 else
        0
    in
    x"""
                        |> GrenParserLenient.run GrenParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "allow record closing curly to be top indented"
                (\() ->
                    """a =
    let
        x =
            { a = 0, b = 1
        }
    in
    x"""
                        |> GrenParserLenient.run GrenParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "allow record field value to be top indented"
                (\() ->
                    """a =
    let
        x =
            { a = 0, b =
        1 }
    in
    x"""
                        |> GrenParserLenient.run GrenParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "allow record field name to be top indented"
                (\() ->
                    """a =
    let
        x =
            { a = 0,
        b       = 1 }
    in
    x"""
                        |> GrenParserLenient.run GrenParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "allow record field `=` to be top indented"
                (\() ->
                    """a =
    let
        x =
            { a = 0, b
        =         1 }
    in
    x"""
                        |> GrenParserLenient.run GrenParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "allow tuple closing parens to be top indented"
                (\() ->
                    """a =
    let
        x =
            ( 0, 1
        )
    in
    x"""
                        |> GrenParserLenient.run GrenParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "allow first tuple part to be top indented"
                (\() ->
                    """a =
    let
        x =
            (
        0   , 1
            )
    in
    x"""
                        |> GrenParserLenient.run GrenParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "allow second tuple part to be top indented"
                (\() ->
                    """a =
    let
        x =
            ( 0,
        1   )
    in
    x"""
                        |> GrenParserLenient.run GrenParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "allow infix operator to be top indented"
                (\() ->
                    """a =
    let
        x =
            0
        + 1
    in
    x"""
                        |> GrenParserLenient.run GrenParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "fail if function call argument is top indented"
                (\() ->
                    """a =
    let
        x =
            f 0
        1
    in
    x"""
                        |> expectFailsToParse GrenParserLenient.declaration
                )
            , Test.test "allow lambda result to be top indented"
                (\() ->
                    """a =
    let
        x =
            \\y ->
        y
    in
    x"""
                        |> GrenParserLenient.run GrenParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "allow case branch result call argument to be top indented"
                (\() ->
                    """foo = 
    case Nothing of
        Nothing -> a <|
        \\_ -> ()"""
                        |> GrenParserLenient.run GrenParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "allow if case branch result call argument to not positively indented if it doesn't overlap with existing indents"
                (\() ->
                    """foo = 
    case Nothing of
        Nothing -> a
  b"""
                        |> GrenParserLenient.run GrenParserLenient.declaration
                        |> Maybe.map (\_ -> ())
                        |> Expect.equal (Just ())
                )
            , Test.test "glsl block"
                (\() ->
                    "[glsl| precision mediump float; |]"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 37 } }
                                (Gren.Syntax.Expression.GLSLExpression " precision mediump float; ")
                            )
                )
            ]
        , Test.describe "pattern"
            [ Test.test "Unit"
                (\() ->
                    "()"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } Gren.Syntax.Pattern.UnitPattern)
                )
            , Test.test "Unit with inner layout"
                (\() ->
                    """(
   -- comment
   )"""
                        |> expectSyntaxWithComments GrenParserLenient.pattern
                            { syntax = Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 3, column = 5 } } Gren.Syntax.Pattern.UnitPattern
                            , comments = [ Gren.Syntax.Node.Node { start = { row = 2, column = 4 }, end = { row = 2, column = 14 } } "-- comment" ]
                            }
                )
            , Test.test "String"
                (\() ->
                    "\"Foo\""
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } (Gren.Syntax.Pattern.StringPattern "Foo"))
                )
            , Test.test "Char"
                (\() ->
                    "'f'"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Gren.Syntax.Pattern.CharPattern 'f'))
                )
            , Test.test "Wildcard"
                (\() ->
                    "_"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } Gren.Syntax.Pattern.AllPattern)
                )
            , Test.test "Parenthesized"
                (\() ->
                    "(x)"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                (Gren.Syntax.Pattern.ParenthesizedPattern
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (Gren.Syntax.Pattern.VarPattern "x"))
                                )
                            )
                )
            , Test.test "Int"
                (\() ->
                    "1"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } (Gren.Syntax.Pattern.IntPattern 1))
                )
            , Test.test "Hex int"
                (\() ->
                    "0x1"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Gren.Syntax.Pattern.HexPattern 1))
                )
            , Test.test "Float should not be valid" (\() -> expectFailsToParse GrenParserLenient.pattern "1.0")
            , Test.test "Uncons"
                (\() ->
                    "n :: tail"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                                (Gren.Syntax.Pattern.UnConsPattern (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } (Gren.Syntax.Pattern.VarPattern "n"))
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 10 } } (Gren.Syntax.Pattern.VarPattern "tail"))
                                )
                            )
                )
            , Test.test "Uncons multiple"
                (\() ->
                    "a :: b :: cUp"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 14 } }
                                (Gren.Syntax.Pattern.UnConsPattern
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } (Gren.Syntax.Pattern.VarPattern "a"))
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 14 } }
                                        (Gren.Syntax.Pattern.UnConsPattern (Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } (Gren.Syntax.Pattern.VarPattern "b"))
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 14 } } (Gren.Syntax.Pattern.VarPattern "cUp"))
                                        )
                                    )
                                )
                            )
                )
            , Test.test "Uncons with parens"
                (\() ->
                    "(X x) :: xs"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                (Gren.Syntax.Pattern.UnConsPattern
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                                        (Gren.Syntax.Pattern.ParenthesizedPattern
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 5 } }
                                                (Gren.Syntax.Pattern.NamedPattern { moduleName = [], name = "X" }
                                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 4 }, end = { row = 1, column = 5 } } (Gren.Syntax.Pattern.VarPattern "x") ]
                                                )
                                            )
                                        )
                                    )
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 12 } } (Gren.Syntax.Pattern.VarPattern "xs"))
                                )
                            )
                )
            , Test.test "Empty list"
                (\() ->
                    "[]"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } (Gren.Syntax.Pattern.ListPattern []))
                )
            , Test.test "Empty list pattern with whitespace"
                (\() ->
                    "[ ]"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Gren.Syntax.Pattern.ListPattern []))
                )
            , Test.test "Single element list"
                (\() ->
                    "[1]"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Gren.Syntax.Pattern.ListPattern [ Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (Gren.Syntax.Pattern.IntPattern 1) ]))
                )
            , Test.test "Single element list with trailing whitespace"
                (\() ->
                    "[1 ]"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } (Gren.Syntax.Pattern.ListPattern [ Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (Gren.Syntax.Pattern.IntPattern 1) ]))
                )
            , Test.test "Single element list with leading whitespace"
                (\() ->
                    "[ 1]"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } (Gren.Syntax.Pattern.ListPattern [ Gren.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (Gren.Syntax.Pattern.IntPattern 1) ]))
                )
            , Test.test "list with prefix extra comma"
                (\() ->
                    "[,1]"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } (Gren.Syntax.Pattern.ListPattern [ Gren.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (Gren.Syntax.Pattern.IntPattern 1) ]))
                )
            , Test.test "list with extra comma between elements"
                (\() ->
                    "[1,,2]"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                                (Gren.Syntax.Pattern.ListPattern
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } }
                                        (Gren.Syntax.Pattern.IntPattern 1)
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } }
                                        (Gren.Syntax.Pattern.IntPattern 2)
                                    ]
                                )
                            )
                )
            , Test.test "Empty record"
                (\() ->
                    "{}"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } (Gren.Syntax.Pattern.RecordPattern []))
                )
            , Test.test "Empty record with whitespace"
                (\() ->
                    "{ }"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Gren.Syntax.Pattern.RecordPattern []))
                )
            , Test.test "Record"
                (\() ->
                    "{a,b}"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                                (Gren.Syntax.Pattern.RecordPattern
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } "a"
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 4 }, end = { row = 1, column = 5 } } "b"
                                    ]
                                )
                            )
                )
            , Test.test "Record pattern with whitespace"
                (\() ->
                    "{a , b}"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                                (Gren.Syntax.Pattern.RecordPattern
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } "a"
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } "b"
                                    ]
                                )
                            )
                )
            , Test.test "Record pattern with extra comma between fields"
                (\() ->
                    "{a,, b}"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                                (Gren.Syntax.Pattern.RecordPattern
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } "a"
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } "b"
                                    ]
                                )
                            )
                )
            , Test.test "Record pattern with prefixed comma"
                (\() ->
                    "{ , a , b }"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                (Gren.Syntax.Pattern.RecordPattern
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } "a"
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } "b"
                                    ]
                                )
                            )
                )
            , Test.test "Record pattern with field name colliding with keyword"
                (\() ->
                    "{ ,as , b }"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                (Gren.Syntax.Pattern.RecordPattern
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 4 }, end = { row = 1, column = 6 } } "as_"
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } "b"
                                    ]
                                )
                            )
                )
            , Test.test "Record pattern with trailing whitespace"
                (\() ->
                    "{a }"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }
                                (Gren.Syntax.Pattern.RecordPattern
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } "a" ]
                                )
                            )
                )
            , Test.test "Record pattern with leading whitespace"
                (\() ->
                    "{ a}"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }
                                (Gren.Syntax.Pattern.RecordPattern
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } "a" ]
                                )
                            )
                )
            , Test.test "Named"
                (\() ->
                    "True"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }
                                (Gren.Syntax.Pattern.NamedPattern { moduleName = [], name = "True" } [])
                            )
                )
            , Test.test "Named pattern without and with spacing should parse to the same"
                (\() ->
                    "Bar "
                        |> GrenParserLenient.run GrenParserLenient.pattern
                        |> Expect.equal
                            ("Bar"
                                |> GrenParserLenient.run GrenParserLenient.pattern
                            )
                )
            , Test.test "Qualified named"
                (\() ->
                    "Basics.True"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                (Gren.Syntax.Pattern.NamedPattern { moduleName = [ "Basics" ], name = "True" } [])
                            )
                )
            , Test.test "Named pattern with data"
                (\() ->
                    "Set x"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                                (Gren.Syntax.Pattern.NamedPattern
                                    { moduleName = [], name = "Set" }
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } (Gren.Syntax.Pattern.VarPattern "x") ]
                                )
                            )
                )
            , Test.test "Qualified named pattern with data"
                (\() ->
                    "Set.Set x"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                                (Gren.Syntax.Pattern.NamedPattern
                                    { moduleName = [ "Set" ], name = "Set" }
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (Gren.Syntax.Pattern.VarPattern "x") ]
                                )
                            )
                )
            , Test.test "Tuple"
                (\() ->
                    "(model, cmd)"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                                (Gren.Syntax.Pattern.TuplePattern
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } (Gren.Syntax.Pattern.VarPattern "model")
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } (Gren.Syntax.Pattern.VarPattern "cmd")
                                    ]
                                )
                            )
                )
            , Test.test "4-tuple pattern is invalid"
                (\() ->
                    "(1,2,3,4)"
                        |> expectFailsToParse GrenParserLenient.pattern
                )
            , Test.test "Nested tuple"
                (\() ->
                    "(a,{b,c},())"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                                (Gren.Syntax.Pattern.TuplePattern
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (Gren.Syntax.Pattern.VarPattern "a")
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 4 }, end = { row = 1, column = 9 } } (Gren.Syntax.Pattern.RecordPattern [ Gren.Syntax.Node.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } "b", Gren.Syntax.Node.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 8 } } "c" ])
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 12 } } Gren.Syntax.Pattern.UnitPattern
                                    ]
                                )
                            )
                )
            , Test.test "As pattern"
                (\() ->
                    "x as y"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                                (Gren.Syntax.Pattern.AsPattern
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } (Gren.Syntax.Pattern.VarPattern "x"))
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } "y")
                                )
                            )
                )
            , Test.test "should fail to parse when right side is not a direct variable name"
                (\() ->
                    "x as (y)"
                        |> expectFailsToParse GrenParserLenient.pattern
                )
            , Test.test "should fail to parse consecutive as"
                (\() ->
                    "x as y as z"
                        |> expectFailsToParse GrenParserLenient.pattern
                )
            , Test.test "should fail to parse :: after as"
                (\() ->
                    "x as y :: z"
                        |> expectFailsToParse GrenParserLenient.pattern
                )
            , Test.test "should fail to parse :: after as even when :: was already used before"
                (\() ->
                    "w :: x as y :: z"
                        |> expectFailsToParse GrenParserLenient.pattern
                )
            , Test.test "should fail to parse when right side is an invalid variable name"
                (\() ->
                    "x as _y"
                        |> expectFailsToParse GrenParserLenient.pattern
                )
            , Test.test "should fail to parse when right side is not a variable name"
                (\() ->
                    "x as 1"
                        |> expectFailsToParse GrenParserLenient.pattern
                )
            , Test.test "Record as"
                (\() ->
                    "{model,context} as appState"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 28 } }
                                (Gren.Syntax.Pattern.AsPattern
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 16 } }
                                        (Gren.Syntax.Pattern.RecordPattern
                                            [ Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } "model"
                                            , Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 15 } } "context"
                                            ]
                                        )
                                    )
                                    (Gren.Syntax.Node.Node { start = { row = 1, column = 20 }, end = { row = 1, column = 28 } } "appState")
                                )
                            )
                )
            , Test.test "Complex"
                (\() ->
                    "(Index irec as index, docVector)"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 33 } }
                                (Gren.Syntax.Pattern.TuplePattern
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 21 } }
                                        (Gren.Syntax.Pattern.AsPattern
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 12 } }
                                                (Gren.Syntax.Pattern.NamedPattern { moduleName = [], name = "Index" }
                                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 12 } } (Gren.Syntax.Pattern.VarPattern "irec") ]
                                                )
                                            )
                                            (Gren.Syntax.Node.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 21 } } "index")
                                        )
                                    , Gren.Syntax.Node.Node { start = { row = 1, column = 23 }, end = { row = 1, column = 32 } } (Gren.Syntax.Pattern.VarPattern "docVector")
                                    ]
                                )
                            )
                )
            , Test.test "Complex pattern 2"
                (\() ->
                    "RBNode_gren_builtin col (RBNode_gren_builtin Red  (RBNode_gren_builtin Red xv))"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (Gren.Syntax.Node.Node { end = { column = 80, row = 1 }, start = { column = 1, row = 1 } }
                                (Gren.Syntax.Pattern.NamedPattern { moduleName = [], name = "RBNode_gren_builtin" }
                                    [ Gren.Syntax.Node.Node { end = { column = 24, row = 1 }, start = { column = 21, row = 1 } }
                                        (Gren.Syntax.Pattern.VarPattern "col")
                                    , Gren.Syntax.Node.Node { end = { column = 80, row = 1 }, start = { column = 25, row = 1 } }
                                        (Gren.Syntax.Pattern.ParenthesizedPattern
                                            (Gren.Syntax.Node.Node { end = { column = 79, row = 1 }, start = { column = 26, row = 1 } }
                                                (Gren.Syntax.Pattern.NamedPattern { moduleName = [], name = "RBNode_gren_builtin" }
                                                    [ Gren.Syntax.Node.Node { end = { column = 49, row = 1 }, start = { column = 46, row = 1 } }
                                                        (Gren.Syntax.Pattern.NamedPattern { moduleName = [], name = "Red" } [])
                                                    , Gren.Syntax.Node.Node { end = { column = 79, row = 1 }, start = { column = 51, row = 1 } }
                                                        (Gren.Syntax.Pattern.ParenthesizedPattern
                                                            (Gren.Syntax.Node.Node { end = { column = 78, row = 1 }, start = { column = 52, row = 1 } }
                                                                (Gren.Syntax.Pattern.NamedPattern { moduleName = [], name = "RBNode_gren_builtin" }
                                                                    [ Gren.Syntax.Node.Node { end = { column = 75, row = 1 }, start = { column = 72, row = 1 } }
                                                                        (Gren.Syntax.Pattern.NamedPattern { moduleName = [], name = "Red" } [])
                                                                    , Gren.Syntax.Node.Node { end = { column = 78, row = 1 }, start = { column = 76, row = 1 } }
                                                                        (Gren.Syntax.Pattern.VarPattern "xv")
                                                                    ]
                                                                )
                                                            )
                                                        )
                                                    ]
                                                )
                                            )
                                        )
                                    ]
                                )
                            )
                )
            ]
        , Test.describe "misc comments and operators"
            [ Test.test "function with documentation comment, without signature"
                (\() ->
                    """
module Bar exposing (..)

import String

{-| The docs
-}
bar = 1
"""
                        |> String.trim
                        |> GrenParserLenient.run GrenParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                                            , exposingList =
                                                Gren.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } (Gren.Syntax.Exposing.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                                            }
                                        )
                                , imports =
                                    [ Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 14 } }
                                        { moduleName = Gren.Syntax.Node.Node { start = { row = 3, column = 8 }, end = { row = 3, column = 14 } } [ "String" ]
                                        , moduleAlias = Nothing
                                        , exposingList = Nothing
                                        }
                                    ]
                                , declarations =
                                    [ Gren.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 7, column = 8 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Just (Gren.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 6, column = 3 } } "{-| The docs\n-}")
                                            , signature = Nothing
                                            , declaration =
                                                Gren.Syntax.Node.Node { start = { row = 7, column = 1 }, end = { row = 7, column = 8 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 7, column = 1 }, end = { row = 7, column = 4 } } "bar"
                                                    , arguments = []
                                                    , expression = Gren.Syntax.Node.Node { start = { row = 7, column = 7 }, end = { row = 7, column = 8 } } (Gren.Syntax.Expression.Integer 1)
                                                    }
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "function with documentation and signature"
                (\() ->
                    """
module Bar exposing (..)

import String

{-| The docs
-}
bar : Int
bar = 1
"""
                        |> String.trim
                        |> GrenParserLenient.run GrenParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                                            , exposingList = Gren.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } (Gren.Syntax.Exposing.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                                            }
                                        )
                                , imports =
                                    [ Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 14 } }
                                        { moduleName = Gren.Syntax.Node.Node { start = { row = 3, column = 8 }, end = { row = 3, column = 14 } } [ "String" ]
                                        , moduleAlias = Nothing
                                        , exposingList = Nothing
                                        }
                                    ]
                                , declarations =
                                    [ Gren.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 8, column = 8 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation =
                                                Just (Gren.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 6, column = 3 } } "{-| The docs\n-}")
                                            , signature =
                                                Just
                                                    (Gren.Syntax.Node.Node { start = { row = 7, column = 1 }, end = { row = 7, column = 10 } }
                                                        { name = Gren.Syntax.Node.Node { start = { row = 7, column = 1 }, end = { row = 7, column = 4 } } "bar"
                                                        , typeAnnotation =
                                                            Gren.Syntax.Node.Node { start = { row = 7, column = 7 }, end = { row = 7, column = 10 } } (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 7, column = 7 }, end = { row = 7, column = 10 } } ( [], "Int" )) [])
                                                        }
                                                    )
                                            , declaration =
                                                Gren.Syntax.Node.Node { start = { row = 8, column = 1 }, end = { row = 8, column = 8 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 8, column = 1 }, end = { row = 8, column = 4 } } "bar"
                                                    , arguments = []
                                                    , expression = Gren.Syntax.Node.Node { start = { row = 8, column = 7 }, end = { row = 8, column = 8 } } (Gren.Syntax.Expression.Integer 1)
                                                    }
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "function with single line comment before"
                (\() ->
                    """
module Bar exposing (..)

--The Doc
bar = 1
"""
                        |> String.trim
                        |> GrenParserLenient.run GrenParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                                            , exposingList = Gren.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } (Gren.Syntax.Exposing.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ Gren.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 8 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Gren.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 8 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 4 } } "bar"
                                                    , arguments = []
                                                    , expression = Gren.Syntax.Node.Node { start = { row = 4, column = 7 }, end = { row = 4, column = 8 } } (Gren.Syntax.Expression.Integer 1)
                                                    }
                                            }
                                        )
                                    ]
                                , comments = [ Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 10 } } "--The Doc" ]
                                }
                            )
                )
            , Test.test "file with multiple comments"
                (\() ->
                    """
-- comment 1
module Bar exposing (..)

-- comment 2
bar = {- comment 3 -} 1 -- comment 4
 -- comment 5
"""
                        |> String.trim
                        |> GrenParserLenient.run GrenParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Gren.Syntax.Node.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 25 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = Gren.Syntax.Node.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } [ "Bar" ]
                                            , exposingList = Gren.Syntax.Node.Node { start = { row = 2, column = 12 }, end = { row = 2, column = 25 } } (Gren.Syntax.Exposing.All { start = { row = 2, column = 22 }, end = { row = 2, column = 24 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ Gren.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 24 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Gren.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 24 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 4 } } "bar"
                                                    , arguments = []
                                                    , expression = Gren.Syntax.Node.Node { start = { row = 5, column = 23 }, end = { row = 5, column = 24 } } (Gren.Syntax.Expression.Integer 1)
                                                    }
                                            }
                                        )
                                    ]
                                , comments =
                                    [ Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } } "-- comment 1"
                                    , Gren.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 13 } } "-- comment 2"
                                    , Gren.Syntax.Node.Node { start = { row = 5, column = 7 }, end = { row = 5, column = 22 } } "{- comment 3 -}"
                                    , Gren.Syntax.Node.Node { start = { row = 5, column = 25 }, end = { row = 5, column = 37 } } "-- comment 4"
                                    , Gren.Syntax.Node.Node { start = { row = 6, column = 2 }, end = { row = 6, column = 14 } } "-- comment 5"
                                    ]
                                }
                            )
                )
            , Test.test "function with multi-line comment before"
                (\() ->
                    """
module Bar exposing (..)

{- The Doc -}
bar = 1
"""
                        |> String.trim
                        |> GrenParserLenient.run GrenParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                                            , exposingList =
                                                Gren.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } (Gren.Syntax.Exposing.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ Gren.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 8 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Gren.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 8 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 4 } } "bar"
                                                    , arguments = []
                                                    , expression = Gren.Syntax.Node.Node { start = { row = 4, column = 7 }, end = { row = 4, column = 8 } } (Gren.Syntax.Expression.Integer 1)
                                                    }
                                            }
                                        )
                                    ]
                                , comments = [ Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 14 } } "{- The Doc -}" ]
                                }
                            )
                )
            , Test.test "type alias with documentation"
                (\() ->
                    """
module Bar exposing (..)

import String

{-| The Doc -}
type alias Foo
   = { name : String }
"""
                        |> String.trim
                        |> GrenParserLenient.run GrenParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                                            , exposingList = Gren.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } (Gren.Syntax.Exposing.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                                            }
                                        )
                                , imports =
                                    [ Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 14 } }
                                        { moduleName = Gren.Syntax.Node.Node { start = { row = 3, column = 8 }, end = { row = 3, column = 14 } } [ "String" ]
                                        , moduleAlias = Nothing
                                        , exposingList = Nothing
                                        }
                                    ]
                                , declarations =
                                    [ Gren.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 7, column = 23 } }
                                        (GrenSyntax.AliasDeclaration
                                            { documentation =
                                                Just (Gren.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 15 } } "{-| The Doc -}")
                                            , name = Gren.Syntax.Node.Node { start = { row = 6, column = 12 }, end = { row = 6, column = 15 } } "Foo"
                                            , generics = []
                                            , typeAnnotation =
                                                Gren.Syntax.Node.Node { start = { row = 7, column = 6 }, end = { row = 7, column = 23 } }
                                                    (Gren.Syntax.TypeAnnotation.Record
                                                        [ Gren.Syntax.Node.Node { start = { row = 7, column = 8 }, end = { row = 7, column = 21 } }
                                                            ( Gren.Syntax.Node.Node { start = { row = 7, column = 8 }, end = { row = 7, column = 12 } } "name"
                                                            , Gren.Syntax.Node.Node { start = { row = 7, column = 15 }, end = { row = 7, column = 21 } } (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 7, column = 15 }, end = { row = 7, column = 21 } } ( [], "String" )) [])
                                                            )
                                                        ]
                                                    )
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "choice type with documentation comment"
                (\() ->
                    """
module Bar exposing (..)

import String

{-| The Doc -}
type Foo
   = Red
   | Blue
"""
                        |> String.trim
                        |> GrenParserLenient.run GrenParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition = Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } } (GrenSyntax.NormalModule { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ], exposingList = Gren.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } (Gren.Syntax.Exposing.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }) })
                                , imports =
                                    [ Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 14 } }
                                        { moduleName = Gren.Syntax.Node.Node { start = { row = 3, column = 8 }, end = { row = 3, column = 14 } } [ "String" ]
                                        , moduleAlias = Nothing
                                        , exposingList = Nothing
                                        }
                                    ]
                                , declarations =
                                    [ Gren.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 8, column = 10 } }
                                        (GrenSyntax.CustomTypeDeclaration
                                            { documentation =
                                                Just
                                                    (Gren.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 15 } }
                                                        "{-| The Doc -}"
                                                    )
                                            , name = Gren.Syntax.Node.Node { start = { row = 6, column = 6 }, end = { row = 6, column = 9 } } "Foo"
                                            , generics = []
                                            , constructors =
                                                [ Gren.Syntax.Node.Node { start = { row = 7, column = 6 }, end = { row = 7, column = 9 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 7, column = 6 }, end = { row = 7, column = 9 } } "Red", arguments = [] }
                                                , Gren.Syntax.Node.Node { start = { row = 8, column = 6 }, end = { row = 8, column = 10 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 8, column = 6 }, end = { row = 8, column = 10 } } "Blue", arguments = [] }
                                                ]
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "max call stack size failure"
                (\() ->
                    """module Simplify.AstHelpers exposing (log)


log : Int -> Int
log a =
    Debug.log "ok" a
"""
                        |> String.trim
                        |> GrenParserLenient.run GrenParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 42 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 27 } } [ "Simplify", "AstHelpers" ]
                                            , exposingList =
                                                Gren.Syntax.Node.Node { start = { row = 1, column = 28 }, end = { row = 1, column = 42 } }
                                                    (Gren.Syntax.Exposing.Explicit
                                                        [ Gren.Syntax.Node.Node { start = { row = 1, column = 38 }, end = { row = 1, column = 41 } } (Gren.Syntax.Exposing.FunctionExpose "log")
                                                        ]
                                                    )
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ Gren.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 6, column = 21 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Just (Gren.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 17 } } { name = Gren.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 4 } } "log", typeAnnotation = Gren.Syntax.Node.Node { start = { row = 4, column = 7 }, end = { row = 4, column = 17 } } (Gren.Syntax.TypeAnnotation.FunctionTypeAnnotation (Gren.Syntax.Node.Node { start = { row = 4, column = 7 }, end = { row = 4, column = 10 } } (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 4, column = 7 }, end = { row = 4, column = 10 } } ( [], "Int" )) [])) (Gren.Syntax.Node.Node { start = { row = 4, column = 14 }, end = { row = 4, column = 17 } } (Gren.Syntax.TypeAnnotation.Typed (Gren.Syntax.Node.Node { start = { row = 4, column = 14 }, end = { row = 4, column = 17 } } ( [], "Int" )) []))) })
                                            , declaration =
                                                Gren.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 6, column = 21 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 4 } } "log"
                                                    , arguments = [ Gren.Syntax.Node.Node { start = { row = 5, column = 5 }, end = { row = 5, column = 6 } } (Gren.Syntax.Pattern.VarPattern "a") ]
                                                    , expression =
                                                        Gren.Syntax.Node.Node { start = { row = 6, column = 5 }, end = { row = 6, column = 21 } }
                                                            (Gren.Syntax.Expression.Application
                                                                [ Gren.Syntax.Node.Node { start = { row = 6, column = 5 }, end = { row = 6, column = 14 } } (Gren.Syntax.Expression.FunctionOrValue [ "Debug" ] "log")
                                                                , Gren.Syntax.Node.Node { start = { row = 6, column = 15 }, end = { row = 6, column = 19 } } (Gren.Syntax.Expression.Literal "ok")
                                                                , Gren.Syntax.Node.Node { start = { row = 6, column = 20 }, end = { row = 6, column = 21 } } (Gren.Syntax.Expression.FunctionOrValue [] "a")
                                                                ]
                                                            )
                                                    }
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "parenthesized infix operations"
                (\() ->
                    """
module Bar exposing (..)

bar = (x + 1) * (2 * y)
"""
                        |> String.trim
                        |> GrenParserLenient.run GrenParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                                            , exposingList = Gren.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } (Gren.Syntax.Exposing.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 24 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 24 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 4 } } "bar"
                                                    , arguments = []
                                                    , expression =
                                                        Gren.Syntax.Node.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 24 } }
                                                            (Gren.Syntax.Expression.OperatorApplication "*"
                                                                (Gren.Syntax.Node.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 14 } }
                                                                    (Gren.Syntax.Expression.ParenthesizedExpression
                                                                        (Gren.Syntax.Node.Node { start = { row = 3, column = 8 }, end = { row = 3, column = 13 } }
                                                                            (Gren.Syntax.Expression.OperatorApplication "+"
                                                                                (Gren.Syntax.Node.Node { start = { row = 3, column = 8 }, end = { row = 3, column = 9 } } (Gren.Syntax.Expression.FunctionOrValue [] "x"))
                                                                                (Gren.Syntax.Node.Node { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } } (Gren.Syntax.Expression.Integer 1))
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                                (Gren.Syntax.Node.Node { start = { row = 3, column = 17 }, end = { row = 3, column = 24 } }
                                                                    (Gren.Syntax.Expression.ParenthesizedExpression
                                                                        (Gren.Syntax.Node.Node { start = { row = 3, column = 18 }, end = { row = 3, column = 23 } }
                                                                            (Gren.Syntax.Expression.OperatorApplication "*"
                                                                                (Gren.Syntax.Node.Node { start = { row = 3, column = 18 }, end = { row = 3, column = 19 } } (Gren.Syntax.Expression.Integer 2))
                                                                                (Gren.Syntax.Node.Node { start = { row = 3, column = 22 }, end = { row = 3, column = 23 } } (Gren.Syntax.Expression.FunctionOrValue [] "y"))
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                    }
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "infix operators consecutive with different associativity loose then tight"
                (\() ->
                    """
module Bar exposing (..)

bar = x + 1 * 2
"""
                        |> String.trim
                        |> GrenParserLenient.run GrenParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                                            , exposingList = Gren.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } (Gren.Syntax.Exposing.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 16 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 16 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 4 } } "bar"
                                                    , arguments = []
                                                    , expression =
                                                        Gren.Syntax.Node.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 16 } }
                                                            (Gren.Syntax.Expression.OperatorApplication "+"
                                                                (Gren.Syntax.Node.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 8 } } (Gren.Syntax.Expression.FunctionOrValue [] "x"))
                                                                (Gren.Syntax.Node.Node { start = { row = 3, column = 11 }, end = { row = 3, column = 16 } }
                                                                    (Gren.Syntax.Expression.OperatorApplication "*"
                                                                        (Gren.Syntax.Node.Node { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } } (Gren.Syntax.Expression.Integer 1))
                                                                        (Gren.Syntax.Node.Node { start = { row = 3, column = 15 }, end = { row = 3, column = 16 } } (Gren.Syntax.Expression.Integer 2))
                                                                    )
                                                                )
                                                            )
                                                    }
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "infix operators consecutive with different associativity tight then loose"
                (\() ->
                    """
module Bar exposing (..)

bar = x * 1 + 2
"""
                        |> String.trim
                        |> GrenParserLenient.run GrenParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Gren.Syntax.Node.Node
                                        { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                                            , exposingList = Gren.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } (Gren.Syntax.Exposing.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 16 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 16 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 4 } } "bar"
                                                    , arguments = []
                                                    , expression =
                                                        Gren.Syntax.Node.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 16 } }
                                                            (Gren.Syntax.Expression.OperatorApplication "+"
                                                                (Gren.Syntax.Node.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 12 } }
                                                                    (Gren.Syntax.Expression.OperatorApplication "*"
                                                                        (Gren.Syntax.Node.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 8 } } (Gren.Syntax.Expression.FunctionOrValue [] "x"))
                                                                        (Gren.Syntax.Node.Node { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } } (Gren.Syntax.Expression.Integer 1))
                                                                    )
                                                                )
                                                                (Gren.Syntax.Node.Node { start = { row = 3, column = 15 }, end = { row = 3, column = 16 } } (Gren.Syntax.Expression.Integer 2))
                                                            )
                                                    }
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "negated infix operation"
                (\() ->
                    """
module Bar exposing (..)

bar = -(1 * 2)
"""
                        |> String.trim
                        |> GrenParserLenient.run GrenParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                                            , exposingList = Gren.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } (Gren.Syntax.Exposing.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 15 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 15 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 4 } } "bar"
                                                    , arguments = []
                                                    , expression =
                                                        Gren.Syntax.Node.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 15 } }
                                                            (Gren.Syntax.Expression.Negation
                                                                (Gren.Syntax.Node.Node { start = { row = 3, column = 8 }, end = { row = 3, column = 15 } }
                                                                    (Gren.Syntax.Expression.ParenthesizedExpression
                                                                        (Gren.Syntax.Node.Node { start = { row = 3, column = 9 }, end = { row = 3, column = 14 } }
                                                                            (Gren.Syntax.Expression.OperatorApplication
                                                                                "*"
                                                                                (Gren.Syntax.Node.Node { start = { row = 3, column = 9 }, end = { row = 3, column = 10 } } (Gren.Syntax.Expression.Integer 1))
                                                                                (Gren.Syntax.Node.Node { start = { row = 3, column = 13 }, end = { row = 3, column = 14 } } (Gren.Syntax.Expression.Integer 2))
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                    }
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "infix operation into record access"
                (\() ->
                    """
module Bar exposing (..)

bar = (1 * 2).x
"""
                        |> String.trim
                        |> GrenParserLenient.run GrenParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                                            , exposingList = Gren.Syntax.Node.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } (Gren.Syntax.Exposing.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 16 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 16 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 4 } } "bar"
                                                    , arguments = []
                                                    , expression =
                                                        Gren.Syntax.Node.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 16 } }
                                                            (Gren.Syntax.Expression.RecordAccess
                                                                (Gren.Syntax.Node.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 14 } }
                                                                    (Gren.Syntax.Expression.ParenthesizedExpression
                                                                        (Gren.Syntax.Node.Node { start = { row = 3, column = 8 }, end = { row = 3, column = 13 } }
                                                                            (Gren.Syntax.Expression.OperatorApplication
                                                                                "*"
                                                                                (Gren.Syntax.Node.Node { start = { row = 3, column = 8 }, end = { row = 3, column = 9 } } (Gren.Syntax.Expression.Integer 1))
                                                                                (Gren.Syntax.Node.Node { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } } (Gren.Syntax.Expression.Integer 2))
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                                (Gren.Syntax.Node.Node { start = { row = 3, column = 15 }, end = { row = 3, column = 16 } } "x")
                                                            )
                                                    }
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "infix operators regression https://github.com/stil4m/gren-syntax/issues/41"
                (\() ->
                    """
module A exposing (..)

bool1 = True && True || True
bool2 = True || True && True

numeric1 = 1 ^ 2 * 3 + 4
numeric2 = 1 + 2 * 3 ^ 4
"""
                        |> String.trim
                        |> GrenParserLenient.run GrenParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 23 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } } [ "A" ]
                                            , exposingList =
                                                Gren.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 23 } }
                                                    (Gren.Syntax.Exposing.All { start = { row = 1, column = 20 }, end = { row = 1, column = 22 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 29 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 29 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 6 } } "bool1"
                                                    , arguments = []
                                                    , expression =
                                                        Gren.Syntax.Node.Node { start = { row = 3, column = 9 }, end = { row = 3, column = 29 } }
                                                            (Gren.Syntax.Expression.OperatorApplication "||"
                                                                (Gren.Syntax.Node.Node { start = { row = 3, column = 9 }, end = { row = 3, column = 21 } }
                                                                    (Gren.Syntax.Expression.OperatorApplication "&&"
                                                                        (Gren.Syntax.Node.Node { start = { row = 3, column = 9 }, end = { row = 3, column = 13 } } (Gren.Syntax.Expression.FunctionOrValue [] "True"))
                                                                        (Gren.Syntax.Node.Node { start = { row = 3, column = 17 }, end = { row = 3, column = 21 } } (Gren.Syntax.Expression.FunctionOrValue [] "True"))
                                                                    )
                                                                )
                                                                (Gren.Syntax.Node.Node { start = { row = 3, column = 25 }, end = { row = 3, column = 29 } } (Gren.Syntax.Expression.FunctionOrValue [] "True"))
                                                            )
                                                    }
                                            }
                                        )
                                    , Gren.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 29 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Gren.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 29 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 6 } } "bool2"
                                                    , arguments = []
                                                    , expression =
                                                        Gren.Syntax.Node.Node { start = { row = 4, column = 9 }, end = { row = 4, column = 29 } }
                                                            (Gren.Syntax.Expression.OperatorApplication "||"
                                                                (Gren.Syntax.Node.Node { start = { row = 4, column = 9 }, end = { row = 4, column = 13 } } (Gren.Syntax.Expression.FunctionOrValue [] "True"))
                                                                (Gren.Syntax.Node.Node { start = { row = 4, column = 17 }, end = { row = 4, column = 29 } }
                                                                    (Gren.Syntax.Expression.OperatorApplication "&&"
                                                                        (Gren.Syntax.Node.Node { start = { row = 4, column = 17 }, end = { row = 4, column = 21 } } (Gren.Syntax.Expression.FunctionOrValue [] "True"))
                                                                        (Gren.Syntax.Node.Node { start = { row = 4, column = 25 }, end = { row = 4, column = 29 } } (Gren.Syntax.Expression.FunctionOrValue [] "True"))
                                                                    )
                                                                )
                                                            )
                                                    }
                                            }
                                        )
                                    , Gren.Syntax.Node.Node { start = { row = 6, column = 1 }, end = { row = 6, column = 25 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Gren.Syntax.Node.Node { start = { row = 6, column = 1 }, end = { row = 6, column = 25 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 6, column = 1 }, end = { row = 6, column = 9 } } "numeric1"
                                                    , arguments = []
                                                    , expression =
                                                        Gren.Syntax.Node.Node { start = { row = 6, column = 12 }, end = { row = 6, column = 25 } }
                                                            (Gren.Syntax.Expression.OperatorApplication "+"
                                                                (Gren.Syntax.Node.Node { start = { row = 6, column = 12 }, end = { row = 6, column = 21 } }
                                                                    (Gren.Syntax.Expression.OperatorApplication "*"
                                                                        (Gren.Syntax.Node.Node { start = { row = 6, column = 12 }, end = { row = 6, column = 17 } }
                                                                            (Gren.Syntax.Expression.OperatorApplication "^"
                                                                                (Gren.Syntax.Node.Node { start = { row = 6, column = 12 }, end = { row = 6, column = 13 } } (Gren.Syntax.Expression.Integer 1))
                                                                                (Gren.Syntax.Node.Node { start = { row = 6, column = 16 }, end = { row = 6, column = 17 } } (Gren.Syntax.Expression.Integer 2))
                                                                            )
                                                                        )
                                                                        (Gren.Syntax.Node.Node { start = { row = 6, column = 20 }, end = { row = 6, column = 21 } } (Gren.Syntax.Expression.Integer 3))
                                                                    )
                                                                )
                                                                (Gren.Syntax.Node.Node
                                                                    { start = { row = 6, column = 24 }, end = { row = 6, column = 25 } }
                                                                    (Gren.Syntax.Expression.Integer 4)
                                                                )
                                                            )
                                                    }
                                            }
                                        )
                                    , Gren.Syntax.Node.Node { start = { row = 7, column = 1 }, end = { row = 7, column = 25 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Gren.Syntax.Node.Node { start = { row = 7, column = 1 }, end = { row = 7, column = 25 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 7, column = 1 }, end = { row = 7, column = 9 } } "numeric2"
                                                    , arguments = []
                                                    , expression =
                                                        Gren.Syntax.Node.Node { start = { row = 7, column = 12 }, end = { row = 7, column = 25 } }
                                                            (Gren.Syntax.Expression.OperatorApplication "+"
                                                                (Gren.Syntax.Node.Node { start = { row = 7, column = 12 }, end = { row = 7, column = 13 } } (Gren.Syntax.Expression.Integer 1))
                                                                (Gren.Syntax.Node.Node { start = { row = 7, column = 16 }, end = { row = 7, column = 25 } }
                                                                    (Gren.Syntax.Expression.OperatorApplication "*"
                                                                        (Gren.Syntax.Node.Node { start = { row = 7, column = 16 }, end = { row = 7, column = 17 } } (Gren.Syntax.Expression.Integer 2))
                                                                        (Gren.Syntax.Node.Node { start = { row = 7, column = 20 }, end = { row = 7, column = 25 } }
                                                                            (Gren.Syntax.Expression.OperatorApplication "^"
                                                                                (Gren.Syntax.Node.Node { start = { row = 7, column = 20 }, end = { row = 7, column = 21 } } (Gren.Syntax.Expression.Integer 3))
                                                                                (Gren.Syntax.Node.Node { start = { row = 7, column = 24 }, end = { row = 7, column = 25 } } (Gren.Syntax.Expression.Integer 4))
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                    }
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "infix operators associativity https://github.com/stil4m/gren-syntax/issues/87"
                (\() ->
                    """
module A exposing (..)

numeric1 = 1 + 2 - 3

pipeline1 = 1 |> 2 |> 3
"""
                        |> String.trim
                        |> GrenParserLenient.run GrenParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 23 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } } [ "A" ]
                                            , exposingList =
                                                Gren.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 23 } }
                                                    (Gren.Syntax.Exposing.All { start = { row = 1, column = 20 }, end = { row = 1, column = 22 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 21 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 21 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 9 } } "numeric1"
                                                    , arguments = []
                                                    , expression =
                                                        Gren.Syntax.Node.Node { start = { row = 3, column = 12 }, end = { row = 3, column = 21 } }
                                                            (Gren.Syntax.Expression.OperatorApplication "-"
                                                                (Gren.Syntax.Node.Node { start = { row = 3, column = 12 }, end = { row = 3, column = 17 } }
                                                                    (Gren.Syntax.Expression.OperatorApplication "+"
                                                                        (Gren.Syntax.Node.Node { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } } (Gren.Syntax.Expression.Integer 1))
                                                                        (Gren.Syntax.Node.Node { start = { row = 3, column = 16 }, end = { row = 3, column = 17 } } (Gren.Syntax.Expression.Integer 2))
                                                                    )
                                                                )
                                                                (Gren.Syntax.Node.Node { start = { row = 3, column = 20 }, end = { row = 3, column = 21 } } (Gren.Syntax.Expression.Integer 3))
                                                            )
                                                    }
                                            }
                                        )
                                    , Gren.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 24 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Gren.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 24 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 10 } } "pipeline1"
                                                    , arguments = []
                                                    , expression =
                                                        Gren.Syntax.Node.Node { start = { row = 5, column = 13 }, end = { row = 5, column = 24 } }
                                                            (Gren.Syntax.Expression.OperatorApplication "|>"
                                                                (Gren.Syntax.Node.Node { start = { row = 5, column = 13 }, end = { row = 5, column = 19 } }
                                                                    (Gren.Syntax.Expression.OperatorApplication "|>"
                                                                        (Gren.Syntax.Node.Node { start = { row = 5, column = 13 }, end = { row = 5, column = 14 } } (Gren.Syntax.Expression.Integer 1))
                                                                        (Gren.Syntax.Node.Node { start = { row = 5, column = 18 }, end = { row = 5, column = 19 } } (Gren.Syntax.Expression.Integer 2))
                                                                    )
                                                                )
                                                                (Gren.Syntax.Node.Node { start = { row = 5, column = 23 }, end = { row = 5, column = 24 } } (Gren.Syntax.Expression.Integer 3))
                                                            )
                                                    }
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "| is equivalent to |>"
                (\() ->
                    """
module A exposing (..)

pipeline0 = 1 | 2 | 3

pipeline1 = 1 |> 2 |> 3
"""
                        |> String.trim
                        |> GrenParserLenient.run GrenParserLenient.module_
                        |> Expect.equal
                            (Just
                                { moduleDefinition =
                                    Gren.Syntax.Node.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 23 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = Gren.Syntax.Node.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } } [ "A" ]
                                            , exposingList =
                                                Gren.Syntax.Node.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 23 } }
                                                    (Gren.Syntax.Exposing.All { start = { row = 1, column = 20 }, end = { row = 1, column = 22 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 22 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 22 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 10 } } "pipeline0"
                                                    , arguments = []
                                                    , expression =
                                                        Gren.Syntax.Node.Node { start = { row = 3, column = 13 }, end = { row = 3, column = 22 } }
                                                            (Gren.Syntax.Expression.OperatorApplication "|>"
                                                                (Gren.Syntax.Node.Node { start = { row = 3, column = 13 }, end = { row = 3, column = 18 } }
                                                                    (Gren.Syntax.Expression.OperatorApplication "|>"
                                                                        (Gren.Syntax.Node.Node { start = { row = 3, column = 13 }, end = { row = 3, column = 14 } } (Gren.Syntax.Expression.Integer 1))
                                                                        (Gren.Syntax.Node.Node { start = { row = 3, column = 17 }, end = { row = 3, column = 18 } } (Gren.Syntax.Expression.Integer 2))
                                                                    )
                                                                )
                                                                (Gren.Syntax.Node.Node { start = { row = 3, column = 21 }, end = { row = 3, column = 22 } } (Gren.Syntax.Expression.Integer 3))
                                                            )
                                                    }
                                            }
                                        )
                                    , Gren.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 24 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Gren.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 24 } }
                                                    { name = Gren.Syntax.Node.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 10 } } "pipeline1"
                                                    , arguments = []
                                                    , expression =
                                                        Gren.Syntax.Node.Node { start = { row = 5, column = 13 }, end = { row = 5, column = 24 } }
                                                            (Gren.Syntax.Expression.OperatorApplication "|>"
                                                                (Gren.Syntax.Node.Node { start = { row = 5, column = 13 }, end = { row = 5, column = 19 } }
                                                                    (Gren.Syntax.Expression.OperatorApplication "|>"
                                                                        (Gren.Syntax.Node.Node { start = { row = 5, column = 13 }, end = { row = 5, column = 14 } } (Gren.Syntax.Expression.Integer 1))
                                                                        (Gren.Syntax.Node.Node { start = { row = 5, column = 18 }, end = { row = 5, column = 19 } } (Gren.Syntax.Expression.Integer 2))
                                                                    )
                                                                )
                                                                (Gren.Syntax.Node.Node { start = { row = 5, column = 23 }, end = { row = 5, column = 24 } } (Gren.Syntax.Expression.Integer 3))
                                                            )
                                                    }
                                            }
                                        )
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "!= is equivalent to /="
                (\() ->
                    """
module A exposing (..)

pipeline0 = 1 != 2

pipeline1 = 1 /= 2
"""
                        |> String.trim
                        |> GrenParserLenient.run GrenParserLenient.module_
                        |> Expect.equal
                            (Just
                                { comments = []
                                , declarations =
                                    [ Gren.Syntax.Node.Node { end = { column = 19, row = 3 }, start = { column = 1, row = 3 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { declaration =
                                                Gren.Syntax.Node.Node { end = { column = 19, row = 3 }, start = { column = 1, row = 3 } }
                                                    { arguments = []
                                                    , expression =
                                                        Gren.Syntax.Node.Node { end = { column = 19, row = 3 }, start = { column = 13, row = 3 } }
                                                            (Gren.Syntax.Expression.OperatorApplication "/="
                                                                (Gren.Syntax.Node.Node { end = { column = 14, row = 3 }, start = { column = 13, row = 3 } }
                                                                    (Gren.Syntax.Expression.Integer 1)
                                                                )
                                                                (Gren.Syntax.Node.Node { end = { column = 19, row = 3 }, start = { column = 18, row = 3 } }
                                                                    (Gren.Syntax.Expression.Integer 2)
                                                                )
                                                            )
                                                    , name = Gren.Syntax.Node.Node { end = { column = 10, row = 3 }, start = { column = 1, row = 3 } } "pipeline0"
                                                    }
                                            , documentation = Nothing
                                            , signature = Nothing
                                            }
                                        )
                                    , Gren.Syntax.Node.Node { end = { column = 19, row = 5 }, start = { column = 1, row = 5 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { declaration =
                                                Gren.Syntax.Node.Node { end = { column = 19, row = 5 }, start = { column = 1, row = 5 } }
                                                    { arguments = []
                                                    , expression =
                                                        Gren.Syntax.Node.Node { end = { column = 19, row = 5 }, start = { column = 13, row = 5 } }
                                                            (Gren.Syntax.Expression.OperatorApplication "/="
                                                                (Gren.Syntax.Node.Node { end = { column = 14, row = 5 }, start = { column = 13, row = 5 } }
                                                                    (Gren.Syntax.Expression.Integer 1)
                                                                )
                                                                (Gren.Syntax.Node.Node { end = { column = 19, row = 5 }, start = { column = 18, row = 5 } }
                                                                    (Gren.Syntax.Expression.Integer 2)
                                                                )
                                                            )
                                                    , name = Gren.Syntax.Node.Node { end = { column = 10, row = 5 }, start = { column = 1, row = 5 } } "pipeline1"
                                                    }
                                            , documentation = Nothing
                                            , signature = Nothing
                                            }
                                        )
                                    ]
                                , imports = []
                                , moduleDefinition =
                                    Gren.Syntax.Node.Node { end = { column = 23, row = 1 }, start = { column = 1, row = 1 } }
                                        (GrenSyntax.NormalModule
                                            { exposingList =
                                                Gren.Syntax.Node.Node { end = { column = 23, row = 1 }, start = { column = 10, row = 1 } }
                                                    (Gren.Syntax.Exposing.All { end = { column = 22, row = 1 }, start = { column = 20, row = 1 } })
                                            , moduleName = Gren.Syntax.Node.Node { end = { column = 9, row = 1 }, start = { column = 8, row = 1 } } [ "A" ]
                                            }
                                        )
                                }
                            )
                )
            , Test.test "!== is equivalent to /="
                (\() ->
                    """
module A exposing (..)

pipeline0 = 1 !== 2

pipeline1 = 1 /= 2
"""
                        |> String.trim
                        |> GrenParserLenient.run GrenParserLenient.module_
                        |> Expect.equal
                            (Just
                                { comments = []
                                , declarations =
                                    [ Gren.Syntax.Node.Node { end = { column = 20, row = 3 }, start = { column = 1, row = 3 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { declaration =
                                                Gren.Syntax.Node.Node { end = { column = 20, row = 3 }, start = { column = 1, row = 3 } }
                                                    { arguments = []
                                                    , expression =
                                                        Gren.Syntax.Node.Node { end = { column = 20, row = 3 }, start = { column = 13, row = 3 } }
                                                            (Gren.Syntax.Expression.OperatorApplication "/="
                                                                (Gren.Syntax.Node.Node { end = { column = 14, row = 3 }, start = { column = 13, row = 3 } }
                                                                    (Gren.Syntax.Expression.Integer 1)
                                                                )
                                                                (Gren.Syntax.Node.Node { end = { column = 20, row = 3 }, start = { column = 19, row = 3 } }
                                                                    (Gren.Syntax.Expression.Integer 2)
                                                                )
                                                            )
                                                    , name = Gren.Syntax.Node.Node { end = { column = 10, row = 3 }, start = { column = 1, row = 3 } } "pipeline0"
                                                    }
                                            , documentation = Nothing
                                            , signature = Nothing
                                            }
                                        )
                                    , Gren.Syntax.Node.Node { end = { column = 19, row = 5 }, start = { column = 1, row = 5 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { declaration =
                                                Gren.Syntax.Node.Node { end = { column = 19, row = 5 }, start = { column = 1, row = 5 } }
                                                    { arguments = []
                                                    , expression =
                                                        Gren.Syntax.Node.Node { end = { column = 19, row = 5 }, start = { column = 13, row = 5 } }
                                                            (Gren.Syntax.Expression.OperatorApplication "/="
                                                                (Gren.Syntax.Node.Node { end = { column = 14, row = 5 }, start = { column = 13, row = 5 } }
                                                                    (Gren.Syntax.Expression.Integer 1)
                                                                )
                                                                (Gren.Syntax.Node.Node { end = { column = 19, row = 5 }, start = { column = 18, row = 5 } }
                                                                    (Gren.Syntax.Expression.Integer 2)
                                                                )
                                                            )
                                                    , name = Gren.Syntax.Node.Node { end = { column = 10, row = 5 }, start = { column = 1, row = 5 } } "pipeline1"
                                                    }
                                            , documentation = Nothing
                                            , signature = Nothing
                                            }
                                        )
                                    ]
                                , imports = []
                                , moduleDefinition =
                                    Gren.Syntax.Node.Node { end = { column = 23, row = 1 }, start = { column = 1, row = 1 } }
                                        (GrenSyntax.NormalModule
                                            { exposingList =
                                                Gren.Syntax.Node.Node { end = { column = 23, row = 1 }, start = { column = 10, row = 1 } }
                                                    (Gren.Syntax.Exposing.All { end = { column = 22, row = 1 }, start = { column = 20, row = 1 } })
                                            , moduleName = Gren.Syntax.Node.Node { end = { column = 9, row = 1 }, start = { column = 8, row = 1 } } [ "A" ]
                                            }
                                        )
                                }
                            )
                )
            , Test.test "=== is equivalent to =="
                (\() ->
                    """
module A exposing (..)

pipeline0 = 1 === 2

pipeline1 = 1 == 2
"""
                        |> String.trim
                        |> GrenParserLenient.run GrenParserLenient.module_
                        |> Expect.equal
                            (Just
                                { comments = []
                                , declarations =
                                    [ Gren.Syntax.Node.Node { end = { column = 20, row = 3 }, start = { column = 1, row = 3 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { declaration =
                                                Gren.Syntax.Node.Node { end = { column = 20, row = 3 }, start = { column = 1, row = 3 } }
                                                    { arguments = []
                                                    , expression =
                                                        Gren.Syntax.Node.Node { end = { column = 20, row = 3 }, start = { column = 13, row = 3 } }
                                                            (Gren.Syntax.Expression.OperatorApplication "=="
                                                                (Gren.Syntax.Node.Node { end = { column = 14, row = 3 }, start = { column = 13, row = 3 } }
                                                                    (Gren.Syntax.Expression.Integer 1)
                                                                )
                                                                (Gren.Syntax.Node.Node { end = { column = 20, row = 3 }, start = { column = 19, row = 3 } }
                                                                    (Gren.Syntax.Expression.Integer 2)
                                                                )
                                                            )
                                                    , name = Gren.Syntax.Node.Node { end = { column = 10, row = 3 }, start = { column = 1, row = 3 } } "pipeline0"
                                                    }
                                            , documentation = Nothing
                                            , signature = Nothing
                                            }
                                        )
                                    , Gren.Syntax.Node.Node { end = { column = 19, row = 5 }, start = { column = 1, row = 5 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { declaration =
                                                Gren.Syntax.Node.Node { end = { column = 19, row = 5 }, start = { column = 1, row = 5 } }
                                                    { arguments = []
                                                    , expression =
                                                        Gren.Syntax.Node.Node { end = { column = 19, row = 5 }, start = { column = 13, row = 5 } }
                                                            (Gren.Syntax.Expression.OperatorApplication "=="
                                                                (Gren.Syntax.Node.Node { end = { column = 14, row = 5 }, start = { column = 13, row = 5 } }
                                                                    (Gren.Syntax.Expression.Integer 1)
                                                                )
                                                                (Gren.Syntax.Node.Node { end = { column = 19, row = 5 }, start = { column = 18, row = 5 } }
                                                                    (Gren.Syntax.Expression.Integer 2)
                                                                )
                                                            )
                                                    , name = Gren.Syntax.Node.Node { end = { column = 10, row = 5 }, start = { column = 1, row = 5 } } "pipeline1"
                                                    }
                                            , documentation = Nothing
                                            , signature = Nothing
                                            }
                                        )
                                    ]
                                , imports = []
                                , moduleDefinition =
                                    Gren.Syntax.Node.Node { end = { column = 23, row = 1 }, start = { column = 1, row = 1 } }
                                        (GrenSyntax.NormalModule
                                            { exposingList =
                                                Gren.Syntax.Node.Node { end = { column = 23, row = 1 }, start = { column = 10, row = 1 } }
                                                    (Gren.Syntax.Exposing.All { end = { column = 22, row = 1 }, start = { column = 20, row = 1 } })
                                            , moduleName = Gren.Syntax.Node.Node { end = { column = 9, row = 1 }, start = { column = 8, row = 1 } } [ "A" ]
                                            }
                                        )
                                }
                            )
                )
            , Test.test "** is equivalent to ^"
                (\() ->
                    """
module A exposing (..)

pipeline0 = 1 ** 2

pipeline1 = 1 ^ 2
"""
                        |> String.trim
                        |> GrenParserLenient.run GrenParserLenient.module_
                        |> Expect.equal
                            (Just
                                { comments = []
                                , declarations =
                                    [ Gren.Syntax.Node.Node { end = { column = 19, row = 3 }, start = { column = 1, row = 3 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { declaration =
                                                Gren.Syntax.Node.Node { end = { column = 19, row = 3 }, start = { column = 1, row = 3 } }
                                                    { arguments = []
                                                    , expression =
                                                        Gren.Syntax.Node.Node { end = { column = 19, row = 3 }, start = { column = 13, row = 3 } }
                                                            (Gren.Syntax.Expression.OperatorApplication "^"
                                                                (Gren.Syntax.Node.Node { end = { column = 14, row = 3 }, start = { column = 13, row = 3 } }
                                                                    (Gren.Syntax.Expression.Integer 1)
                                                                )
                                                                (Gren.Syntax.Node.Node { end = { column = 19, row = 3 }, start = { column = 18, row = 3 } }
                                                                    (Gren.Syntax.Expression.Integer 2)
                                                                )
                                                            )
                                                    , name = Gren.Syntax.Node.Node { end = { column = 10, row = 3 }, start = { column = 1, row = 3 } } "pipeline0"
                                                    }
                                            , documentation = Nothing
                                            , signature = Nothing
                                            }
                                        )
                                    , Gren.Syntax.Node.Node { end = { column = 18, row = 5 }, start = { column = 1, row = 5 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { declaration =
                                                Gren.Syntax.Node.Node { end = { column = 18, row = 5 }, start = { column = 1, row = 5 } }
                                                    { arguments = []
                                                    , expression =
                                                        Gren.Syntax.Node.Node { end = { column = 18, row = 5 }, start = { column = 13, row = 5 } }
                                                            (Gren.Syntax.Expression.OperatorApplication "^"
                                                                (Gren.Syntax.Node.Node { end = { column = 14, row = 5 }, start = { column = 13, row = 5 } }
                                                                    (Gren.Syntax.Expression.Integer 1)
                                                                )
                                                                (Gren.Syntax.Node.Node { end = { column = 18, row = 5 }, start = { column = 17, row = 5 } }
                                                                    (Gren.Syntax.Expression.Integer 2)
                                                                )
                                                            )
                                                    , name = Gren.Syntax.Node.Node { end = { column = 10, row = 5 }, start = { column = 1, row = 5 } } "pipeline1"
                                                    }
                                            , documentation = Nothing
                                            , signature = Nothing
                                            }
                                        )
                                    ]
                                , imports = []
                                , moduleDefinition =
                                    Gren.Syntax.Node.Node { end = { column = 23, row = 1 }, start = { column = 1, row = 1 } }
                                        (GrenSyntax.NormalModule
                                            { exposingList =
                                                Gren.Syntax.Node.Node { end = { column = 23, row = 1 }, start = { column = 10, row = 1 } }
                                                    (Gren.Syntax.Exposing.All { end = { column = 22, row = 1 }, start = { column = 20, row = 1 } })
                                            , moduleName = Gren.Syntax.Node.Node { end = { column = 9, row = 1 }, start = { column = 8, row = 1 } } [ "A" ]
                                            }
                                        )
                                }
                            )
                )
            ]
        ]


parseSource : String -> GrenParserLenient.Parser a -> Maybe a
parseSource source parser =
    GrenParserLenient.run parser source


moduleExpectInvalid : String -> Expect.Expectation
moduleExpectInvalid source =
    case GrenParserLenient.run GrenParserLenient.module_ source of
        Nothing ->
            Expect.pass

        Just actual ->
            Expect.fail ("This source code is successfully parsed but it shouldn't:\n" ++ Debug.toString actual)


longString : String
longString =
    "\"" ++ String.repeat (5 * 10 ^ 5) "a" ++ "\""


longMultiLineString : String
longMultiLineString =
    "\"\"\"" ++ String.repeat (5 * 10 ^ 5) "a" ++ "\"\"\""


expectSyntaxWithoutComments :
    GrenParserLenient.Parser { comments : GrenParserLenient.Comments, syntax : a }
    -> a
    -> String
    -> Expect.Expectation
expectSyntaxWithoutComments parser expected source =
    case GrenParserLenient.run parser source of
        Nothing ->
            Expect.fail "Expected the source to be parsed correctly"

        Just actual ->
            Expect.all
                [ \() -> actual.syntax |> Expect.equal expected
                , \() ->
                    actual.comments
                        |> GrenParserLenient.commentsToList
                        |> Expect.equalLists []
                        |> Expect.onFail "This parser should not produce any comments. If this is expected, then you should use expectAstWithComments instead."
                ]
                ()


expectSyntaxWithComments :
    GrenParserLenient.Parser { comments : GrenParserLenient.Comments, syntax : a }
    -> { syntax : a, comments : List (Gren.Syntax.Node.Node String) }
    -> String
    -> Expect.Expectation
expectSyntaxWithComments parser expected source =
    case GrenParserLenient.run parser source of
        Nothing ->
            Expect.fail "Expected the source to be parsed correctly"

        Just actual ->
            Expect.all
                [ \() -> actual.syntax |> Expect.equal expected.syntax
                , \() -> actual.comments |> GrenParserLenient.commentsToList |> Expect.equal expected.comments
                ]
                ()


expectFailsToParse :
    GrenParserLenient.Parser { comments : GrenParserLenient.Comments, syntax : a_ }
    -> String
    -> Expect.Expectation
expectFailsToParse parser source =
    case source |> GrenParserLenient.run parser of
        Nothing ->
            Expect.pass

        Just actual ->
            Expect.fail ("This source code is successfully parsed but it shouldn't:\n" ++ Debug.toString actual)
