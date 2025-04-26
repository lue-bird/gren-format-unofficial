module GrainParserLenientTest exposing (all)

import Expect
import GrainParserLenientTestFullModules
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
                    |> Maybe.map GrenSyntax.nodeValue
                    |> Expect.equal (Just [ "Foo" ])
            )
        , Test.test "moduleNameDir"
            (\() ->
                "Foo.Bar"
                    |> GrenParserLenient.run GrenParserLenient.moduleName
                    |> Maybe.map GrenSyntax.nodeValue
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
                                (Just (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } "--bar"))
                    )
                , Test.test "singleLineComment state"
                    (\() ->
                        GrenParserLenient.run GrenParserLenient.singleLineComment "--bar"
                            |> Expect.equal
                                (Just (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } "--bar"))
                    )
                , Test.test "singleLineComment including 2-part utf-16 char range"
                    (\() ->
                        GrenParserLenient.run GrenParserLenient.singleLineComment "--bar🔧"
                            |> Expect.equal
                                (Just (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } } "--bar🔧"))
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
                                (Just (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } "{-foo\nbar-}"))
                    )
                , Test.test "multilineComment range"
                    (\() ->
                        GrenParserLenient.run GrenParserLenient.multiLineComment "{-foo\nbar-}"
                            |> Expect.equal
                                (Just (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } "{-foo\nbar-}"))
                    )
                , Test.test "multilineComment including 2-part utf-16 char range"
                    (\() ->
                        GrenParserLenient.run GrenParserLenient.multiLineComment "{-foo\nbar🔧-}"
                            |> Expect.equal
                                (Just (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 7 } } "{-foo\nbar🔧-}"))
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
                                (Just (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } "{- {- -} -}"))
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 26 } }
                                (GrenSyntax.NormalModule
                                    { moduleName = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Foo" ]
                                    , exposingList =
                                        GrenSyntax.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 26 } }
                                            (GrenSyntax.Explicit
                                                [ GrenSyntax.Node { start = { row = 1, column = 22 }, end = { row = 1, column = 25 } } (GrenSyntax.TypeOrAliasExpose "Bar")
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 31 } }
                                (GrenSyntax.PortModule
                                    { moduleName = GrenSyntax.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 16 } } [ "Foo" ]
                                    , exposingList =
                                        GrenSyntax.Node { start = { row = 1, column = 17 }, end = { row = 1, column = 31 } }
                                            (GrenSyntax.Explicit
                                                [ GrenSyntax.Node { start = { row = 1, column = 27 }, end = { row = 1, column = 30 } } (GrenSyntax.TypeOrAliasExpose "Bar") ]
                                            )
                                    }
                                )
                            )
                )
            , Test.test "port moduleDefinition with spacing"
                (\() ->
                    "port module Foo exposing ( Bar )"
                        |> expectSyntaxWithoutComments GrenParserLenient.moduleHeader
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 33 } }
                                (GrenSyntax.PortModule
                                    { moduleName = GrenSyntax.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 16 } } [ "Foo" ]
                                    , exposingList =
                                        GrenSyntax.Node { start = { row = 1, column = 17 }, end = { row = 1, column = 33 } }
                                            (GrenSyntax.Explicit
                                                [ GrenSyntax.Node { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } } (GrenSyntax.TypeOrAliasExpose "Bar") ]
                                            )
                                    }
                                )
                            )
                )
            , Test.test "effect moduleDefinition"
                (\() ->
                    "effect module Foo where {command = MyCmd, subscription = MySub } exposing (Bar)"
                        |> expectSyntaxWithoutComments GrenParserLenient.moduleHeader
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 80 } }
                                (GrenSyntax.EffectModule
                                    { moduleName = GrenSyntax.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 18 } } [ "Foo" ]
                                    , exposingList = GrenSyntax.Node { start = { row = 1, column = 66 }, end = { row = 1, column = 80 } } (GrenSyntax.Explicit [ GrenSyntax.Node { start = { row = 1, column = 76 }, end = { row = 1, column = 79 } } (GrenSyntax.TypeOrAliasExpose "Bar") ])
                                    , command = Just (GrenSyntax.Node { start = { row = 1, column = 36 }, end = { row = 1, column = 41 } } "MyCmd")
                                    , subscription = Just (GrenSyntax.Node { start = { row = 1, column = 58 }, end = { row = 1, column = 63 } } "MySub")
                                    }
                                )
                            )
                )
            , Test.test "unformatted"
                (\() ->
                    "module \n Foo \n exposing  (..)"
                        |> expectSyntaxWithoutComments GrenParserLenient.moduleHeader
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 3, column = 16 } }
                                (GrenSyntax.NormalModule
                                    { moduleName = GrenSyntax.Node { start = { row = 2, column = 2 }, end = { row = 2, column = 5 } } [ "Foo" ]
                                    , exposingList =
                                        GrenSyntax.Node { start = { row = 3, column = 2 }, end = { row = 3, column = 16 } }
                                            (GrenSyntax.All { start = { row = 3, column = 13 }, end = { row = 3, column = 15 } })
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                (GrenSyntax.NormalModule
                                    { moduleName = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Foo" ]
                                    , exposingList =
                                        GrenSyntax.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } }
                                            (GrenSyntax.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                                    }
                                )
                            )
                )
            , Test.test "exposing ..."
                (\() ->
                    "module Foo exposing (...)"
                        |> expectSyntaxWithoutComments GrenParserLenient.moduleHeader
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 26 } }
                                (GrenSyntax.NormalModule
                                    { moduleName = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Foo" ]
                                    , exposingList =
                                        GrenSyntax.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 26 } }
                                            (GrenSyntax.All { start = { row = 1, column = 22 }, end = { row = 1, column = 25 } })
                                    }
                                )
                            )
                )
            , Test.test "module name with _"
                (\() ->
                    "module I_en_gb exposing (..)"
                        |> expectSyntaxWithoutComments GrenParserLenient.moduleHeader
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 29 } }
                                (GrenSyntax.NormalModule
                                    { moduleName = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 15 } } [ "I_en_gb" ]
                                    , exposingList =
                                        GrenSyntax.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 29 } }
                                            (GrenSyntax.All { start = { row = 1, column = 26 }, end = { row = 1, column = 28 } })
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
                                (GrenSyntax.All { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } })
                    )
                , Test.test "Exposing all with spacing and comment"
                    (\() ->
                        """(
  .. -- foo
  )"""
                            |> expectSyntaxWithComments GrenParserLenient.exposing_
                                { syntax = GrenSyntax.All { start = { row = 2, column = 3 }, end = { row = 3, column = 3 } }
                                , comments = [ GrenSyntax.Node { start = { row = 2, column = 6 }, end = { row = 2, column = 12 } } "-- foo" ]
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
                                (GrenSyntax.Explicit
                                    [ GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } (GrenSyntax.TypeOrAliasExpose "Model")
                                    , GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 15 } }
                                        (GrenSyntax.TypeExpose
                                            { name = "Msg"
                                            , open = Just { start = { row = 1, column = 11 }, end = { row = 1, column = 15 } }
                                            }
                                        )
                                    , GrenSyntax.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 24 } }
                                        (GrenSyntax.TypeExpose
                                            { name = "Info"
                                            , open = Just { start = { row = 1, column = 20 }, end = { row = 1, column = 24 } }
                                            }
                                        )
                                    , GrenSyntax.Node { start = { row = 1, column = 25 }, end = { row = 1, column = 29 } } (GrenSyntax.FunctionExpose "init")
                                    , GrenSyntax.Node { start = { row = 1, column = 30 }, end = { row = 1, column = 34 } } (GrenSyntax.InfixExpose "::")
                                    ]
                                )
                    )
                , Test.test "exposingList with spacing on one line"
                    (\() ->
                        "(Model, Msg, Info   (..)   ,init,(::) )"
                            |> expectSyntaxWithoutComments GrenParserLenient.exposing_
                                (GrenSyntax.Explicit
                                    [ GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } (GrenSyntax.TypeOrAliasExpose "Model")
                                    , GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } (GrenSyntax.TypeOrAliasExpose "Msg")
                                    , GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } }
                                        (GrenSyntax.TypeExpose
                                            { name = "Info"
                                            , open = Just { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } }
                                            }
                                        )
                                    , GrenSyntax.Node { start = { row = 1, column = 29 }, end = { row = 1, column = 33 } } (GrenSyntax.FunctionExpose "init")
                                    , GrenSyntax.Node { start = { row = 1, column = 34 }, end = { row = 1, column = 38 } } (GrenSyntax.InfixExpose "::")
                                    ]
                                )
                    )
                , Test.test "exposingList with extra commas between exposes"
                    (\() ->
                        "(Model,,Msg,,Info   (..)  ,,init,(::) )"
                            |> expectSyntaxWithoutComments GrenParserLenient.exposing_
                                (GrenSyntax.Explicit
                                    [ GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } (GrenSyntax.TypeOrAliasExpose "Model")
                                    , GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } (GrenSyntax.TypeOrAliasExpose "Msg")
                                    , GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } }
                                        (GrenSyntax.TypeExpose
                                            { name = "Info"
                                            , open = Just { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } }
                                            }
                                        )
                                    , GrenSyntax.Node { start = { row = 1, column = 29 }, end = { row = 1, column = 33 } } (GrenSyntax.FunctionExpose "init")
                                    , GrenSyntax.Node { start = { row = 1, column = 34 }, end = { row = 1, column = 38 } } (GrenSyntax.InfixExpose "::")
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
                                (GrenSyntax.Explicit
                                    [ GrenSyntax.Node { start = { row = 2, column = 7 }, end = { row = 2, column = 8 } } (GrenSyntax.TypeOrAliasExpose "A")
                                    , GrenSyntax.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 13 } }
                                        (GrenSyntax.TypeExpose
                                            { name = "B"
                                            , open = Just { start = { row = 3, column = 8 }, end = { row = 3, column = 13 } }
                                            }
                                        )
                                    , GrenSyntax.Node { start = { row = 4, column = 7 }, end = { row = 4, column = 16 } }
                                        (GrenSyntax.TypeExpose
                                            { name = "Info"
                                            , open = Just { start = { row = 4, column = 12 }, end = { row = 4, column = 16 } }
                                            }
                                        )
                                    , GrenSyntax.Node { start = { row = 5, column = 12 }, end = { row = 5, column = 16 } } (GrenSyntax.FunctionExpose "init")
                                    , GrenSyntax.Node { start = { row = 6, column = 2 }, end = { row = 6, column = 6 } } (GrenSyntax.InfixExpose "::")
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
                                (GrenSyntax.Explicit
                                    [ GrenSyntax.Node { start = { row = 2, column = 7 }, end = { row = 2, column = 8 } } (GrenSyntax.TypeOrAliasExpose "A")
                                    , GrenSyntax.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 12 } }
                                        (GrenSyntax.TypeExpose
                                            { name = "B"
                                            , open = Just { start = { row = 3, column = 8 }, end = { row = 3, column = 12 } }
                                            }
                                        )
                                    , GrenSyntax.Node { start = { row = 4, column = 7 }, end = { row = 4, column = 16 } }
                                        (GrenSyntax.TypeExpose
                                            { name = "Info"
                                            , open = Just { start = { row = 4, column = 12 }, end = { row = 4, column = 16 } }
                                            }
                                        )
                                    , GrenSyntax.Node { start = { row = 5, column = 12 }, end = { row = 5, column = 16 } } (GrenSyntax.FunctionExpose "init")
                                    , GrenSyntax.Node { start = { row = 6, column = 2 }, end = { row = 6, column = 6 } } (GrenSyntax.InfixExpose "::")
                                    ]
                                )
                    )
                , Test.test "Comments inside the exposing clause"
                    (\() ->
                        "(foo\n --bar\n )"
                            |> expectSyntaxWithComments GrenParserLenient.exposing_
                                { syntax =
                                    GrenSyntax.Explicit
                                        [ GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 5 } }
                                            (GrenSyntax.FunctionExpose "foo")
                                        ]
                                , comments = [ GrenSyntax.Node { start = { row = 2, column = 2 }, end = { row = 2, column = 7 } } "--bar" ]
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
                                    GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName =
                                                GrenSyntax.Node
                                                    { start = { row = 1, column = 8 }
                                                    , end = { row = 1, column = 18 }
                                                    }
                                                    [ "TestModule" ]
                                            , exposingList =
                                                GrenSyntax.Node
                                                    { start = { row = 1, column = 19 }
                                                    , end =
                                                        { row = 1
                                                        , column = 32
                                                        }
                                                    }
                                                    (GrenSyntax.All { start = { row = 1, column = 29 }, end = { row = 1, column = 31 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 7, column = 10 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 7, column = 10 } }
                                                    { name = GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 2 } } "a"
                                                    , arguments = []
                                                    , expression =
                                                        GrenSyntax.Node { start = { row = 4, column = 5 }, end = { row = 7, column = 10 } }
                                                            (GrenSyntax.IfBlock
                                                                (GrenSyntax.Node { start = { row = 4, column = 8 }, end = { row = 4, column = 12 } }
                                                                    (GrenSyntax.FunctionOrValue [] "cond")
                                                                )
                                                                (GrenSyntax.Node { start = { row = 5, column = 9 }, end = { row = 5, column = 10 } } (GrenSyntax.Integer 1))
                                                                (GrenSyntax.Node
                                                                    { start =
                                                                        { row = 7
                                                                        , column = 9
                                                                        }
                                                                    , end = { row = 7, column = 10 }
                                                                    }
                                                                    (GrenSyntax.Integer 2)
                                                                )
                                                            )
                                                    }
                                            }
                                        )
                                    , GrenSyntax.Node { start = { row = 11, column = 1 }, end = { row = 13, column = 6 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Just (GrenSyntax.Node { start = { row = 11, column = 1 }, end = { row = 12, column = 3 } } "{-| doc\n-}")
                                            , signature = Nothing
                                            , declaration =
                                                GrenSyntax.Node
                                                    { start = { row = 13, column = 1 }
                                                    , end =
                                                        { row = 13
                                                        , column = 6
                                                        }
                                                    }
                                                    { name = GrenSyntax.Node { start = { row = 13, column = 1 }, end = { row = 13, column = 2 } } "b"
                                                    , arguments = []
                                                    , expression = GrenSyntax.Node { start = { row = 13, column = 5 }, end = { row = 13, column = 6 } } (GrenSyntax.Integer 3)
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
                                    GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 18 } } [ "TestModule" ]
                                            , exposingList =
                                                GrenSyntax.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 32 } }
                                                    (GrenSyntax.All
                                                        { start = { row = 1, column = 29 }
                                                        , end = { row = 1, column = 31 }
                                                        }
                                                    )
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ GrenSyntax.Node
                                        { start = { row = 3, column = 1 }
                                        , end = { row = 4, column = 6 }
                                        }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 4, column = 6 } }
                                                    { name = GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 2 } } "a"
                                                    , arguments = []
                                                    , expression =
                                                        GrenSyntax.Node
                                                            { start = { row = 4, column = 5 }
                                                            , end = { row = 4, column = 6 }
                                                            }
                                                            (GrenSyntax.Integer 2)
                                                    }
                                            }
                                        )
                                    , GrenSyntax.Node
                                        { start = { row = 8, column = 1 }
                                        , end = { row = 10, column = 6 }
                                        }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Just (GrenSyntax.Node { start = { row = 8, column = 1 }, end = { row = 9, column = 3 } } "{-| doc\n-}")
                                            , signature = Nothing
                                            , declaration =
                                                GrenSyntax.Node { start = { row = 10, column = 1 }, end = { row = 10, column = 6 } }
                                                    { name = GrenSyntax.Node { start = { row = 10, column = 1 }, end = { row = 10, column = 2 } } "b"
                                                    , arguments = []
                                                    , expression =
                                                        GrenSyntax.Node
                                                            { start = { row = 10, column = 5 }
                                                            , end = { row = 10, column = 6 }
                                                            }
                                                            (GrenSyntax.Integer 3)
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
                                    GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 18 } } [ "TestModule" ]
                                            , exposingList =
                                                GrenSyntax.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 32 } }
                                                    (GrenSyntax.All { start = { row = 1, column = 29 }, end = { row = 1, column = 31 } })
                                            }
                                        )
                                , imports =
                                    [ GrenSyntax.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 9 } }
                                        { moduleName = GrenSyntax.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 9 } } [ "A" ]
                                        , moduleAlias = Nothing
                                        , exposingList = Nothing
                                        }
                                    , GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 9 } }
                                        { moduleName = GrenSyntax.Node { start = { row = 3, column = 8 }, end = { row = 3, column = 9 } } [ "B" ]
                                        , moduleAlias = Nothing
                                        , exposingList = Nothing
                                        }
                                    ]
                                , declarations =
                                    [ GrenSyntax.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 6 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                GrenSyntax.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 6 } }
                                                    { name = GrenSyntax.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 2 } } "a"
                                                    , arguments = []
                                                    , expression = GrenSyntax.Node { start = { row = 5, column = 5 }, end = { row = 5, column = 6 } } (GrenSyntax.Integer 1)
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
                                    GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 18 } } [ "TestModule" ]
                                            , exposingList = GrenSyntax.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 32 } } (GrenSyntax.All { start = { row = 1, column = 29 }, end = { row = 1, column = 31 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ GrenSyntax.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 15 } }
                                        (GrenSyntax.CustomTypeDeclaration
                                            { documentation = Nothing
                                            , name = GrenSyntax.Node { start = { row = 2, column = 6 }, end = { row = 2, column = 7 } } "A"
                                            , generics = []
                                            , constructors =
                                                [ GrenSyntax.Node { start = { row = 2, column = 10 }, end = { row = 2, column = 11 } }
                                                    { name = GrenSyntax.Node { start = { row = 2, column = 10 }, end = { row = 2, column = 11 } } "B"
                                                    , arguments = []
                                                    }
                                                , GrenSyntax.Node { start = { row = 2, column = 14 }, end = { row = 2, column = 15 } }
                                                    { name = GrenSyntax.Node { start = { row = 2, column = 14 }, end = { row = 2, column = 15 } } "C"
                                                    , arguments = []
                                                    }
                                                ]
                                            }
                                        )
                                    , GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 6 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 6 } }
                                                    { name = GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 2 } } "a"
                                                    , arguments = []
                                                    , expression = GrenSyntax.Node { start = { row = 3, column = 5 }, end = { row = 3, column = 6 } } (GrenSyntax.Integer 1)
                                                    }
                                            }
                                        )
                                    , GrenSyntax.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 17 } }
                                        (GrenSyntax.AliasDeclaration
                                            { documentation = Nothing
                                            , name = GrenSyntax.Node { start = { row = 4, column = 12 }, end = { row = 4, column = 13 } } "B"
                                            , generics = []
                                            , typeAnnotation =
                                                GrenSyntax.Node { start = { row = 4, column = 16 }, end = { row = 4, column = 17 } }
                                                    (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 4, column = 16 }, end = { row = 4, column = 17 } } ( [], "A" )) [])
                                            }
                                        )
                                    , GrenSyntax.Node { start = { row = 5, column = 1 }, end = { row = 6, column = 6 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature =
                                                Just
                                                    (GrenSyntax.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 8 } }
                                                        { name = GrenSyntax.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 2 } } "b"
                                                        , typeAnnotation = GrenSyntax.Node { start = { row = 5, column = 5 }, end = { row = 5, column = 8 } } (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 5, column = 5 }, end = { row = 5, column = 8 } } ( [], "Int" )) [])
                                                        }
                                                    )
                                            , declaration =
                                                GrenSyntax.Node { start = { row = 6, column = 1 }, end = { row = 6, column = 6 } }
                                                    { name = GrenSyntax.Node { start = { row = 6, column = 1 }, end = { row = 6, column = 2 } } "b"
                                                    , arguments = []
                                                    , expression = GrenSyntax.Node { start = { row = 6, column = 5 }, end = { row = 6, column = 6 } } (GrenSyntax.Integer 2)
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
                                { moduleDefinition = GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 31 } } (GrenSyntax.NormalModule { moduleName = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } } [ "A" ], exposingList = GrenSyntax.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 31 } } (GrenSyntax.Explicit [ GrenSyntax.Node { start = { row = 1, column = 20 }, end = { row = 1, column = 24 } } (GrenSyntax.FunctionExpose "fun1"), GrenSyntax.Node { start = { row = 1, column = 26 }, end = { row = 1, column = 30 } } (GrenSyntax.FunctionExpose "fun2") ]) })
                                , imports = []
                                , declarations =
                                    [ GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 5, column = 11 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 5, column = 11 } }
                                                    { name = GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } } "fun1"
                                                    , arguments = [ GrenSyntax.Node { start = { row = 3, column = 6 }, end = { row = 3, column = 7 } } (GrenSyntax.PatternVariable "n") ]
                                                    , expression =
                                                        GrenSyntax.Node { start = { row = 4, column = 3 }, end = { row = 5, column = 11 } }
                                                            (GrenSyntax.OperatorApplication "+"
                                                                (GrenSyntax.Node { start = { row = 4, column = 3 }, end = { row = 4, column = 9 } }
                                                                    (GrenSyntax.Application
                                                                        [ GrenSyntax.Node { start = { row = 4, column = 3 }, end = { row = 4, column = 7 } } (GrenSyntax.FunctionOrValue [] "fun2")
                                                                        , GrenSyntax.Node { start = { row = 4, column = 8 }, end = { row = 4, column = 9 } } (GrenSyntax.FunctionOrValue [] "n")
                                                                        ]
                                                                    )
                                                                )
                                                                (GrenSyntax.Node { start = { row = 5, column = 5 }, end = { row = 5, column = 11 } }
                                                                    (GrenSyntax.Application
                                                                        [ GrenSyntax.Node { start = { row = 5, column = 5 }, end = { row = 5, column = 9 } } (GrenSyntax.FunctionOrValue [] "fun2")
                                                                        , GrenSyntax.Node { start = { row = 5, column = 10 }, end = { row = 5, column = 11 } } (GrenSyntax.FunctionOrValue [] "n")
                                                                        ]
                                                                    )
                                                                )
                                                            )
                                                    }
                                            }
                                        )
                                    , GrenSyntax.Node { start = { row = 7, column = 1 }, end = { row = 8, column = 9 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                GrenSyntax.Node { start = { row = 7, column = 1 }, end = { row = 8, column = 9 } }
                                                    { name = GrenSyntax.Node { start = { row = 7, column = 1 }, end = { row = 7, column = 5 } } "fun2"
                                                    , arguments = [ GrenSyntax.Node { start = { row = 7, column = 6 }, end = { row = 7, column = 7 } } (GrenSyntax.PatternVariable "n") ]
                                                    , expression =
                                                        GrenSyntax.Node { start = { row = 8, column = 3 }, end = { row = 8, column = 9 } }
                                                            (GrenSyntax.Application
                                                                [ GrenSyntax.Node { start = { row = 8, column = 3 }, end = { row = 8, column = 7 } } (GrenSyntax.FunctionOrValue [] "fun1")
                                                                , GrenSyntax.Node { start = { row = 8, column = 8 }, end = { row = 8, column = 9 } } (GrenSyntax.FunctionOrValue [] "n")
                                                                ]
                                                            )
                                                    }
                                            }
                                        )
                                    ]
                                , comments = [ GrenSyntax.Node { start = { row = 5, column = 13 }, end = { row = 5, column = 17 } } "-- a", GrenSyntax.Node { start = { row = 8, column = 13 }, end = { row = 8, column = 17 } } "-- b" ]
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
                                { moduleDefinition = GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 31 } } (GrenSyntax.NormalModule { moduleName = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } } [ "A" ], exposingList = GrenSyntax.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 31 } } (GrenSyntax.Explicit [ GrenSyntax.Node { start = { row = 1, column = 20 }, end = { row = 1, column = 24 } } (GrenSyntax.FunctionExpose "fun1"), GrenSyntax.Node { start = { row = 1, column = 26 }, end = { row = 1, column = 30 } } (GrenSyntax.FunctionExpose "fun2") ]) })
                                , imports =
                                    [ GrenSyntax.Node { end = { column = 1, row = 3 }, start = { column = 1, row = 3 } }
                                        { exposingList = Nothing, moduleAlias = Nothing, moduleName = GrenSyntax.Node { end = { column = 9, row = 5 }, start = { column = 8, row = 5 } } [ "B" ] }
                                    ]
                                , declarations =
                                    [ GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 4, column = 9 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 4, column = 9 } }
                                                    { name = GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } } "fun1"
                                                    , arguments = [ GrenSyntax.Node { start = { row = 3, column = 6 }, end = { row = 3, column = 7 } } (GrenSyntax.PatternVariable "n") ]
                                                    , expression =
                                                        GrenSyntax.Node { start = { row = 4, column = 3 }, end = { row = 4, column = 9 } }
                                                            (GrenSyntax.Application
                                                                [ GrenSyntax.Node { start = { row = 4, column = 3 }, end = { row = 4, column = 7 } } (GrenSyntax.FunctionOrValue [] "fun2")
                                                                , GrenSyntax.Node { start = { row = 4, column = 8 }, end = { row = 4, column = 9 } } (GrenSyntax.FunctionOrValue [] "n")
                                                                ]
                                                            )
                                                    }
                                            }
                                        )
                                    , GrenSyntax.Node { start = { row = 7, column = 1 }, end = { row = 8, column = 9 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                GrenSyntax.Node { start = { row = 7, column = 1 }, end = { row = 8, column = 9 } }
                                                    { name = GrenSyntax.Node { start = { row = 7, column = 1 }, end = { row = 7, column = 5 } } "fun2"
                                                    , arguments = [ GrenSyntax.Node { start = { row = 7, column = 6 }, end = { row = 7, column = 7 } } (GrenSyntax.PatternVariable "n") ]
                                                    , expression =
                                                        GrenSyntax.Node { start = { row = 8, column = 3 }, end = { row = 8, column = 9 } }
                                                            (GrenSyntax.Application
                                                                [ GrenSyntax.Node { start = { row = 8, column = 3 }, end = { row = 8, column = 7 } } (GrenSyntax.FunctionOrValue [] "fun1")
                                                                , GrenSyntax.Node { start = { row = 8, column = 8 }, end = { row = 8, column = 9 } } (GrenSyntax.FunctionOrValue [] "n")
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
                                { moduleDefinition = GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 31 } } (GrenSyntax.NormalModule { moduleName = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } } [ "A" ], exposingList = GrenSyntax.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 31 } } (GrenSyntax.Explicit [ GrenSyntax.Node { start = { row = 1, column = 20 }, end = { row = 1, column = 24 } } (GrenSyntax.FunctionExpose "fun1"), GrenSyntax.Node { start = { row = 1, column = 26 }, end = { row = 1, column = 30 } } (GrenSyntax.FunctionExpose "fun2") ]) })
                                , imports =
                                    [ GrenSyntax.Node { end = { column = 1, row = 2 }, start = { column = 1, row = 2 } }
                                        { exposingList = Nothing, moduleAlias = Nothing, moduleName = GrenSyntax.Node { end = { column = 9, row = 5 }, start = { column = 8, row = 5 } } [ "B" ] }
                                    , GrenSyntax.Node { end = { column = 9, row = 2 }, start = { column = 1, row = 2 } }
                                        { exposingList = Nothing, moduleAlias = Nothing, moduleName = GrenSyntax.Node { end = { column = 9, row = 2 }, start = { column = 8, row = 2 } } [ "C" ] }
                                    ]
                                , declarations =
                                    [ GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 4, column = 9 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 4, column = 9 } }
                                                    { name = GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } } "fun1"
                                                    , arguments = [ GrenSyntax.Node { start = { row = 3, column = 6 }, end = { row = 3, column = 7 } } (GrenSyntax.PatternVariable "n") ]
                                                    , expression =
                                                        GrenSyntax.Node { start = { row = 4, column = 3 }, end = { row = 4, column = 9 } }
                                                            (GrenSyntax.Application
                                                                [ GrenSyntax.Node { start = { row = 4, column = 3 }, end = { row = 4, column = 7 } } (GrenSyntax.FunctionOrValue [] "fun2")
                                                                , GrenSyntax.Node { start = { row = 4, column = 8 }, end = { row = 4, column = 9 } } (GrenSyntax.FunctionOrValue [] "n")
                                                                ]
                                                            )
                                                    }
                                            }
                                        )
                                    , GrenSyntax.Node { start = { row = 7, column = 1 }, end = { row = 8, column = 9 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                GrenSyntax.Node { start = { row = 7, column = 1 }, end = { row = 8, column = 9 } }
                                                    { name = GrenSyntax.Node { start = { row = 7, column = 1 }, end = { row = 7, column = 5 } } "fun2"
                                                    , arguments = [ GrenSyntax.Node { start = { row = 7, column = 6 }, end = { row = 7, column = 7 } } (GrenSyntax.PatternVariable "n") ]
                                                    , expression =
                                                        GrenSyntax.Node { start = { row = 8, column = 3 }, end = { row = 8, column = 9 } }
                                                            (GrenSyntax.Application
                                                                [ GrenSyntax.Node { start = { row = 8, column = 3 }, end = { row = 8, column = 7 } } (GrenSyntax.FunctionOrValue [] "fun1")
                                                                , GrenSyntax.Node { start = { row = 8, column = 8 }, end = { row = 8, column = 9 } } (GrenSyntax.FunctionOrValue [] "n")
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 37 } }
                                { moduleName = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Foo" ]
                                , moduleAlias = Nothing
                                , exposingList =
                                    Just
                                        (GrenSyntax.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 37 } }
                                            (GrenSyntax.Explicit
                                                [ GrenSyntax.Node { start = { row = 1, column = 22 }, end = { row = 1, column = 27 } } (GrenSyntax.TypeOrAliasExpose "Model"), GrenSyntax.Node { start = { row = 1, column = 29 }, end = { row = 1, column = 36 } } (GrenSyntax.TypeExpose { name = "Msg", open = Just { start = { row = 1, column = 32 }, end = { row = 1, column = 36 } } }) ]
                                            )
                                        )
                                }
                            )
                )
            , Test.test "import with explicits 2"
                (\() ->
                    "import Html exposing (text)"
                        |> expectSyntaxWithoutComments GrenParserLenient.import_
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 28 } }
                                { moduleName = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 12 } } [ "Html" ]
                                , moduleAlias = Nothing
                                , exposingList =
                                    Just
                                        (GrenSyntax.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 28 } }
                                            (GrenSyntax.Explicit
                                                [ GrenSyntax.Node { start = { row = 1, column = 23 }, end = { row = 1, column = 27 } } (GrenSyntax.FunctionExpose "text") ]
                                            )
                                        )
                                }
                            )
                )
            , Test.test "import with exposing ()"
                (\() ->
                    "import Html exposing ()"
                        |> expectSyntaxWithoutComments GrenParserLenient.import_
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                { moduleName = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 12 } } [ "Html" ]
                                , moduleAlias = Nothing
                                , exposingList = Nothing
                                }
                            )
                )
            , Test.test "import with alias and exposing ()"
                (\() ->
                    "import Html as H exposing ()"
                        |> expectSyntaxWithoutComments GrenParserLenient.import_
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 17 } }
                                { moduleName = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 12 } } [ "Html" ]
                                , moduleAlias = Just (GrenSyntax.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 17 } } [ "H" ])
                                , exposingList = Nothing
                                }
                            )
                )
            , Test.test "import minimal"
                (\() ->
                    "import Foo"
                        |> expectSyntaxWithoutComments GrenParserLenient.import_
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                                { moduleName = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Foo" ]
                                , moduleAlias = Nothing
                                , exposingList = Nothing
                                }
                            )
                )
            , Test.test "import with alias"
                (\() ->
                    "import Foo as Bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.import_
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 18 } }
                                { moduleName = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Foo" ]
                                , moduleAlias = Just (GrenSyntax.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 18 } } [ "Bar" ])
                                , exposingList = Nothing
                                }
                            )
                )
            , Test.test "import with alias and exposing .."
                (\() ->
                    "import Foo as Bar exposing (..)"
                        |> expectSyntaxWithoutComments GrenParserLenient.import_
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                                { moduleName = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Foo" ]
                                , moduleAlias = Just (GrenSyntax.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 18 } } [ "Bar" ])
                                , exposingList =
                                    Just
                                        (GrenSyntax.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 32 } }
                                            (GrenSyntax.All { start = { row = 1, column = 29 }, end = { row = 1, column = 31 } })
                                        )
                                }
                            )
                )
            , Test.test "import with alias and exposing ..."
                (\() ->
                    "import Foo as Bar exposing (...)"
                        |> expectSyntaxWithoutComments GrenParserLenient.import_
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 33 } }
                                { moduleName = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Foo" ]
                                , moduleAlias = Just (GrenSyntax.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 18 } } [ "Bar" ])
                                , exposingList =
                                    Just
                                        (GrenSyntax.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 33 } }
                                            (GrenSyntax.All { start = { row = 1, column = 29 }, end = { row = 1, column = 32 } })
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
                            [ GrenSyntax.Node { start = { row = 4, column = 1 }, end = { row = 5, column = 3 } } "{-| Module documentation\n-}"
                            , GrenSyntax.Node { start = { row = 9, column = 1 }, end = { row = 9, column = 5 } } "-- 1"
                            , GrenSyntax.Node { start = { row = 10, column = 1 }, end = { row = 10, column = 8 } } "{- 2 -}"
                            , GrenSyntax.Node { start = { row = 11, column = 1 }, end = { row = 11, column = 5 } } "-- 3"
                            , GrenSyntax.Node { start = { row = 16, column = 5 }, end = { row = 16, column = 9 } } "-- 4"
                            , GrenSyntax.Node { start = { row = 19, column = 1 }, end = { row = 19, column = 5 } } "-- 5"
                            , GrenSyntax.Node { start = { row = 20, column = 1 }, end = { row = 20, column = 8 } } "{- 6 -}"
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
                            [ GrenSyntax.Node { start = { row = 7, column = 17 }, end = { row = 7, column = 21 } } "-- 1"
                            , GrenSyntax.Node { start = { row = 13, column = 1 }, end = { row = 13, column = 5 } } "-- 2"
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
                                GrenSyntax.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 41 } }
                                    (GrenSyntax.NormalModule
                                        { moduleName = GrenSyntax.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 27 } } [ "Trailing", "Whitespace" ]
                                        , exposingList =
                                            GrenSyntax.Node { start = { row = 2, column = 28 }, end = { row = 2, column = 41 } }
                                                (GrenSyntax.All { start = { row = 2, column = 38 }, end = { row = 2, column = 40 } })
                                        }
                                    )
                            , imports = []
                            , declarations =
                                [ GrenSyntax.Node { start = { row = 4, column = 1 }, end = { row = 11, column = 11 } }
                                    (GrenSyntax.FunctionDeclaration
                                        { documentation = Nothing
                                        , signature = Nothing
                                        , declaration =
                                            GrenSyntax.Node { start = { row = 4, column = 1 }, end = { row = 11, column = 11 } }
                                                { name = GrenSyntax.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 15 } } "caseWhitespace"
                                                , arguments = [ GrenSyntax.Node { start = { row = 4, column = 16 }, end = { row = 4, column = 17 } } (GrenSyntax.PatternVariable "f") ]
                                                , expression =
                                                    GrenSyntax.Node { start = { row = 4, column = 20 }, end = { row = 11, column = 11 } }
                                                        (GrenSyntax.CaseExpression
                                                            { expression = GrenSyntax.Node { start = { row = 4, column = 25 }, end = { row = 4, column = 26 } } (GrenSyntax.FunctionOrValue [] "f")
                                                            , cases =
                                                                [ ( GrenSyntax.Node { start = { row = 5, column = 3 }, end = { row = 5, column = 7 } } (GrenSyntax.PatternVariant { moduleName = [], name = "True" } [])
                                                                  , GrenSyntax.Node { start = { row = 6, column = 5 }, end = { row = 6, column = 6 } } (GrenSyntax.Integer 1)
                                                                  )
                                                                , ( GrenSyntax.Node { start = { row = 7, column = 3 }, end = { row = 7, column = 8 } } (GrenSyntax.PatternVariant { moduleName = [], name = "False" } [])
                                                                  , GrenSyntax.Node { start = { row = 11, column = 10 }, end = { row = 11, column = 11 } } (GrenSyntax.Integer 2)
                                                                  )
                                                                ]
                                                            }
                                                        )
                                                }
                                        }
                                    )
                                ]
                            , comments = [ GrenSyntax.Node { start = { row = 13, column = 4 }, end = { row = 13, column = 18 } } "--some comment" ]
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
                                GrenSyntax.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 41 } }
                                    (GrenSyntax.NormalModule
                                        { moduleName = GrenSyntax.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 27 } } [ "Trailing", "Whitespace" ]
                                        , exposingList =
                                            GrenSyntax.Node { start = { row = 2, column = 28 }, end = { row = 2, column = 41 } }
                                                (GrenSyntax.All { start = { row = 2, column = 38 }, end = { row = 2, column = 40 } })
                                        }
                                    )
                            , imports = []
                            , declarations =
                                [ GrenSyntax.Node { start = { row = 4, column = 1 }, end = { row = 8, column = 6 } }
                                    (GrenSyntax.FunctionDeclaration
                                        { documentation = Nothing
                                        , signature = Nothing
                                        , declaration =
                                            GrenSyntax.Node { start = { row = 4, column = 1 }, end = { row = 8, column = 6 } }
                                                { name = GrenSyntax.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 17 } } "lambdaWhitespace"
                                                , arguments = []
                                                , expression =
                                                    GrenSyntax.Node { start = { row = 4, column = 22 }, end = { row = 8, column = 6 } }
                                                        (GrenSyntax.LambdaExpression
                                                            { args =
                                                                [ GrenSyntax.Node { start = { row = 4, column = 24 }, end = { row = 4, column = 25 } } (GrenSyntax.PatternVariable "a")
                                                                , GrenSyntax.Node { start = { row = 4, column = 26 }, end = { row = 4, column = 27 } } (GrenSyntax.PatternVariable "b")
                                                                ]
                                                            , expression =
                                                                GrenSyntax.Node { start = { row = 4, column = 34 }, end = { row = 8, column = 6 } }
                                                                    (GrenSyntax.OperatorApplication "+"
                                                                        (GrenSyntax.Node { start = { row = 4, column = 34 }, end = { row = 4, column = 35 } } (GrenSyntax.FunctionOrValue [] "a"))
                                                                        (GrenSyntax.Node { start = { row = 8, column = 5 }, end = { row = 8, column = 6 } } (GrenSyntax.FunctionOrValue [] "b"))
                                                                    )
                                                            }
                                                        )
                                                }
                                        }
                                    )
                                ]
                            , comments = [ GrenSyntax.Node { start = { row = 11, column = 1 }, end = { row = 11, column = 15 } } "--some comment" ]
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
                                GrenSyntax.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 41 } }
                                    (GrenSyntax.NormalModule
                                        { moduleName = GrenSyntax.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 27 } } [ "Trailing", "Whitespace" ]
                                        , exposingList =
                                            GrenSyntax.Node { start = { row = 2, column = 28 }, end = { row = 2, column = 41 } }
                                                (GrenSyntax.All { start = { row = 2, column = 38 }, end = { row = 2, column = 40 } })
                                        }
                                    )
                            , imports = []
                            , declarations =
                                [ GrenSyntax.Node { start = { row = 4, column = 1 }, end = { row = 8, column = 3 } }
                                    (GrenSyntax.FunctionDeclaration
                                        { documentation = Nothing
                                        , signature = Nothing
                                        , declaration =
                                            GrenSyntax.Node { start = { row = 4, column = 1 }, end = { row = 8, column = 3 } }
                                                { name = GrenSyntax.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 14 } } "letWhitespace"
                                                , arguments = []
                                                , expression =
                                                    GrenSyntax.Node { start = { row = 4, column = 17 }, end = { row = 8, column = 3 } }
                                                        (GrenSyntax.LetExpression
                                                            { declarations =
                                                                [ GrenSyntax.Node { start = { row = 5, column = 19 }, end = { row = 5, column = 26 } }
                                                                    (GrenSyntax.LetFunction
                                                                        { documentation = Nothing
                                                                        , signature = Nothing
                                                                        , declaration =
                                                                            GrenSyntax.Node { start = { row = 5, column = 19 }, end = { row = 5, column = 26 } }
                                                                                { name = GrenSyntax.Node { start = { row = 5, column = 19 }, end = { row = 5, column = 20 } } "b"
                                                                                , arguments = []
                                                                                , expression = GrenSyntax.Node { start = { row = 5, column = 25 }, end = { row = 5, column = 26 } } (GrenSyntax.Integer 1)
                                                                                }
                                                                        }
                                                                    )
                                                                ]
                                                            , expression = GrenSyntax.Node { start = { row = 8, column = 2 }, end = { row = 8, column = 3 } } (GrenSyntax.FunctionOrValue [] "b")
                                                            }
                                                        )
                                                }
                                        }
                                    )
                                ]
                            , comments = [ GrenSyntax.Node { start = { row = 11, column = 1 }, end = { row = 11, column = 15 } } "--some comment" ]
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
                                GrenSyntax.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 25 } }
                                    (GrenSyntax.NormalModule
                                        { moduleName = GrenSyntax.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } [ "Foo" ]
                                        , exposingList =
                                            GrenSyntax.Node { start = { row = 2, column = 12 }, end = { row = 2, column = 25 } }
                                                (GrenSyntax.All { start = { row = 2, column = 22 }, end = { row = 2, column = 24 } })
                                        }
                                    )
                            , imports =
                                [ GrenSyntax.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 12 } }
                                    { moduleName = GrenSyntax.Node { start = { row = 4, column = 8 }, end = { row = 4, column = 12 } } [ "Dict" ]
                                    , moduleAlias = Nothing
                                    , exposingList = Nothing
                                    }
                                ]
                            , declarations =
                                [ GrenSyntax.Node { start = { row = 6, column = 1 }, end = { row = 9, column = 20 } }
                                    (GrenSyntax.CustomTypeDeclaration
                                        { documentation = Just (GrenSyntax.Node { start = { row = 6, column = 1 }, end = { row = 7, column = 3 } } "{-| Config goes here\n-}")
                                        , name = GrenSyntax.Node { start = { row = 8, column = 6 }, end = { row = 8, column = 19 } } "Configuration"
                                        , generics = []
                                        , constructors =
                                            [ GrenSyntax.Node { start = { row = 9, column = 7 }, end = { row = 9, column = 20 } }
                                                { name = GrenSyntax.Node { start = { row = 9, column = 7 }, end = { row = 9, column = 20 } } "Configuration"
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
                                GrenSyntax.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 25 } }
                                    (GrenSyntax.NormalModule
                                        { moduleName = GrenSyntax.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } [ "Foo" ]
                                        , exposingList =
                                            GrenSyntax.Node { start = { row = 2, column = 12 }, end = { row = 2, column = 25 } }
                                                (GrenSyntax.All { start = { row = 2, column = 22 }, end = { row = 2, column = 24 } })
                                        }
                                    )
                            , imports = []
                            , declarations =
                                [ GrenSyntax.Node { start = { row = 6, column = 1 }, end = { row = 7, column = 20 } }
                                    (GrenSyntax.CustomTypeDeclaration
                                        { documentation = Nothing
                                        , name = GrenSyntax.Node { start = { row = 6, column = 6 }, end = { row = 6, column = 19 } } "Configuration"
                                        , generics = []
                                        , constructors =
                                            [ GrenSyntax.Node { start = { row = 7, column = 7 }, end = { row = 7, column = 20 } }
                                                { name = GrenSyntax.Node { start = { row = 7, column = 7 }, end = { row = 7, column = 20 } } "Configuration"
                                                , arguments = []
                                                }
                                            ]
                                        }
                                    )
                                ]
                            , comments = [ GrenSyntax.Node { start = { row = 4, column = 1 }, end = { row = 5, column = 3 } } "{-| actually module doc\n-}" ]
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
                                GrenSyntax.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 30 } }
                                    (GrenSyntax.PortModule
                                        { moduleName = GrenSyntax.Node { start = { row = 2, column = 13 }, end = { row = 2, column = 16 } } [ "Foo" ]
                                        , exposingList =
                                            GrenSyntax.Node { start = { row = 2, column = 17 }, end = { row = 2, column = 30 } }
                                                (GrenSyntax.All { start = { row = 2, column = 27 }, end = { row = 2, column = 29 } })
                                        }
                                    )
                            , imports =
                                [ GrenSyntax.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 14 } }
                                    { moduleName = GrenSyntax.Node { start = { row = 4, column = 8 }, end = { row = 4, column = 14 } } [ "String" ]
                                    , moduleAlias = Nothing
                                    , exposingList = Nothing
                                    }
                                ]
                            , declarations =
                                [ GrenSyntax.Node { start = { row = 8, column = 1 }, end = { row = 8, column = 38 } }
                                    (GrenSyntax.PortDeclaration
                                        { name = GrenSyntax.Node { start = { row = 8, column = 6 }, end = { row = 8, column = 18 } } "sendResponse"
                                        , typeAnnotation =
                                            GrenSyntax.Node { start = { row = 8, column = 21 }, end = { row = 8, column = 38 } }
                                                (GrenSyntax.TypeAnnotationFunction
                                                    (GrenSyntax.Node { start = { row = 8, column = 21 }, end = { row = 8, column = 27 } }
                                                        (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 8, column = 21 }, end = { row = 8, column = 27 } } ( [], "String" )) [])
                                                    )
                                                    (GrenSyntax.Node { start = { row = 8, column = 31 }, end = { row = 8, column = 38 } }
                                                        (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 8, column = 31 }, end = { row = 8, column = 34 } } ( [], "Cmd" ))
                                                            [ GrenSyntax.Node { start = { row = 8, column = 35 }, end = { row = 8, column = 38 } } (GrenSyntax.TypeAnnotationVariable "msg") ]
                                                        )
                                                    )
                                                )
                                        }
                                    )
                                ]
                            , comments = [ GrenSyntax.Node { start = { row = 6, column = 1 }, end = { row = 7, column = 3 } } "{-| foo\n-}" ]
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
                                [ GrenSyntax.Node { end = { column = 13, row = 5 }, start = { column = 1, row = 4 } }
                                    (GrenSyntax.FunctionDeclaration
                                        { declaration =
                                            GrenSyntax.Node { end = { column = 13, row = 5 }, start = { column = 1, row = 4 } }
                                                { arguments = []
                                                , expression =
                                                    GrenSyntax.Node { end = { column = 13, row = 5 }, start = { column = 5, row = 5 } }
                                                        (GrenSyntax.FunctionOrValue [ "Cmd" ] "none")
                                                , name = GrenSyntax.Node { end = { column = 13, row = 4 }, start = { column = 1, row = 4 } } "sendResponse"
                                                }
                                        , documentation = Nothing
                                        , signature = Nothing
                                        }
                                    )
                                ]
                            , imports = []
                            , moduleDefinition =
                                GrenSyntax.Node { end = { column = 30, row = 2 }, start = { column = 1, row = 2 } }
                                    (GrenSyntax.NormalModule
                                        { exposingList =
                                            GrenSyntax.Node { end = { column = 30, row = 2 }, start = { column = 17, row = 2 } }
                                                (GrenSyntax.All { end = { column = 29, row = 2 }, start = { column = 27, row = 2 } })
                                        , moduleName = GrenSyntax.Node { end = { column = 16, row = 2 }, start = { column = 13, row = 2 } } [ "Foo" ]
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
                                [ GrenSyntax.Node { end = { column = 38, row = 4 }, start = { column = 1, row = 4 } }
                                    (GrenSyntax.PortDeclaration
                                        { name = GrenSyntax.Node { end = { column = 18, row = 4 }, start = { column = 6, row = 4 } } "sendResponse"
                                        , typeAnnotation =
                                            GrenSyntax.Node { end = { column = 38, row = 4 }, start = { column = 21, row = 4 } }
                                                (GrenSyntax.TypeAnnotationFunction
                                                    (GrenSyntax.Node { end = { column = 27, row = 4 }, start = { column = 21, row = 4 } }
                                                        (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { end = { column = 27, row = 4 }, start = { column = 21, row = 4 } } ( [], "String" )) [])
                                                    )
                                                    (GrenSyntax.Node { end = { column = 38, row = 4 }, start = { column = 31, row = 4 } }
                                                        (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { end = { column = 34, row = 4 }, start = { column = 31, row = 4 } } ( [], "Cmd" ))
                                                            [ GrenSyntax.Node { end = { column = 38, row = 4 }, start = { column = 35, row = 4 } }
                                                                (GrenSyntax.TypeAnnotationVariable "msg")
                                                            ]
                                                        )
                                                    )
                                                )
                                        }
                                    )
                                ]
                            , imports = []
                            , moduleDefinition =
                                GrenSyntax.Node { end = { column = 25, row = 2 }, start = { column = 1, row = 2 } }
                                    (GrenSyntax.PortModule
                                        { exposingList =
                                            GrenSyntax.Node { end = { column = 25, row = 2 }, start = { column = 12, row = 2 } }
                                                (GrenSyntax.All { end = { column = 24, row = 2 }, start = { column = 22, row = 2 } })
                                        , moduleName = GrenSyntax.Node { end = { column = 11, row = 2 }, start = { column = 8, row = 2 } } [ "Foo" ]
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                                (GrenSyntax.FunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                                            { name = GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "foo"
                                            , arguments = []
                                            , expression = GrenSyntax.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 10 } } (GrenSyntax.FunctionOrValue [] "bar")
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 10 } }
                                (GrenSyntax.FunctionDeclaration
                                    { documentation = Just (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 20 } } "{-| Foo does bar -}")
                                    , signature = Nothing
                                    , declaration =
                                        GrenSyntax.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 10 } }
                                            { name = GrenSyntax.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 4 } } "foo"
                                            , arguments = []
                                            , expression = GrenSyntax.Node { start = { row = 2, column = 7 }, end = { row = 2, column = 10 } } (GrenSyntax.FunctionOrValue [] "bar")
                                            }
                                    }
                                )
                            )
                )
            , Test.test "function declaration with empty record"
                (\() ->
                    "foo = {}"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 9 } }
                                (GrenSyntax.FunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 9 } }
                                            { name = GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "foo"
                                            , arguments = []
                                            , expression = GrenSyntax.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 9 } } (GrenSyntax.RecordExpr [])
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 7, column = 7 } }
                                (GrenSyntax.FunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 7, column = 7 } }
                                            { name = GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "inc"
                                            , arguments = [ GrenSyntax.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } (GrenSyntax.PatternVariable "x") ]
                                            , expression =
                                                GrenSyntax.Node { start = { row = 2, column = 3 }, end = { row = 7, column = 7 } }
                                                    (GrenSyntax.LetExpression
                                                        { declarations =
                                                            [ GrenSyntax.Node { start = { row = 3, column = 5 }, end = { row = 5, column = 18 } }
                                                                (GrenSyntax.LetFunction
                                                                    { documentation = Nothing
                                                                    , signature = Nothing
                                                                    , declaration =
                                                                        GrenSyntax.Node { start = { row = 3, column = 5 }, end = { row = 5, column = 18 } }
                                                                            { name = GrenSyntax.Node { start = { row = 3, column = 5 }, end = { row = 3, column = 6 } } "y"
                                                                            , arguments = []
                                                                            , expression =
                                                                                GrenSyntax.Node { start = { row = 4, column = 7 }, end = { row = 5, column = 18 } }
                                                                                    (GrenSyntax.CaseExpression
                                                                                        { expression = GrenSyntax.Node { start = { row = 4, column = 12 }, end = { row = 4, column = 13 } } (GrenSyntax.FunctionOrValue [] "x")
                                                                                        , cases =
                                                                                            [ ( GrenSyntax.Node { start = { row = 5, column = 9 }, end = { row = 5, column = 13 } } (GrenSyntax.PatternVariant { moduleName = [], name = "True" } [])
                                                                                              , GrenSyntax.Node { start = { row = 5, column = 17 }, end = { row = 5, column = 18 } } (GrenSyntax.FunctionOrValue [] "z")
                                                                                              )
                                                                                            ]
                                                                                        }
                                                                                    )
                                                                            }
                                                                    }
                                                                )
                                                            , GrenSyntax.Node { start = { row = 6, column = 5 }, end = { row = 6, column = 10 } }
                                                                (GrenSyntax.LetFunction
                                                                    { documentation = Nothing
                                                                    , signature = Nothing
                                                                    , declaration =
                                                                        GrenSyntax.Node { start = { row = 6, column = 5 }, end = { row = 6, column = 10 } }
                                                                            { name = GrenSyntax.Node { start = { row = 6, column = 5 }, end = { row = 6, column = 6 } } "a"
                                                                            , arguments = []
                                                                            , expression = GrenSyntax.Node { start = { row = 6, column = 9 }, end = { row = 6, column = 10 } } (GrenSyntax.FunctionOrValue [] "b")
                                                                            }
                                                                    }
                                                                )
                                                            ]
                                                        , expression = GrenSyntax.Node { start = { row = 7, column = 6 }, end = { row = 7, column = 7 } } (GrenSyntax.FunctionOrValue [] "a")
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 14 } }
                                (GrenSyntax.FunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 14 } }
                                            { name = GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "inc"
                                            , arguments = [ GrenSyntax.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } (GrenSyntax.PatternVariable "x") ]
                                            , expression =
                                                GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 14 } }
                                                    (GrenSyntax.OperatorApplication "+"
                                                        (GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (GrenSyntax.FunctionOrValue [] "x"))
                                                        (GrenSyntax.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 14 } } (GrenSyntax.Integer 1))
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 5, column = 4 } }
                                (GrenSyntax.FunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 5, column = 4 } }
                                            { name = GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "foo"
                                            , arguments = []
                                            , expression =
                                                GrenSyntax.Node { start = { row = 2, column = 2 }, end = { row = 5, column = 4 } }
                                                    (GrenSyntax.LetExpression
                                                        { declarations =
                                                            [ GrenSyntax.Node { start = { row = 3, column = 3 }, end = { row = 3, column = 8 } }
                                                                (GrenSyntax.LetFunction
                                                                    { documentation = Nothing
                                                                    , signature = Nothing
                                                                    , declaration =
                                                                        GrenSyntax.Node { start = { row = 3, column = 3 }, end = { row = 3, column = 8 } }
                                                                            { name = GrenSyntax.Node { start = { row = 3, column = 3 }, end = { row = 3, column = 4 } } "b"
                                                                            , arguments = []
                                                                            , expression = GrenSyntax.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 8 } } (GrenSyntax.Integer 1)
                                                                            }
                                                                    }
                                                                )
                                                            ]
                                                        , expression = GrenSyntax.Node { start = { row = 5, column = 3 }, end = { row = 5, column = 4 } } (GrenSyntax.FunctionOrValue [] "b")
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 5, column = 4 } }
                                (GrenSyntax.FunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 5, column = 4 } }
                                            { name = GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "foo"
                                            , arguments = []
                                            , expression =
                                                GrenSyntax.Node { start = { row = 2, column = 2 }, end = { row = 5, column = 4 } }
                                                    (GrenSyntax.LetExpression
                                                        { declarations =
                                                            [ GrenSyntax.Node { start = { row = 3, column = 3 }, end = { row = 3, column = 16 } }
                                                                (GrenSyntax.LetDestructuring
                                                                    (GrenSyntax.Node { start = { row = 3, column = 3 }, end = { row = 3, column = 9 } }
                                                                        (GrenSyntax.PatternTuple
                                                                            [ GrenSyntax.Node { start = { row = 3, column = 4 }, end = { row = 3, column = 5 } } (GrenSyntax.PatternVariable "b")
                                                                            , GrenSyntax.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 8 } } (GrenSyntax.PatternVariable "c")
                                                                            ]
                                                                        )
                                                                    )
                                                                    (GrenSyntax.Node { start = { row = 3, column = 10 }, end = { row = 3, column = 16 } }
                                                                        (GrenSyntax.TupledExpression
                                                                            [ GrenSyntax.Node { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } } (GrenSyntax.Integer 1)
                                                                            , GrenSyntax.Node { start = { row = 3, column = 14 }, end = { row = 3, column = 15 } } (GrenSyntax.Integer 2)
                                                                            ]
                                                                        )
                                                                    )
                                                                )
                                                            ]
                                                        , expression = GrenSyntax.Node { start = { row = 5, column = 3 }, end = { row = 5, column = 4 } } (GrenSyntax.FunctionOrValue [] "b")
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 62 } }
                                (GrenSyntax.FunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 62 } }
                                            { name = GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } "main"
                                            , arguments = []
                                            , expression =
                                                GrenSyntax.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 62 } }
                                                    (GrenSyntax.Application
                                                        [ GrenSyntax.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 18 } } (GrenSyntax.FunctionOrValue [] "beginnerProgram")
                                                        , GrenSyntax.Node { start = { row = 2, column = 19 }, end = { row = 2, column = 62 } }
                                                            (GrenSyntax.RecordExpr
                                                                [ GrenSyntax.Node { start = { row = 2, column = 21 }, end = { row = 2, column = 30 } }
                                                                    ( GrenSyntax.Node { start = { row = 2, column = 21 }, end = { row = 2, column = 26 } } "model"
                                                                    , GrenSyntax.Node { start = { row = 2, column = 29 }, end = { row = 2, column = 30 } } (GrenSyntax.Integer 0)
                                                                    )
                                                                , GrenSyntax.Node { start = { row = 2, column = 32 }, end = { row = 2, column = 43 } }
                                                                    ( GrenSyntax.Node { start = { row = 2, column = 32 }, end = { row = 2, column = 36 } } "view"
                                                                    , GrenSyntax.Node { start = { row = 2, column = 39 }, end = { row = 2, column = 43 } } (GrenSyntax.FunctionOrValue [] "view")
                                                                    )
                                                                , GrenSyntax.Node { start = { row = 2, column = 45 }, end = { row = 2, column = 61 } }
                                                                    ( GrenSyntax.Node { start = { row = 2, column = 45 }, end = { row = 2, column = 51 } } "update"
                                                                    , GrenSyntax.Node { start = { row = 2, column = 54 }, end = { row = 2, column = 60 } } (GrenSyntax.FunctionOrValue [] "update")
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 7, column = 16 } }
                                (GrenSyntax.FunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 7, column = 16 } }
                                            { name = GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } } "update"
                                            , arguments =
                                                [ GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } (GrenSyntax.PatternVariable "msg")
                                                , GrenSyntax.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 17 } } (GrenSyntax.PatternVariable "model")
                                                ]
                                            , expression =
                                                GrenSyntax.Node { start = { row = 2, column = 3 }, end = { row = 7, column = 16 } }
                                                    (GrenSyntax.CaseExpression
                                                        { expression = GrenSyntax.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } (GrenSyntax.FunctionOrValue [] "msg")
                                                        , cases =
                                                            [ ( GrenSyntax.Node { start = { row = 3, column = 5 }, end = { row = 3, column = 14 } } (GrenSyntax.PatternVariant { moduleName = [], name = "Increment" } [])
                                                              , GrenSyntax.Node { start = { row = 4, column = 7 }, end = { row = 4, column = 16 } }
                                                                    (GrenSyntax.OperatorApplication "+"
                                                                        (GrenSyntax.Node { start = { row = 4, column = 7 }, end = { row = 4, column = 12 } } (GrenSyntax.FunctionOrValue [] "model"))
                                                                        (GrenSyntax.Node { start = { row = 4, column = 15 }, end = { row = 4, column = 16 } } (GrenSyntax.Integer 1))
                                                                    )
                                                              )
                                                            , ( GrenSyntax.Node { start = { row = 6, column = 5 }, end = { row = 6, column = 14 } } (GrenSyntax.PatternVariant { moduleName = [], name = "Decrement" } [])
                                                              , GrenSyntax.Node { start = { row = 7, column = 7 }, end = { row = 7, column = 16 } }
                                                                    (GrenSyntax.OperatorApplication "-"
                                                                        (GrenSyntax.Node { start = { row = 7, column = 7 }, end = { row = 7, column = 12 } } (GrenSyntax.FunctionOrValue [] "model"))
                                                                        (GrenSyntax.Node { start = { row = 7, column = 15 }, end = { row = 7, column = 16 } } (GrenSyntax.Integer 1))
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
                    "port parseResponse : ( String         ) -> Cmd msg"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (GrenSyntax.Node { end = { column = 51, row = 1 }, start = { column = 1, row = 1 } }
                                (GrenSyntax.PortDeclaration
                                    { name = GrenSyntax.Node { end = { column = 19, row = 1 }, start = { column = 6, row = 1 } } "parseResponse"
                                    , typeAnnotation =
                                        GrenSyntax.Node { end = { column = 51, row = 1 }, start = { column = 22, row = 1 } }
                                            (GrenSyntax.TypeAnnotationFunction
                                                (GrenSyntax.Node { end = { column = 40, row = 1 }, start = { column = 22, row = 1 } }
                                                    (GrenSyntax.TypeAnnotationParenthesized
                                                        (GrenSyntax.Node { end = { column = 30, row = 1 }, start = { column = 24, row = 1 } }
                                                            (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { end = { column = 30, row = 1 }, start = { column = 24, row = 1 } } ( [], "String" )) [])
                                                        )
                                                    )
                                                )
                                                (GrenSyntax.Node { end = { column = 51, row = 1 }, start = { column = 44, row = 1 } }
                                                    (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { end = { column = 47, row = 1 }, start = { column = 44, row = 1 } } ( [], "Cmd" ))
                                                        [ GrenSyntax.Node { end = { column = 51, row = 1 }, start = { column = 48, row = 1 } }
                                                            (GrenSyntax.TypeAnnotationVariable "msg")
                                                        ]
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
                            (GrenSyntax.Node { end = { column = 39, row = 1 }, start = { column = 1, row = 1 } }
                                (GrenSyntax.PortDeclaration
                                    { name = GrenSyntax.Node { end = { column = 12, row = 1 }, start = { column = 6, row = 1 } } "scroll"
                                    , typeAnnotation =
                                        GrenSyntax.Node { end = { column = 39, row = 1 }, start = { column = 15, row = 1 } }
                                            (GrenSyntax.TypeAnnotationFunction (GrenSyntax.Node { end = { column = 28, row = 1 }, start = { column = 15, row = 1 } } (GrenSyntax.TypeAnnotationParenthesized (GrenSyntax.Node { end = { column = 27, row = 1 }, start = { column = 16, row = 1 } } (GrenSyntax.TypeAnnotationFunction (GrenSyntax.Node { end = { column = 20, row = 1 }, start = { column = 16, row = 1 } } (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { end = { column = 20, row = 1 }, start = { column = 16, row = 1 } } ( [], "Move" )) [])) (GrenSyntax.Node { end = { column = 27, row = 1 }, start = { column = 24, row = 1 } } (GrenSyntax.TypeAnnotationVariable "msg"))))))
                                                (GrenSyntax.Node { end = { column = 39, row = 1 }, start = { column = 32, row = 1 } }
                                                    (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { end = { column = 35, row = 1 }, start = { column = 32, row = 1 } } ( [], "Sub" ))
                                                        [ GrenSyntax.Node { end = { column = 39, row = 1 }, start = { column = 36, row = 1 } }
                                                            (GrenSyntax.TypeAnnotationVariable "msg")
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 23 } }
                                (GrenSyntax.FunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 23 } }
                                            { name = GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } "main"
                                            , arguments = []
                                            , expression =
                                                GrenSyntax.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 23 } }
                                                    (GrenSyntax.Application
                                                        [ GrenSyntax.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 7 } } (GrenSyntax.FunctionOrValue [] "text")
                                                        , GrenSyntax.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 23 } } (GrenSyntax.Literal "Hello, World!")
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
                            (GrenSyntax.Node { end = { column = 23, row = 3 }, start = { column = 1, row = 1 } }
                                (GrenSyntax.FunctionDeclaration
                                    { declaration =
                                        GrenSyntax.Node { end = { column = 23, row = 3 }, start = { column = 1, row = 2 } }
                                            { arguments = []
                                            , expression =
                                                GrenSyntax.Node { end = { column = 23, row = 3 }, start = { column = 3, row = 3 } }
                                                    (GrenSyntax.Application
                                                        [ GrenSyntax.Node { end = { column = 7, row = 3 }, start = { column = 3, row = 3 } }
                                                            (GrenSyntax.FunctionOrValue [] "text")
                                                        , GrenSyntax.Node { end = { column = 23, row = 3 }, start = { column = 8, row = 3 } }
                                                            (GrenSyntax.Literal "Hello, World!")
                                                        ]
                                                    )
                                            , name = GrenSyntax.Node { end = { column = 5, row = 2 }, start = { column = 1, row = 2 } } "main"
                                            }
                                    , documentation = Nothing
                                    , signature =
                                        Just
                                            (GrenSyntax.Node { end = { column = 16, row = 1 }, start = { column = 1, row = 1 } }
                                                { name = GrenSyntax.Node { end = { column = 5, row = 1 }, start = { column = 1, row = 1 } } "main"
                                                , typeAnnotation =
                                                    GrenSyntax.Node { end = { column = 16, row = 1 }, start = { column = 8, row = 1 } }
                                                        (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { end = { column = 12, row = 1 }, start = { column = 8, row = 1 } } ( [], "Html" ))
                                                            [ GrenSyntax.Node { end = { column = 16, row = 1 }, start = { column = 13, row = 1 } }
                                                                (GrenSyntax.TypeAnnotationVariable "msg")
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
                            (GrenSyntax.Node { end = { column = 23, row = 3 }, start = { column = 1, row = 1 } }
                                (GrenSyntax.FunctionDeclaration
                                    { declaration =
                                        GrenSyntax.Node { end = { column = 23, row = 3 }, start = { column = 1, row = 2 } }
                                            { arguments = []
                                            , expression =
                                                GrenSyntax.Node { end = { column = 23, row = 3 }, start = { column = 3, row = 3 } }
                                                    (GrenSyntax.Application
                                                        [ GrenSyntax.Node { end = { column = 7, row = 3 }, start = { column = 3, row = 3 } }
                                                            (GrenSyntax.FunctionOrValue [] "text")
                                                        , GrenSyntax.Node { end = { column = 23, row = 3 }, start = { column = 8, row = 3 } }
                                                            (GrenSyntax.Literal "Hello, World!")
                                                        ]
                                                    )
                                            , name = GrenSyntax.Node { end = { column = 5, row = 2 }, start = { column = 1, row = 2 } } "main"
                                            }
                                    , documentation = Nothing
                                    , signature =
                                        Just
                                            (GrenSyntax.Node { end = { column = 11, row = 1 }, start = { column = 1, row = 1 } }
                                                { name = GrenSyntax.Node { end = { column = 1, row = 1 }, start = { column = 1, row = 1 } } "main"
                                                , typeAnnotation =
                                                    GrenSyntax.Node { end = { column = 11, row = 1 }, start = { column = 3, row = 1 } }
                                                        (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { end = { column = 7, row = 1 }, start = { column = 3, row = 1 } } ( [], "Html" ))
                                                            [ GrenSyntax.Node { end = { column = 11, row = 1 }, start = { column = 8, row = 1 } }
                                                                (GrenSyntax.TypeAnnotationVariable "msg")
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 23 } }
                                (GrenSyntax.FunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 23 } }
                                            { name = GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } "main"
                                            , arguments = []
                                            , expression =
                                                GrenSyntax.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 23 } }
                                                    (GrenSyntax.Application
                                                        [ GrenSyntax.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 7 } } (GrenSyntax.FunctionOrValue [] "text")
                                                        , GrenSyntax.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 23 } } (GrenSyntax.Literal "Hello, World!")
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
                                GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 12 } }
                                    (GrenSyntax.FunctionDeclaration
                                        { documentation = Nothing
                                        , signature = Nothing
                                        , declaration =
                                            GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 12 } }
                                                { name = GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } "main"
                                                , arguments = []
                                                , expression = GrenSyntax.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 12 } } (GrenSyntax.FunctionOrValue [] "x")
                                                }
                                        }
                                    )
                            , comments = [ GrenSyntax.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } } "{- y -}" ]
                            }
                )
            , Test.test "function with a lot of symbols"
                (\() ->
                    "updateState update sendPort = curry <| (uncurry update) >> batchStateCmds sendPort"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 83 } }
                                (GrenSyntax.FunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 83 } }
                                            { name = GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } "updateState"
                                            , arguments = [ GrenSyntax.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 19 } } (GrenSyntax.PatternVariable "update"), GrenSyntax.Node { start = { row = 1, column = 20 }, end = { row = 1, column = 28 } } (GrenSyntax.PatternVariable "sendPort") ]
                                            , expression =
                                                GrenSyntax.Node { start = { row = 1, column = 31 }, end = { row = 1, column = 83 } }
                                                    (GrenSyntax.OperatorApplication "<|"
                                                        (GrenSyntax.Node { start = { row = 1, column = 31 }, end = { row = 1, column = 36 } } (GrenSyntax.FunctionOrValue [] "curry"))
                                                        (GrenSyntax.Node { start = { row = 1, column = 40 }, end = { row = 1, column = 83 } }
                                                            (GrenSyntax.OperatorApplication ">>"
                                                                (GrenSyntax.Node { start = { row = 1, column = 40 }, end = { row = 1, column = 56 } }
                                                                    (GrenSyntax.ParenthesizedExpression
                                                                        (GrenSyntax.Node { start = { row = 1, column = 41 }, end = { row = 1, column = 55 } }
                                                                            (GrenSyntax.Application
                                                                                [ GrenSyntax.Node { start = { row = 1, column = 41 }, end = { row = 1, column = 48 } } (GrenSyntax.FunctionOrValue [] "uncurry")
                                                                                , GrenSyntax.Node { start = { row = 1, column = 49 }, end = { row = 1, column = 55 } } (GrenSyntax.FunctionOrValue [] "update")
                                                                                ]
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                                (GrenSyntax.Node { start = { row = 1, column = 60 }, end = { row = 1, column = 83 } }
                                                                    (GrenSyntax.Application
                                                                        [ GrenSyntax.Node { start = { row = 1, column = 60 }, end = { row = 1, column = 74 } } (GrenSyntax.FunctionOrValue [] "batchStateCmds")
                                                                        , GrenSyntax.Node { start = { row = 1, column = 75 }, end = { row = 1, column = 83 } } (GrenSyntax.FunctionOrValue [] "sendPort")
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 7, column = 16 } }
                                (GrenSyntax.FunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 7, column = 16 } }
                                            { name = GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } } "update"
                                            , arguments =
                                                [ GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } (GrenSyntax.PatternVariable "msg")
                                                , GrenSyntax.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 17 } } (GrenSyntax.PatternVariable "model")
                                                ]
                                            , expression =
                                                GrenSyntax.Node { start = { row = 2, column = 3 }, end = { row = 7, column = 16 } }
                                                    (GrenSyntax.CaseExpression
                                                        { expression = GrenSyntax.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } (GrenSyntax.FunctionOrValue [] "msg")
                                                        , cases =
                                                            [ ( GrenSyntax.Node { start = { row = 3, column = 5 }, end = { row = 3, column = 14 } } (GrenSyntax.PatternVariant { moduleName = [], name = "Increment" } [])
                                                              , GrenSyntax.Node { start = { row = 4, column = 7 }, end = { row = 4, column = 16 } }
                                                                    (GrenSyntax.OperatorApplication "+"
                                                                        (GrenSyntax.Node { start = { row = 4, column = 7 }, end = { row = 4, column = 12 } } (GrenSyntax.FunctionOrValue [] "model"))
                                                                        (GrenSyntax.Node { start = { row = 4, column = 15 }, end = { row = 4, column = 16 } } (GrenSyntax.Integer 1))
                                                                    )
                                                              )
                                                            , ( GrenSyntax.Node { start = { row = 6, column = 5 }, end = { row = 6, column = 14 } } (GrenSyntax.PatternVariant { moduleName = [], name = "Decrement" } [])
                                                              , GrenSyntax.Node { start = { row = 7, column = 7 }, end = { row = 7, column = 16 } }
                                                                    (GrenSyntax.OperatorApplication "-"
                                                                        (GrenSyntax.Node { start = { row = 7, column = 7 }, end = { row = 7, column = 12 } } (GrenSyntax.FunctionOrValue [] "model"))
                                                                        (GrenSyntax.Node { start = { row = 7, column = 15 }, end = { row = 7, column = 16 } } (GrenSyntax.Integer 1))
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 3, column = 8 } }
                                (GrenSyntax.FunctionDeclaration
                                    { documentation = Nothing
                                    , signature =
                                        Just
                                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                                                { name = GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } } "update"
                                                , typeAnnotation = GrenSyntax.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } } (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } } ( [], "Model" )) [])
                                                }
                                            )
                                    , declaration =
                                        GrenSyntax.Node { start = { row = 2, column = 1 }, end = { row = 3, column = 8 } }
                                            { name = GrenSyntax.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 7 } } "update"
                                            , arguments =
                                                [ GrenSyntax.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } (GrenSyntax.PatternVariable "msg")
                                                , GrenSyntax.Node { start = { row = 2, column = 12 }, end = { row = 2, column = 17 } } (GrenSyntax.PatternVariable "model")
                                                ]
                                            , expression = GrenSyntax.Node { start = { row = 3, column = 5 }, end = { row = 3, column = 8 } } (GrenSyntax.FunctionOrValue [] "msg")
                                            }
                                    }
                                )
                            )
                )
            , Test.test "type alias"
                (\() ->
                    "type alias Foo = {color: String }"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 34 } }
                                (GrenSyntax.AliasDeclaration
                                    { documentation = Nothing
                                    , name = GrenSyntax.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 15 } } "Foo"
                                    , generics = []
                                    , typeAnnotation =
                                        GrenSyntax.Node { start = { row = 1, column = 18 }, end = { row = 1, column = 34 } }
                                            (GrenSyntax.TypeAnnotationRecord
                                                [ GrenSyntax.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 32 } }
                                                    ( GrenSyntax.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 24 } } "color"
                                                    , GrenSyntax.Node { start = { row = 1, column = 26 }, end = { row = 1, column = 32 } }
                                                        (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 26 }, end = { row = 1, column = 32 } } ( [], "String" )) [])
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 34 } }
                                (GrenSyntax.AliasDeclaration
                                    { documentation = Just (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 23 } } "{-| Foo is colorful -}")
                                    , name = GrenSyntax.Node { start = { row = 2, column = 12 }, end = { row = 2, column = 15 } } "Foo"
                                    , generics = []
                                    , typeAnnotation =
                                        GrenSyntax.Node { start = { row = 2, column = 18 }, end = { row = 2, column = 34 } }
                                            (GrenSyntax.TypeAnnotationRecord
                                                [ GrenSyntax.Node { start = { row = 2, column = 19 }, end = { row = 2, column = 32 } }
                                                    ( GrenSyntax.Node { start = { row = 2, column = 19 }, end = { row = 2, column = 24 } } "color"
                                                    , GrenSyntax.Node { start = { row = 2, column = 26 }, end = { row = 2, column = 32 } }
                                                        (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 2, column = 26 }, end = { row = 2, column = 32 } } ( [], "String" )) [])
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                                (GrenSyntax.AliasDeclaration
                                    { documentation = Nothing
                                    , name = GrenSyntax.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 15 } } "Foo"
                                    , generics = []
                                    , typeAnnotation =
                                        GrenSyntax.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 32 } }
                                            (GrenSyntax.TypeAnnotationRecord
                                                [ GrenSyntax.Node { start = { row = 1, column = 17 }, end = { row = 1, column = 30 } }
                                                    ( GrenSyntax.Node { start = { row = 1, column = 17 }, end = { row = 1, column = 22 } } "color"
                                                    , GrenSyntax.Node { start = { row = 1, column = 24 }, end = { row = 1, column = 30 } }
                                                        (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 24 }, end = { row = 1, column = 30 } } ( [], "String" )) [])
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 31 } }
                                (GrenSyntax.AliasDeclaration
                                    { documentation = Nothing
                                    , name = GrenSyntax.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 15 } } "Foo"
                                    , generics = [ GrenSyntax.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 17 } } "a" ]
                                    , typeAnnotation =
                                        GrenSyntax.Node { start = { row = 1, column = 20 }, end = { row = 1, column = 31 } }
                                            (GrenSyntax.TypeAnnotationRecord
                                                [ GrenSyntax.Node { start = { row = 1, column = 21 }, end = { row = 1, column = 29 } }
                                                    ( GrenSyntax.Node { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } } "some"
                                                    , GrenSyntax.Node { start = { row = 1, column = 28 }, end = { row = 1, column = 29 } } (GrenSyntax.TypeAnnotationVariable "a")
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 39 } }
                                (GrenSyntax.CustomTypeDeclaration
                                    { documentation = Nothing
                                    , name = GrenSyntax.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 11 } } "Color"
                                    , generics = []
                                    , constructors =
                                        [ GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } }
                                            { name = GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 18 } } "Blue"
                                            , arguments =
                                                [ GrenSyntax.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 25 } }
                                                    (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 25 } } ( [], "String" )) [])
                                                ]
                                            }
                                        , GrenSyntax.Node { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } }
                                            { name = GrenSyntax.Node { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } } "Red"
                                            , arguments = []
                                            }
                                        , GrenSyntax.Node { start = { row = 1, column = 34 }, end = { row = 1, column = 39 } }
                                            { name = GrenSyntax.Node { start = { row = 1, column = 34 }, end = { row = 1, column = 39 } } "Green"
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 39 } }
                                (GrenSyntax.CustomTypeDeclaration
                                    { documentation = Nothing
                                    , name = GrenSyntax.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 11 } } "Color"
                                    , generics = []
                                    , constructors =
                                        [ GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } }
                                            { name = GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 18 } } "Blue"
                                            , arguments =
                                                [ GrenSyntax.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 25 } }
                                                    (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 25 } } ( [], "String" )) [])
                                                ]
                                            }
                                        , GrenSyntax.Node { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } }
                                            { name = GrenSyntax.Node { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } } "Red"
                                            , arguments = []
                                            }
                                        , GrenSyntax.Node { start = { row = 1, column = 34 }, end = { row = 1, column = 39 } }
                                            { name = GrenSyntax.Node { start = { row = 1, column = 34 }, end = { row = 1, column = 39 } } "Green"
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 39 } }
                                (GrenSyntax.CustomTypeDeclaration
                                    { documentation = Nothing
                                    , name = GrenSyntax.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 11 } } "Color"
                                    , generics = []
                                    , constructors =
                                        [ GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } }
                                            { name = GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 18 } } "Blue"
                                            , arguments =
                                                [ GrenSyntax.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 25 } }
                                                    (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 25 } } ( [], "String" )) [])
                                                ]
                                            }
                                        , GrenSyntax.Node { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } }
                                            { name = GrenSyntax.Node { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } } "Red"
                                            , arguments = []
                                            }
                                        , GrenSyntax.Node { start = { row = 1, column = 34 }, end = { row = 1, column = 39 } }
                                            { name = GrenSyntax.Node { start = { row = 1, column = 34 }, end = { row = 1, column = 39 } } "Green"
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 39 } }
                                (GrenSyntax.CustomTypeDeclaration
                                    { documentation = Just (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 19 } } "{-| Classic RGB -}")
                                    , name = GrenSyntax.Node { start = { row = 2, column = 6 }, end = { row = 2, column = 11 } } "Color"
                                    , generics = []
                                    , constructors =
                                        [ GrenSyntax.Node
                                            { start = { row = 2, column = 14 }
                                            , end = { row = 2, column = 25 }
                                            }
                                            { name = GrenSyntax.Node { start = { row = 2, column = 14 }, end = { row = 2, column = 18 } } "Blue"
                                            , arguments =
                                                [ GrenSyntax.Node { start = { row = 2, column = 19 }, end = { row = 2, column = 25 } }
                                                    (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 2, column = 19 }, end = { row = 2, column = 25 } } ( [], "String" )) [])
                                                ]
                                            }
                                        , GrenSyntax.Node { start = { row = 2, column = 28 }, end = { row = 2, column = 31 } }
                                            { name = GrenSyntax.Node { start = { row = 2, column = 28 }, end = { row = 2, column = 31 } } "Red"
                                            , arguments = []
                                            }
                                        , GrenSyntax.Node { start = { row = 2, column = 34 }, end = { row = 2, column = 39 } }
                                            { name = GrenSyntax.Node { start = { row = 2, column = 34 }, end = { row = 2, column = 39 } } "Green"
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                                (GrenSyntax.CustomTypeDeclaration
                                    { documentation = Nothing
                                    , name = GrenSyntax.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } "D"
                                    , generics = []
                                    , constructors =
                                        [ GrenSyntax.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } }
                                            { name = GrenSyntax.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 11 } } "C"
                                            , arguments =
                                                [ GrenSyntax.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 13 } } (GrenSyntax.TypeAnnotationVariable "a")
                                                , GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } }
                                                    (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } } ( [], "B" )) [])
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                                (GrenSyntax.CustomTypeDeclaration
                                    { documentation = Nothing
                                    , name = GrenSyntax.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } "D"
                                    , generics = []
                                    , constructors =
                                        [ GrenSyntax.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } }
                                            { name = GrenSyntax.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 11 } } "C"
                                            , arguments =
                                                [ GrenSyntax.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 13 } }
                                                    (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 13 } } ( [], "B" )) [])
                                                , GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } } (GrenSyntax.TypeAnnotationVariable "a")
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                                (GrenSyntax.CustomTypeDeclaration
                                    { documentation = Nothing
                                    , name = GrenSyntax.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 11 } } "Maybe"
                                    , generics = [ GrenSyntax.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 13 } } "a" ]
                                    , constructors =
                                        [ GrenSyntax.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 22 } }
                                            { name = GrenSyntax.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 20 } } "Just"
                                            , arguments = [ GrenSyntax.Node { start = { row = 1, column = 21 }, end = { row = 1, column = 22 } } (GrenSyntax.TypeAnnotationVariable "a") ]
                                            }
                                        , GrenSyntax.Node { start = { row = 1, column = 25 }, end = { row = 1, column = 32 } }
                                            { name = GrenSyntax.Node { start = { row = 1, column = 25 }, end = { row = 1, column = 32 } } "Nothing"
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 28 } }
                                (GrenSyntax.InfixDeclaration
                                    { direction = GrenSyntax.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 12 } } GrenSyntax.Right
                                    , precedence = GrenSyntax.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 14 } } 7
                                    , operator = GrenSyntax.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 20 } } "</>"
                                    , function = GrenSyntax.Node { start = { row = 1, column = 23 }, end = { row = 1, column = 28 } } "slash"
                                    }
                                )
                            )
                )
            , Test.test "left infix"
                (\() ->
                    "infix left  8 (<?>) = questionMark"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 35 } }
                                (GrenSyntax.InfixDeclaration
                                    { direction = GrenSyntax.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 11 } } GrenSyntax.Left
                                    , precedence = GrenSyntax.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 14 } } 8
                                    , operator = GrenSyntax.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 20 } } "<?>"
                                    , function = GrenSyntax.Node { start = { row = 1, column = 23 }, end = { row = 1, column = 35 } } "questionMark"
                                    }
                                )
                            )
                )
            , Test.test "non infix"
                (\() ->
                    "infix non   4 (==) = eq"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 24 } }
                                (GrenSyntax.InfixDeclaration
                                    { direction = GrenSyntax.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 10 } } GrenSyntax.Non
                                    , precedence = GrenSyntax.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 14 } } 4
                                    , operator = GrenSyntax.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 19 } } "=="
                                    , function = GrenSyntax.Node { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } } "eq"
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } GrenSyntax.TypeAnnotationUnit)
                )
            , Test.test "unitTypeReference with spaces"
                (\() ->
                    "( )"
                        |> expectFailsToParse GrenParserLenient.type_
                )
            , Test.test "4-tuple type annotation is invalid"
                (\() ->
                    "(Int,String,(),a)"
                        |> expectFailsToParse GrenParserLenient.type_
                )
            , Test.test "qualified type reference"
                (\() ->
                    "Foo.Bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                                (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } } ( [ "Foo" ], "Bar" )) [])
                            )
                )
            , Test.test "typeAnnotationNoFn"
                (\() ->
                    "Bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } ( [], "Bar" )) [])
                            )
                )
            , Test.test "typedTypeReference 1"
                (\() ->
                    "Foo () a Bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                                (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } ( [], "Foo" ))
                                    [ GrenSyntax.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 7 } } GrenSyntax.TypeAnnotationUnit
                                    , GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } } (GrenSyntax.TypeAnnotationVariable "a")
                                    , GrenSyntax.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 13 } }
                                        (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 13 } } ( [], "Bar" )) [])
                                    ]
                                )
                            )
                )
            , Test.test "typedTypeReference 2"
                (\() ->
                    "Foo () a Bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                                (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } ( [], "Foo" ))
                                    [ GrenSyntax.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 7 } } GrenSyntax.TypeAnnotationUnit
                                    , GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } } (GrenSyntax.TypeAnnotationVariable "a")
                                    , GrenSyntax.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 13 } }
                                        (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 13 } } ( [], "Bar" )) [])
                                    ]
                                )
                            )
                )
            , Test.test "recordTypeReference empty"
                (\() ->
                    "{}"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } (GrenSyntax.TypeAnnotationRecord []))
                )
            , Test.test "recordTypeReference one field"
                (\() ->
                    "{color: String }"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 17 } }
                                (GrenSyntax.TypeAnnotationRecord
                                    [ GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 15 } }
                                        ( GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } "color"
                                        , GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 15 } }
                                            (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 15 } } ( [], "String" )) [])
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "recordTypeReference one field with name-value separator ="
                (\() ->
                    "{color= String }"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 17 } }
                                (GrenSyntax.TypeAnnotationRecord
                                    [ GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 15 } }
                                        ( GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } "color"
                                        , GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 15 } }
                                            (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 15 } } ( [], "String" )) [])
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "recordTypeReference one field with name-value separator empty"
                (\() ->
                    "{color  String }"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 17 } }
                                (GrenSyntax.TypeAnnotationRecord
                                    [ GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 15 } }
                                        ( GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } "color"
                                        , GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 15 } }
                                            (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 15 } } ( [], "String" )) [])
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "type record with field name colliding with keyword"
                (\() ->
                    "{  as : Int, b : Int }"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (GrenSyntax.Node { end = { column = 23, row = 1 }, start = { column = 1, row = 1 } }
                                (GrenSyntax.TypeAnnotationRecord
                                    [ GrenSyntax.Node { end = { column = 12, row = 1 }, start = { column = 4, row = 1 } }
                                        ( GrenSyntax.Node { end = { column = 6, row = 1 }, start = { column = 4, row = 1 } } "as_"
                                        , GrenSyntax.Node { end = { column = 12, row = 1 }, start = { column = 9, row = 1 } }
                                            (GrenSyntax.TypeAnnotationConstruct
                                                (GrenSyntax.Node { end = { column = 12, row = 1 }, start = { column = 9, row = 1 } }
                                                    ( [], "Int" )
                                                )
                                                []
                                            )
                                        )
                                    , GrenSyntax.Node { end = { column = 22, row = 1 }, start = { column = 14, row = 1 } }
                                        ( GrenSyntax.Node { end = { column = 15, row = 1 }, start = { column = 14, row = 1 } } "b"
                                        , GrenSyntax.Node { end = { column = 21, row = 1 }, start = { column = 18, row = 1 } }
                                            (GrenSyntax.TypeAnnotationConstruct
                                                (GrenSyntax.Node { end = { column = 21, row = 1 }, start = { column = 18, row = 1 } }
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
                            (GrenSyntax.Node { end = { column = 23, row = 1 }, start = { column = 1, row = 1 } }
                                (GrenSyntax.TypeAnnotationRecord
                                    [ GrenSyntax.Node { end = { column = 12, row = 1 }, start = { column = 5, row = 1 } }
                                        ( GrenSyntax.Node { end = { column = 6, row = 1 }, start = { column = 5, row = 1 } } "a"
                                        , GrenSyntax.Node { end = { column = 12, row = 1 }, start = { column = 9, row = 1 } }
                                            (GrenSyntax.TypeAnnotationConstruct
                                                (GrenSyntax.Node { end = { column = 12, row = 1 }, start = { column = 9, row = 1 } }
                                                    ( [], "Int" )
                                                )
                                                []
                                            )
                                        )
                                    , GrenSyntax.Node { end = { column = 22, row = 1 }, start = { column = 14, row = 1 } }
                                        ( GrenSyntax.Node { end = { column = 15, row = 1 }, start = { column = 14, row = 1 } } "b"
                                        , GrenSyntax.Node { end = { column = 21, row = 1 }, start = { column = 18, row = 1 } }
                                            (GrenSyntax.TypeAnnotationConstruct
                                                (GrenSyntax.Node { end = { column = 21, row = 1 }, start = { column = 18, row = 1 } }
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
                            (GrenSyntax.Node { end = { column = 23, row = 1 }, start = { column = 1, row = 1 } }
                                (GrenSyntax.TypeAnnotationRecord
                                    [ GrenSyntax.Node { end = { column = 12, row = 1 }, start = { column = 5, row = 1 } }
                                        ( GrenSyntax.Node { end = { column = 6, row = 1 }, start = { column = 5, row = 1 } } "a"
                                        , GrenSyntax.Node { end = { column = 12, row = 1 }, start = { column = 9, row = 1 } }
                                            (GrenSyntax.TypeAnnotationConstruct
                                                (GrenSyntax.Node { end = { column = 12, row = 1 }, start = { column = 9, row = 1 } }
                                                    ( [], "Int" )
                                                )
                                                []
                                            )
                                        )
                                    , GrenSyntax.Node { end = { column = 22, row = 1 }, start = { column = 14, row = 1 } }
                                        ( GrenSyntax.Node { end = { column = 15, row = 1 }, start = { column = 14, row = 1 } } "b"
                                        , GrenSyntax.Node { end = { column = 21, row = 1 }, start = { column = 18, row = 1 } }
                                            (GrenSyntax.TypeAnnotationConstruct
                                                (GrenSyntax.Node { end = { column = 21, row = 1 }, start = { column = 18, row = 1 } }
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 43 } }
                                (GrenSyntax.TypeAnnotationRecordExtension (GrenSyntax.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 7 } } "attr")
                                    (GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 42 } }
                                        [ GrenSyntax.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 25 } }
                                            ( GrenSyntax.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 18 } } "position"
                                            , GrenSyntax.Node { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } }
                                                (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } } ( [], "Vec2" )) [])
                                            )
                                        , GrenSyntax.Node { start = { row = 1, column = 27 }, end = { row = 1, column = 42 } }
                                            ( GrenSyntax.Node { start = { row = 1, column = 27 }, end = { row = 1, column = 34 } } "texture"
                                            , GrenSyntax.Node { start = { row = 1, column = 37 }, end = { row = 1, column = 41 } }
                                                (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 37 }, end = { row = 1, column = 41 } } ( [], "Vec2" )) [])
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 43 } }
                                (GrenSyntax.TypeAnnotationRecordExtension (GrenSyntax.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 7 } } "attr")
                                    (GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 42 } }
                                        [ GrenSyntax.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 25 } }
                                            ( GrenSyntax.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 18 } } "position"
                                            , GrenSyntax.Node { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } }
                                                (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } } ( [], "Vec2" )) [])
                                            )
                                        , GrenSyntax.Node { start = { row = 1, column = 27 }, end = { row = 1, column = 42 } }
                                            ( GrenSyntax.Node { start = { row = 1, column = 27 }, end = { row = 1, column = 34 } } "texture"
                                            , GrenSyntax.Node { start = { row = 1, column = 37 }, end = { row = 1, column = 41 } }
                                                (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 37 }, end = { row = 1, column = 41 } } ( [], "Vec2" )) [])
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 43 } }
                                (GrenSyntax.TypeAnnotationRecordExtension (GrenSyntax.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 7 } } "attr")
                                    (GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 42 } }
                                        [ GrenSyntax.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 25 } }
                                            ( GrenSyntax.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 18 } } "position"
                                            , GrenSyntax.Node { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } }
                                                (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } } ( [], "Vec2" )) [])
                                            )
                                        , GrenSyntax.Node { start = { row = 1, column = 27 }, end = { row = 1, column = 42 } }
                                            ( GrenSyntax.Node { start = { row = 1, column = 27 }, end = { row = 1, column = 34 } } "texture"
                                            , GrenSyntax.Node { start = { row = 1, column = 37 }, end = { row = 1, column = 41 } }
                                                (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 37 }, end = { row = 1, column = 41 } } ( [], "Vec2" )) [])
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 37 } }
                                (GrenSyntax.TypeAnnotationRecord
                                    [ GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 35 } }
                                        ( GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } "color"
                                        , GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 35 } }
                                            (GrenSyntax.TypeAnnotationRecord
                                                [ GrenSyntax.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 17 } }
                                                    ( GrenSyntax.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 11 } } "r"
                                                    , GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 17 } }
                                                        (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 17 } } ( [], "Int" )) [])
                                                    )
                                                , GrenSyntax.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 25 } }
                                                    ( GrenSyntax.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } } "g"
                                                    , GrenSyntax.Node { start = { row = 1, column = 22 }, end = { row = 1, column = 25 } }
                                                        (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 22 }, end = { row = 1, column = 25 } } ( [], "Int" )) [])
                                                    )
                                                , GrenSyntax.Node { start = { row = 1, column = 27 }, end = { row = 1, column = 34 } }
                                                    ( GrenSyntax.Node { start = { row = 1, column = 27 }, end = { row = 1, column = 28 } } "b"
                                                    , GrenSyntax.Node { start = { row = 1, column = 30 }, end = { row = 1, column = 33 } }
                                                        (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 30 }, end = { row = 1, column = 33 } } ( [], "Int" )) [])
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 36 } }
                                (GrenSyntax.TypeAnnotationRecord
                                    [ GrenSyntax.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 12 } }
                                        ( GrenSyntax.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 6 } } "foo"
                                        , GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } ( [], "Int" )) [])
                                        )
                                    , GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 23 } }
                                        ( GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 17 } } "bar"
                                        , GrenSyntax.Node { start = { row = 1, column = 20 }, end = { row = 1, column = 23 } } (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 20 }, end = { row = 1, column = 23 } } ( [], "Int" )) [])
                                        )
                                    , GrenSyntax.Node { start = { row = 1, column = 25 }, end = { row = 1, column = 35 } }
                                        ( GrenSyntax.Node { start = { row = 1, column = 25 }, end = { row = 1, column = 28 } } "baz"
                                        , GrenSyntax.Node { start = { row = 1, column = 31 }, end = { row = 1, column = 34 } } (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 31 }, end = { row = 1, column = 34 } } ( [], "Int" )) [])
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "recordTypeReference with generic"
                (\() ->
                    "{color: s }"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                (GrenSyntax.TypeAnnotationRecord
                                    [ GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 10 } }
                                        ( GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } "color"
                                        , GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (GrenSyntax.TypeAnnotationVariable "s")
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "function type reference"
                (\() ->
                    "Foo -> Bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                                (GrenSyntax.TypeAnnotationFunction
                                    (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                        (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } ( [], "Foo" )) [])
                                    )
                                    (GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }
                                        (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } ( [], "Bar" )) [])
                                    )
                                )
                            )
                )
            , Test.test "function type reference multiple"
                (\() ->
                    "Foo -> Bar -> baz"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 18 } }
                                (GrenSyntax.TypeAnnotationFunction
                                    (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                        (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } ( [], "Foo" )) [])
                                    )
                                    (GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 18 } }
                                        (GrenSyntax.TypeAnnotationFunction
                                            (GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }
                                                (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } ( [], "Bar" )) [])
                                            )
                                            (GrenSyntax.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 18 } } (GrenSyntax.TypeAnnotationVariable "baz"))
                                        )
                                    )
                                )
                            )
                )
            , Test.test "function type reference multiple with consecutive ->"
                (\() ->
                    "Foo->->Bar -> baz"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 18 } }
                                (GrenSyntax.TypeAnnotationFunction
                                    (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                        (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } ( [], "Foo" )) [])
                                    )
                                    (GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 18 } }
                                        (GrenSyntax.TypeAnnotationFunction
                                            (GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }
                                                (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } ( [], "Bar" )) [])
                                            )
                                            (GrenSyntax.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 18 } } (GrenSyntax.TypeAnnotationVariable "baz"))
                                        )
                                    )
                                )
                            )
                )
            , Test.test "function type reference generics"
                (\() ->
                    "cMsg -> cModel -> a"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 20 } }
                                (GrenSyntax.TypeAnnotationFunction
                                    (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } (GrenSyntax.TypeAnnotationVariable "cMsg"))
                                    (GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 20 } }
                                        (GrenSyntax.TypeAnnotationFunction
                                            (GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 15 } } (GrenSyntax.TypeAnnotationVariable "cModel"))
                                            (GrenSyntax.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } } (GrenSyntax.TypeAnnotationVariable "a"))
                                        )
                                    )
                                )
                            )
                )
            , Test.test "annotation with parens"
                (\() ->
                    "Msg -> Model -> (Model->Cmd Msg)"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (GrenSyntax.Node { end = { column = 33, row = 1 }, start = { column = 1, row = 1 } }
                                (GrenSyntax.TypeAnnotationFunction
                                    (GrenSyntax.Node { end = { column = 4, row = 1 }, start = { column = 1, row = 1 } }
                                        (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { end = { column = 4, row = 1 }, start = { column = 1, row = 1 } } ( [], "Msg" )) [])
                                    )
                                    (GrenSyntax.Node { end = { column = 33, row = 1 }, start = { column = 8, row = 1 } }
                                        (GrenSyntax.TypeAnnotationFunction
                                            (GrenSyntax.Node { end = { column = 13, row = 1 }, start = { column = 8, row = 1 } }
                                                (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { end = { column = 13, row = 1 }, start = { column = 8, row = 1 } } ( [], "Model" )) [])
                                            )
                                            (GrenSyntax.Node { end = { column = 33, row = 1 }, start = { column = 17, row = 1 } }
                                                (GrenSyntax.TypeAnnotationParenthesized
                                                    (GrenSyntax.Node { end = { column = 32, row = 1 }, start = { column = 18, row = 1 } }
                                                        (GrenSyntax.TypeAnnotationFunction
                                                            (GrenSyntax.Node { end = { column = 23, row = 1 }, start = { column = 18, row = 1 } }
                                                                (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { end = { column = 23, row = 1 }, start = { column = 18, row = 1 } } ( [], "Model" )) [])
                                                            )
                                                            (GrenSyntax.Node { end = { column = 32, row = 1 }, start = { column = 25, row = 1 } }
                                                                (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { end = { column = 28, row = 1 }, start = { column = 25, row = 1 } } ( [], "Cmd" ))
                                                                    [ GrenSyntax.Node { end = { column = 32, row = 1 }, start = { column = 29, row = 1 } }
                                                                        (GrenSyntax.TypeAnnotationConstruct
                                                                            (GrenSyntax.Node { end = { column = 32, row = 1 }, start = { column = 29, row = 1 } } ( [], "Msg" ))
                                                                            []
                                                                        )
                                                                    ]
                                                                )
                                                            )
                                                        )
                                                    )
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
                            (GrenSyntax.Node { end = { column = 29, row = 1 }, start = { column = 1, row = 1 } }
                                (GrenSyntax.TypeAnnotationFunction (GrenSyntax.Node { end = { column = 24, row = 1 }, start = { column = 1, row = 1 } } (GrenSyntax.TypeAnnotationParenthesized (GrenSyntax.Node { end = { column = 22, row = 1 }, start = { column = 3, row = 1 } } (GrenSyntax.TypeAnnotationFunction (GrenSyntax.Node { end = { column = 7, row = 1 }, start = { column = 3, row = 1 } } (GrenSyntax.TypeAnnotationVariable "cMsg")) (GrenSyntax.Node { end = { column = 22, row = 1 }, start = { column = 11, row = 1 } } (GrenSyntax.TypeAnnotationFunction (GrenSyntax.Node { end = { column = 17, row = 1 }, start = { column = 11, row = 1 } } (GrenSyntax.TypeAnnotationVariable "cModel")) (GrenSyntax.Node { end = { column = 22, row = 1 }, start = { column = 21, row = 1 } } (GrenSyntax.TypeAnnotationVariable "a"))))))))
                                    (GrenSyntax.Node { end = { column = 29, row = 1 }, start = { column = 28, row = 1 } }
                                        (GrenSyntax.TypeAnnotationVariable "b")
                                    )
                                )
                            )
                )
            , Test.test "type with params"
                (\() ->
                    "(Foo -> Bar)"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (GrenSyntax.Node { end = { column = 13, row = 1 }, start = { column = 1, row = 1 } }
                                (GrenSyntax.TypeAnnotationParenthesized
                                    (GrenSyntax.Node { end = { column = 12, row = 1 }, start = { column = 2, row = 1 } }
                                        (GrenSyntax.TypeAnnotationFunction (GrenSyntax.Node { end = { column = 5, row = 1 }, start = { column = 2, row = 1 } } (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { end = { column = 5, row = 1 }, start = { column = 2, row = 1 } } ( [], "Foo" )) []))
                                            (GrenSyntax.Node { end = { column = 12, row = 1 }, start = { column = 9, row = 1 } }
                                                (GrenSyntax.TypeAnnotationConstruct
                                                    (GrenSyntax.Node { end = { column = 12, row = 1 }, start = { column = 9, row = 1 } } ( [], "Bar" ))
                                                    []
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                )
            , Test.test "function type reference multiple and parens"
                (\() ->
                    "(Foo -> Bar) -> baz"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (GrenSyntax.Node { end = { column = 20, row = 1 }, start = { column = 1, row = 1 } }
                                (GrenSyntax.TypeAnnotationFunction
                                    (GrenSyntax.Node { end = { column = 13, row = 1 }, start = { column = 1, row = 1 } }
                                        (GrenSyntax.TypeAnnotationParenthesized
                                            (GrenSyntax.Node { end = { column = 12, row = 1 }, start = { column = 2, row = 1 } }
                                                (GrenSyntax.TypeAnnotationFunction
                                                    (GrenSyntax.Node { end = { column = 5, row = 1 }, start = { column = 2, row = 1 } }
                                                        (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { end = { column = 5, row = 1 }, start = { column = 2, row = 1 } } ( [], "Foo" )) [])
                                                    )
                                                    (GrenSyntax.Node { end = { column = 12, row = 1 }, start = { column = 9, row = 1 } }
                                                        (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { end = { column = 12, row = 1 }, start = { column = 9, row = 1 } } ( [], "Bar" )) [])
                                                    )
                                                )
                                            )
                                        )
                                    )
                                    (GrenSyntax.Node { end = { column = 20, row = 1 }, start = { column = 17, row = 1 } }
                                        (GrenSyntax.TypeAnnotationVariable "baz")
                                    )
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 3 } }
                                (GrenSyntax.TypeAnnotationConstruct
                                    (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } ( [], "Maybe" ))
                                    [ GrenSyntax.Node { start = { row = 2, column = 2 }, end = { row = 2, column = 3 } } (GrenSyntax.TypeAnnotationVariable "a") ]
                                )
                            )
                )
            , Test.test "issue #5 - no spaces between type and generic with parens"
                (\() ->
                    "List(String)"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (GrenSyntax.Node { end = { column = 13, row = 1 }, start = { column = 1, row = 1 } }
                                (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { end = { column = 5, row = 1 }, start = { column = 1, row = 1 } } ( [], "List" ))
                                    [ GrenSyntax.Node { end = { column = 13, row = 1 }, start = { column = 5, row = 1 } }
                                        (GrenSyntax.TypeAnnotationParenthesized
                                            (GrenSyntax.Node { end = { column = 12, row = 1 }, start = { column = 6, row = 1 } }
                                                (GrenSyntax.TypeAnnotationConstruct
                                                    (GrenSyntax.Node { end = { column = 12, row = 1 }, start = { column = 6, row = 1 } } ( [], "String" ))
                                                    []
                                                )
                                            )
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "parse type with multiple params"
                (\() ->
                    "Dict String Int"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 16 } }
                                (GrenSyntax.TypeAnnotationConstruct
                                    (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } ( [], "Dict" ))
                                    [ GrenSyntax.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 12 } }
                                        (GrenSyntax.TypeAnnotationConstruct
                                            (GrenSyntax.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 12 } } ( [], "String" ))
                                            []
                                        )
                                    , GrenSyntax.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 16 } }
                                        (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 16 } } ( [], "Int" )) [])
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
                            |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                            |> Expect.equal
                                (Just (GrenSyntax.Hex 67108863))
                    )
                , Test.test "hex FF"
                    (\() ->
                        "0xFF"
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                            |> Expect.equal
                                (Just (GrenSyntax.Hex 255))
                    )
                , Test.test "hex"
                    (\() ->
                        "0x2A"
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                            |> Expect.equal
                                (Just (GrenSyntax.Hex 42))
                    )
                , Test.test "Hex integer literal"
                    (\() ->
                        "0x56"
                            |> expectSyntaxWithoutComments GrenParserLenient.expression (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } (GrenSyntax.Hex 86))
                    )
                , Test.test "Integer literal"
                    (\() ->
                        "101"
                            |> expectSyntaxWithoutComments GrenParserLenient.expression (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (GrenSyntax.Integer 101))
                    )
                , Test.test "float"
                    (\() ->
                        "2.0"
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                            |> Expect.equal
                                (Just (GrenSyntax.Floatable 2.0))
                    )
                , Test.test "integer with negative exponent"
                    (\() ->
                        "2e-2"
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                            |> Expect.equal
                                (Just (GrenSyntax.Floatable 0.02))
                    )
                , Test.test "literal e is not a number"
                    (\() ->
                        "e"
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                            |> Expect.equal
                                (Just (GrenSyntax.FunctionOrValue [] "e"))
                    )
                , Test.test "integer with negative exponent (uppercase E)"
                    (\() ->
                        "2E-2"
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                            |> Expect.equal
                                (Just (GrenSyntax.Floatable 0.02))
                    )
                , Test.test "integer with positive exponent"
                    (\() ->
                        "2e+2"
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                            |> Expect.equal
                                (Just (GrenSyntax.Floatable 200.0))
                    )
                , Test.test "float with negative exponent"
                    (\() ->
                        "2.0e-2"
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                            |> Expect.equal
                                (Just (GrenSyntax.Floatable 0.02))
                    )
                , Test.test "float with negative exponent (uppercase E)"
                    (\() ->
                        "2.0E-2"
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                            |> Expect.equal
                                (Just (GrenSyntax.Floatable 0.02))
                    )
                , Test.test "float with positive exponent"
                    (\() ->
                        "2.0e+2"
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                            |> Expect.equal
                                (Just (GrenSyntax.Floatable 200.0))
                    )
                ]
            , Test.test "String literal"
                (\() ->
                    "\"Bar\""
                        |> expectSyntaxWithoutComments GrenParserLenient.expression (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } (GrenSyntax.Literal "Bar"))
                )
            , Test.test "multiline string"
                (\() ->
                    "\"\"\"Bar foo \n a\"\"\""
                        |> GrenParserLenient.run GrenParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                        |> Expect.equal (Just (GrenSyntax.Literal "Bar foo \n a"))
                )
            , Test.test "multiline string escape"
                (\() ->
                    """\"\"\" \\\"\"\" \"\"\""""
                        |> GrenParserLenient.run GrenParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                        |> Expect.equal (Just (GrenSyntax.Literal """ \"\"\" """))
                )
            , Test.test "character escaped"
                (\() ->
                    "'\\''"
                        |> GrenParserLenient.run GrenParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                        |> Expect.equal (Just (GrenSyntax.CharLiteral '\''))
                )
            , Test.test "character escaped - 2"
                (\() ->
                    "'\\r'"
                        |> GrenParserLenient.run GrenParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                        |> Expect.equal (Just (GrenSyntax.CharLiteral '\u{000D}'))
                )
            , Test.test "unicode char"
                (\() ->
                    "'\\u{000D}'"
                        |> GrenParserLenient.run GrenParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                        |> Expect.equal (Just (GrenSyntax.CharLiteral '\u{000D}'))
                )
            , Test.test "unicode char with lowercase hex"
                (\() ->
                    "'\\u{000d}'"
                        |> GrenParserLenient.run GrenParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                        |> Expect.equal (Just (GrenSyntax.CharLiteral '\u{000D}'))
                )
            , Test.test "string escaped 3"
                (\() ->
                    "\"\\\"\""
                        |> GrenParserLenient.run GrenParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                        |> Expect.equal (Just (GrenSyntax.Literal "\""))
                )
            , Test.test "string escaped"
                (\() ->
                    "\"foo\\\\\""
                        |> GrenParserLenient.run GrenParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                        |> Expect.equal (Just (GrenSyntax.Literal "foo\\"))
                )
            , Test.test "character escaped 3"
                (\() ->
                    "'\\n'"
                        |> GrenParserLenient.run GrenParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                        |> Expect.equal (Just (GrenSyntax.CharLiteral '\n'))
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
                        |> expectSyntaxWithoutComments GrenParserLenient.expression (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (GrenSyntax.CharLiteral 'c'))
                )
            , Test.test "tuple expression"
                (\() ->
                    "(1,2)"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                                (GrenSyntax.TupledExpression
                                    [ GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (GrenSyntax.Integer 1)
                                    , GrenSyntax.Node { start = { row = 1, column = 4 }, end = { row = 1, column = 5 } } (GrenSyntax.Integer 2)
                                    ]
                                )
                            )
                )
            , Test.test "triple expression"
                (\() ->
                    "(1,2,3)"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                                (GrenSyntax.TupledExpression
                                    [ GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (GrenSyntax.Integer 1)
                                    , GrenSyntax.Node { start = { row = 1, column = 4 }, end = { row = 1, column = 5 } } (GrenSyntax.Integer 2)
                                    , GrenSyntax.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } (GrenSyntax.Integer 3)
                                    ]
                                )
                            )
                )
            , Test.test "tuple expression with spaces"
                (\() ->
                    "( 1  ,  2 )"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                (GrenSyntax.TupledExpression
                                    [ GrenSyntax.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (GrenSyntax.Integer 1)
                                    , GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (GrenSyntax.Integer 2)
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
                        |> expectSyntaxWithoutComments GrenParserLenient.expression (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } (GrenSyntax.Literal "Bar foo \n a"))
                )
            , Test.test "Regression test for multiline strings with backslashes"
                (\() ->
                    "a = \"\"\"\\{\\}\"\"\""
                        |> expectFailsToParse GrenParserLenient.declaration
                )
            , Test.test "Regression test 2 for multiline strings with backslashes"
                (\() ->
                    "\"\"\"\\\\{\\\\}\"\"\""
                        |> expectSyntaxWithoutComments GrenParserLenient.expression (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } } (GrenSyntax.Literal "\\{\\}"))
                )
            , Test.test "Regression test 3 for multiline strings with backslashes"
                (\() ->
                    "\"\"\"\\\\a-blablabla-\\\\b\"\"\""
                        |> expectSyntaxWithoutComments GrenParserLenient.expression (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 24 } } (GrenSyntax.Literal "\\a-blablabla-\\b"))
                )
            , Test.test "Type expression for upper case"
                (\() ->
                    "Bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (GrenSyntax.FunctionOrValue [] "Bar"))
                )
            , Test.test "Type expression for lower case"
                (\() ->
                    "bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (GrenSyntax.FunctionOrValue [] "bar"))
                )
            , Test.test "Type expression for lower case but qualified"
                (\() ->
                    "Bar.foo"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } } (GrenSyntax.FunctionOrValue [ "Bar" ] "foo"))
                )
            , Test.test "parenthesizedExpression"
                (\() ->
                    "(bar)"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                                (GrenSyntax.ParenthesizedExpression
                                    (GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 5 } } (GrenSyntax.FunctionOrValue [] "bar"))
                                )
                            )
                )
            , Test.test "parenthesized expression starting with a negation"
                (\() ->
                    "(-1 * sign)"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                (GrenSyntax.ParenthesizedExpression
                                    (GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 11 } }
                                        (GrenSyntax.OperatorApplication "*"
                                            (GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } }
                                                (GrenSyntax.Negation (GrenSyntax.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (GrenSyntax.Integer 1)))
                                            )
                                            (GrenSyntax.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 11 } } (GrenSyntax.FunctionOrValue [] "sign"))
                                        )
                                    )
                                )
                            )
                )
            , Test.test "application expression"
                (\() ->
                    "List.concat []"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                                (GrenSyntax.Application
                                    [ GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } (GrenSyntax.FunctionOrValue [ "List" ] "concat")
                                    , GrenSyntax.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 15 } } (GrenSyntax.ListExpr [])
                                    ]
                                )
                            )
                )
            , Test.test "Binary operation"
                (\() ->
                    "model + 1"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                                (GrenSyntax.OperatorApplication "+"
                                    (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } (GrenSyntax.FunctionOrValue [] "model"))
                                    (GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (GrenSyntax.Integer 1))
                                )
                            )
                )
            , Test.test "Nested binary operations (+ and ==)"
                (\() ->
                    "count + 1 == 1"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                                (GrenSyntax.OperatorApplication "=="
                                    (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                                        (GrenSyntax.OperatorApplication "+"
                                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } (GrenSyntax.FunctionOrValue [] "count"))
                                            (GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (GrenSyntax.Integer 1))
                                        )
                                    )
                                    (GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } } (GrenSyntax.Integer 1))
                                )
                            )
                )
            , Test.test "Nested binary operations (+ and /=)"
                (\() ->
                    "count + 1 /= 1"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                                (GrenSyntax.OperatorApplication "/="
                                    (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                                        (GrenSyntax.OperatorApplication "+"
                                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } (GrenSyntax.FunctionOrValue [] "count"))
                                            (GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (GrenSyntax.Integer 1))
                                        )
                                    )
                                    (GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } } (GrenSyntax.Integer 1))
                                )
                            )
                )
            , Test.test "Nested binary operations (+ and //)"
                (\() ->
                    "count + 1 // 2"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                                (GrenSyntax.OperatorApplication "+"
                                    (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } (GrenSyntax.FunctionOrValue [] "count"))
                                    (GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 15 } }
                                        (GrenSyntax.OperatorApplication "//"
                                            (GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (GrenSyntax.Integer 1))
                                            (GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } } (GrenSyntax.Integer 2))
                                        )
                                    )
                                )
                            )
                )
            , Test.test "Nested binary operations (&& and <|)"
                (\() ->
                    "condition && condition <| f"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 28 } }
                                (GrenSyntax.OperatorApplication "<|"
                                    (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 23 } }
                                        (GrenSyntax.OperatorApplication "&&"
                                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } } (GrenSyntax.FunctionOrValue [] "condition"))
                                            (GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 23 } } (GrenSyntax.FunctionOrValue [] "condition"))
                                        )
                                    )
                                    (GrenSyntax.Node { start = { row = 1, column = 27 }, end = { row = 1, column = 28 } } (GrenSyntax.FunctionOrValue [] "f"))
                                )
                            )
                )
            , Test.test "application expression 2"
                (\() ->
                    "(\"\", always (List.concat [ [ fileName ], [] ]))"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 48 } }
                                (GrenSyntax.TupledExpression
                                    [ GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } } (GrenSyntax.Literal "")
                                    , GrenSyntax.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 47 } }
                                        (GrenSyntax.Application
                                            [ GrenSyntax.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 12 } } (GrenSyntax.FunctionOrValue [] "always")
                                            , GrenSyntax.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 47 } }
                                                (GrenSyntax.ParenthesizedExpression
                                                    (GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 46 } }
                                                        (GrenSyntax.Application
                                                            [ GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } } (GrenSyntax.FunctionOrValue [ "List" ] "concat")
                                                            , GrenSyntax.Node { start = { row = 1, column = 26 }, end = { row = 1, column = 46 } }
                                                                (GrenSyntax.ListExpr
                                                                    [ GrenSyntax.Node { start = { row = 1, column = 28 }, end = { row = 1, column = 40 } }
                                                                        (GrenSyntax.ListExpr
                                                                            [ GrenSyntax.Node { start = { row = 1, column = 30 }, end = { row = 1, column = 38 } } (GrenSyntax.FunctionOrValue [] "fileName")
                                                                            ]
                                                                        )
                                                                    , GrenSyntax.Node { start = { row = 1, column = 42 }, end = { row = 1, column = 44 } } (GrenSyntax.ListExpr [])
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
                        |> expectSyntaxWithoutComments GrenParserLenient.expression (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (GrenSyntax.FunctionOrValue [] "foo"))
                )
            , Test.test "unit application"
                (\() ->
                    "Task.succeed ()"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 16 } }
                                (GrenSyntax.Application
                                    [ GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } } (GrenSyntax.FunctionOrValue [ "Task" ] "succeed")
                                    , GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 16 } } GrenSyntax.UnitExpr
                                    ]
                                )
                            )
                )
            , Test.test "Function call"
                (\() ->
                    "foo bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                                (GrenSyntax.Application
                                    [ GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (GrenSyntax.FunctionOrValue [] "foo")
                                    , GrenSyntax.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 8 } } (GrenSyntax.FunctionOrValue [] "bar")
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 26 } }
                                (GrenSyntax.IfBlock
                                    (GrenSyntax.Node { start = { row = 1, column = 4 }, end = { row = 1, column = 8 } } (GrenSyntax.FunctionOrValue [] "True"))
                                    (GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 17 } } (GrenSyntax.FunctionOrValue [] "foo"))
                                    (GrenSyntax.Node { start = { row = 1, column = 23 }, end = { row = 1, column = 26 } } (GrenSyntax.FunctionOrValue [] "bar"))
                                )
                            )
                )
            , Test.test "if-then-else with -> instead of then"
                (\() ->
                    "if True ->   foo else bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 26 } }
                                (GrenSyntax.IfBlock
                                    (GrenSyntax.Node { start = { row = 1, column = 4 }, end = { row = 1, column = 8 } } (GrenSyntax.FunctionOrValue [] "True"))
                                    (GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 17 } } (GrenSyntax.FunctionOrValue [] "foo"))
                                    (GrenSyntax.Node { start = { row = 1, column = 23 }, end = { row = 1, column = 26 } } (GrenSyntax.FunctionOrValue [] "bar"))
                                )
                            )
                )
            , Test.test "nestedIfExpression"
                (\() ->
                    "if True then if False then foo else baz else bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 49 } }
                                (GrenSyntax.IfBlock
                                    (GrenSyntax.Node { start = { row = 1, column = 4 }, end = { row = 1, column = 8 } } (GrenSyntax.FunctionOrValue [] "True"))
                                    (GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 40 } }
                                        (GrenSyntax.IfBlock
                                            (GrenSyntax.Node { start = { row = 1, column = 17 }, end = { row = 1, column = 22 } } (GrenSyntax.FunctionOrValue [] "False"))
                                            (GrenSyntax.Node { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } } (GrenSyntax.FunctionOrValue [] "foo"))
                                            (GrenSyntax.Node { start = { row = 1, column = 37 }, end = { row = 1, column = 40 } } (GrenSyntax.FunctionOrValue [] "baz"))
                                        )
                                    )
                                    (GrenSyntax.Node { start = { row = 1, column = 46 }, end = { row = 1, column = 49 } } (GrenSyntax.FunctionOrValue [] "bar"))
                                )
                            )
                )
            , Test.test "recordExpression"
                (\() ->
                    "{ model = 0, view = view, update = update }"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 44 } }
                                (GrenSyntax.RecordExpr
                                    [ GrenSyntax.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 12 } }
                                        ( GrenSyntax.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 8 } } "model"
                                        , GrenSyntax.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 12 } } (GrenSyntax.Integer 0)
                                        )
                                    , GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } }
                                        ( GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 18 } } "view"
                                        , GrenSyntax.Node { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } } (GrenSyntax.FunctionOrValue [] "view")
                                        )
                                    , GrenSyntax.Node { start = { row = 1, column = 27 }, end = { row = 1, column = 43 } }
                                        ( GrenSyntax.Node { start = { row = 1, column = 27 }, end = { row = 1, column = 33 } } "update"
                                        , GrenSyntax.Node { start = { row = 1, column = 36 }, end = { row = 1, column = 42 } } (GrenSyntax.FunctionOrValue [] "update")
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
                                GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 13 } }
                                    (GrenSyntax.RecordExpr
                                        [ GrenSyntax.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 10 } }
                                            ( GrenSyntax.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 6 } } "foo"
                                            , GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (GrenSyntax.Integer 1)
                                            )
                                        , GrenSyntax.Node { start = { row = 2, column = 4 }, end = { row = 2, column = 12 } }
                                            ( GrenSyntax.Node { start = { row = 2, column = 4 }, end = { row = 2, column = 7 } } "baz"
                                            , GrenSyntax.Node { start = { row = 2, column = 10 }, end = { row = 2, column = 11 } } (GrenSyntax.Integer 2)
                                            )
                                        ]
                                    )
                            , comments = [ GrenSyntax.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 17 } } "-- bar" ]
                            }
                )
            , Test.test "listExpression"
                (\() ->
                    "[ class \"a\", text \"Foo\"]"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                (GrenSyntax.ListExpr
                                    [ GrenSyntax.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 12 } }
                                        (GrenSyntax.Application
                                            [ GrenSyntax.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 8 } } (GrenSyntax.FunctionOrValue [] "class")
                                            , GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } (GrenSyntax.Literal "a")
                                            ]
                                        )
                                    , GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 24 } }
                                        (GrenSyntax.Application
                                            [ GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 18 } } (GrenSyntax.FunctionOrValue [] "text")
                                            , GrenSyntax.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 24 } } (GrenSyntax.Literal "Foo")
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                (GrenSyntax.ListExpr
                                    [ GrenSyntax.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 12 } }
                                        (GrenSyntax.Application
                                            [ GrenSyntax.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 8 } } (GrenSyntax.FunctionOrValue [] "class")
                                            , GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } (GrenSyntax.Literal "a")
                                            ]
                                        )
                                    , GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 24 } }
                                        (GrenSyntax.Application
                                            [ GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 18 } } (GrenSyntax.FunctionOrValue [] "text")
                                            , GrenSyntax.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 24 } } (GrenSyntax.Literal "Foo")
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
                                GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                                    (GrenSyntax.ListExpr
                                        [ GrenSyntax.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (GrenSyntax.Integer 1)
                                        ]
                                    )
                            , comments = [ GrenSyntax.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 13 } } "{- Foo-}" ]
                            }
                )
            , Test.test "list with extra prefix comma and comment"
                (\() ->
                    "[,1 {- Foo-} ]"
                        |> expectSyntaxWithComments GrenParserLenient.expression
                            { syntax =
                                GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                                    (GrenSyntax.ListExpr
                                        [ GrenSyntax.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (GrenSyntax.Integer 1)
                                        ]
                                    )
                            , comments = [ GrenSyntax.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 13 } } "{- Foo-}" ]
                            }
                )
            , Test.test "listExpression empty with comment"
                (\() ->
                    "[{- Foo -}]"
                        |> expectSyntaxWithComments GrenParserLenient.expression
                            { syntax = GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } (GrenSyntax.ListExpr [])
                            , comments = [ GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 11 } } "{- Foo -}" ]
                            }
                )
            , Test.test "qualified expression"
                (\() ->
                    "Html.text"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                                (GrenSyntax.FunctionOrValue [ "Html" ] "text")
                            )
                )
            , Test.test "qualified expression with name colliding with keyword"
                (\() ->
                    "Html.Attributes.type"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 21 } }
                                (GrenSyntax.FunctionOrValue [ "Html", "Attributes" ] "type_")
                            )
                )
            , Test.test "record access"
                (\() ->
                    "foo.bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                                (GrenSyntax.RecordAccess
                                    (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (GrenSyntax.FunctionOrValue [] "foo"))
                                    (GrenSyntax.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 8 } } "bar")
                                )
                            )
                )
            , Test.test "record access with field name colliding with keyword"
                (\() ->
                    "foo.exposing"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                                (GrenSyntax.RecordAccess
                                    (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (GrenSyntax.FunctionOrValue [] "foo"))
                                    (GrenSyntax.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 13 } } "exposing_")
                                )
                            )
                )
            , Test.test "multiple record access operations"
                (\() ->
                    "foo.bar.baz"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                (GrenSyntax.RecordAccess
                                    (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                                        (GrenSyntax.RecordAccess
                                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (GrenSyntax.FunctionOrValue [] "foo"))
                                            (GrenSyntax.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 8 } } "bar")
                                        )
                                    )
                                    (GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } "baz")
                                )
                            )
                )
            , Test.test "multiple record access operations with module name"
                (\() ->
                    "A.B.foo.bar.baz"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 16 } }
                                (GrenSyntax.RecordAccess
                                    (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                        (GrenSyntax.RecordAccess
                                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } } (GrenSyntax.FunctionOrValue [ "A", "B" ] "foo"))
                                            (GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } "bar")
                                        )
                                    )
                                    (GrenSyntax.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 16 } } "baz")
                                )
                            )
                )
            , Test.test "record with name-value separator : and name-value separator = and name-value separator empty and punned fields with each of those"
                (\() ->
                    "{ a 1, b = 2, c : 3, aPunned, bPunned =, cPunned : }"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { end = { column = 53, row = 1 }, start = { column = 1, row = 1 } }
                                (GrenSyntax.RecordExpr
                                    [ GrenSyntax.Node { end = { column = 6, row = 1 }, start = { column = 3, row = 1 } }
                                        ( GrenSyntax.Node { end = { column = 4, row = 1 }, start = { column = 3, row = 1 } } "a"
                                        , GrenSyntax.Node { end = { column = 6, row = 1 }, start = { column = 5, row = 1 } }
                                            (GrenSyntax.Integer 1)
                                        )
                                    , GrenSyntax.Node { end = { column = 13, row = 1 }, start = { column = 8, row = 1 } }
                                        ( GrenSyntax.Node { end = { column = 9, row = 1 }, start = { column = 8, row = 1 } } "b"
                                        , GrenSyntax.Node { end = { column = 13, row = 1 }, start = { column = 12, row = 1 } }
                                            (GrenSyntax.Integer 2)
                                        )
                                    , GrenSyntax.Node { end = { column = 20, row = 1 }, start = { column = 15, row = 1 } }
                                        ( GrenSyntax.Node { end = { column = 16, row = 1 }, start = { column = 15, row = 1 } } "c"
                                        , GrenSyntax.Node { end = { column = 20, row = 1 }, start = { column = 19, row = 1 } }
                                            (GrenSyntax.Integer 3)
                                        )
                                    , GrenSyntax.Node { end = { column = 29, row = 1 }, start = { column = 22, row = 1 } }
                                        ( GrenSyntax.Node { end = { column = 29, row = 1 }, start = { column = 22, row = 1 } } "aPunned"
                                        , GrenSyntax.Node { end = { column = 29, row = 1 }, start = { column = 29, row = 1 } }
                                            (GrenSyntax.FunctionOrValue [] "aPunned")
                                        )
                                    , GrenSyntax.Node { end = { column = 40, row = 1 }, start = { column = 31, row = 1 } }
                                        ( GrenSyntax.Node { end = { column = 38, row = 1 }, start = { column = 31, row = 1 } } "bPunned"
                                        , GrenSyntax.Node { end = { column = 38, row = 1 }, start = { column = 38, row = 1 } }
                                            (GrenSyntax.FunctionOrValue [] "bPunned")
                                        )
                                    , GrenSyntax.Node { end = { column = 52, row = 1 }, start = { column = 42, row = 1 } }
                                        ( GrenSyntax.Node { end = { column = 49, row = 1 }, start = { column = 42, row = 1 } } "cPunned"
                                        , GrenSyntax.Node { end = { column = 49, row = 1 }, start = { column = 49, row = 1 } }
                                            (GrenSyntax.FunctionOrValue [] "cPunned")
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "record with prefix comma"
                (\() ->
                    "{ , a = 1, b = 2 }"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { end = { column = 19, row = 1 }, start = { column = 1, row = 1 } }
                                (GrenSyntax.RecordExpr
                                    [ GrenSyntax.Node { end = { column = 10, row = 1 }, start = { column = 5, row = 1 } }
                                        ( GrenSyntax.Node { end = { column = 6, row = 1 }, start = { column = 5, row = 1 } } "a"
                                        , GrenSyntax.Node { end = { column = 10, row = 1 }, start = { column = 9, row = 1 } }
                                            (GrenSyntax.Integer 1)
                                        )
                                    , GrenSyntax.Node { end = { column = 18, row = 1 }, start = { column = 12, row = 1 } }
                                        ( GrenSyntax.Node { end = { column = 13, row = 1 }, start = { column = 12, row = 1 } } "b"
                                        , GrenSyntax.Node { end = { column = 17, row = 1 }, start = { column = 16, row = 1 } }
                                            (GrenSyntax.Integer 2)
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "record with extra comma between fields"
                (\() ->
                    "{   a = 1,,b = 2 }"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { end = { column = 19, row = 1 }, start = { column = 1, row = 1 } }
                                (GrenSyntax.RecordExpr
                                    [ GrenSyntax.Node { end = { column = 10, row = 1 }, start = { column = 5, row = 1 } }
                                        ( GrenSyntax.Node { end = { column = 6, row = 1 }, start = { column = 5, row = 1 } } "a"
                                        , GrenSyntax.Node { end = { column = 10, row = 1 }, start = { column = 9, row = 1 } }
                                            (GrenSyntax.Integer 1)
                                        )
                                    , GrenSyntax.Node { end = { column = 18, row = 1 }, start = { column = 12, row = 1 } }
                                        ( GrenSyntax.Node { end = { column = 13, row = 1 }, start = { column = 12, row = 1 } } "b"
                                        , GrenSyntax.Node { end = { column = 17, row = 1 }, start = { column = 16, row = 1 } }
                                            (GrenSyntax.Integer 2)
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "record update"
                (\() ->
                    "{ model | count = 1, loading = True }"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 38 } }
                                (GrenSyntax.RecordUpdateExpression
                                    (GrenSyntax.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 8 } } "model")
                                    [ GrenSyntax.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 20 } }
                                        ( GrenSyntax.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 16 } } "count"
                                        , GrenSyntax.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } } (GrenSyntax.Integer 1)
                                        )
                                    , GrenSyntax.Node { start = { row = 1, column = 22 }, end = { row = 1, column = 37 } }
                                        ( GrenSyntax.Node { start = { row = 1, column = 22 }, end = { row = 1, column = 29 } } "loading"
                                        , GrenSyntax.Node { start = { row = 1, column = 32 }, end = { row = 1, column = 36 } } (GrenSyntax.FunctionOrValue [] "True")
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "record update with extra comma between fields"
                (\() ->
                    "{ model | count = 1,,loading = True }"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 38 } }
                                (GrenSyntax.RecordUpdateExpression
                                    (GrenSyntax.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 8 } } "model")
                                    [ GrenSyntax.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 20 } }
                                        ( GrenSyntax.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 16 } } "count"
                                        , GrenSyntax.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } } (GrenSyntax.Integer 1)
                                        )
                                    , GrenSyntax.Node { start = { row = 1, column = 22 }, end = { row = 1, column = 37 } }
                                        ( GrenSyntax.Node { start = { row = 1, column = 22 }, end = { row = 1, column = 29 } } "loading"
                                        , GrenSyntax.Node { start = { row = 1, column = 32 }, end = { row = 1, column = 36 } } (GrenSyntax.FunctionOrValue [] "True")
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "record update with name-value separator : and name-value separator empty"
                (\() ->
                    "{ model | count : 1, loading   True }"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 38 } }
                                (GrenSyntax.RecordUpdateExpression
                                    (GrenSyntax.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 8 } } "model")
                                    [ GrenSyntax.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 20 } }
                                        ( GrenSyntax.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 16 } } "count"
                                        , GrenSyntax.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } } (GrenSyntax.Integer 1)
                                        )
                                    , GrenSyntax.Node { start = { row = 1, column = 22 }, end = { row = 1, column = 37 } }
                                        ( GrenSyntax.Node { start = { row = 1, column = 22 }, end = { row = 1, column = 29 } } "loading"
                                        , GrenSyntax.Node { start = { row = 1, column = 32 }, end = { row = 1, column = 36 } } (GrenSyntax.FunctionOrValue [] "True")
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "record update no spacing"
                (\() ->
                    "{model| count = 1, loading = True }"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 36 } }
                                (GrenSyntax.RecordUpdateExpression
                                    (GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } "model")
                                    [ GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 18 } }
                                        ( GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 14 } } "count"
                                        , GrenSyntax.Node { start = { row = 1, column = 17 }, end = { row = 1, column = 18 } } (GrenSyntax.Integer 1)
                                        )
                                    , GrenSyntax.Node { start = { row = 1, column = 20 }, end = { row = 1, column = 35 } }
                                        ( GrenSyntax.Node { start = { row = 1, column = 20 }, end = { row = 1, column = 27 } } "loading"
                                        , GrenSyntax.Node { start = { row = 1, column = 30 }, end = { row = 1, column = 34 } } (GrenSyntax.FunctionOrValue [] "True")
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "record access as function"
                (\() ->
                    "List.map .name people"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 22 } }
                                (GrenSyntax.Application
                                    [ GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 9 } } (GrenSyntax.FunctionOrValue [ "List" ] "map")
                                    , GrenSyntax.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } } (GrenSyntax.RecordAccessFunction ".name")
                                    , GrenSyntax.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 22 } } (GrenSyntax.FunctionOrValue [] "people")
                                    ]
                                )
                            )
                )
            , Test.test "record access direct"
                (\() ->
                    "(.spaceEvenly Internal.Style.classes)"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 38 } }
                                (GrenSyntax.ParenthesizedExpression
                                    (GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 37 } }
                                        (GrenSyntax.Application
                                            [ GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 14 } } (GrenSyntax.RecordAccessFunction ".spaceEvenly")
                                            , GrenSyntax.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 37 } } (GrenSyntax.FunctionOrValue [ "Internal", "Style" ] "classes")
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                                (GrenSyntax.Application
                                    [ GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } (GrenSyntax.PrefixOperator "::")
                                    , GrenSyntax.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } (GrenSyntax.FunctionOrValue [] "x")
                                    ]
                                )
                            )
                )
            , Test.test "subtraction without spaces"
                (\() ->
                    "2-1"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                (GrenSyntax.OperatorApplication "-"
                                    (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } (GrenSyntax.Integer 2))
                                    (GrenSyntax.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (GrenSyntax.Integer 1))
                                )
                            )
                )
            , Test.test "negated expression after comma"
                (\() ->
                    "a = (0,-x)"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                                (GrenSyntax.FunctionDeclaration
                                    { signature = Nothing
                                    , documentation = Nothing
                                    , declaration =
                                        GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                                            { name = GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } "a"
                                            , arguments = []
                                            , expression =
                                                GrenSyntax.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 11 } }
                                                    (GrenSyntax.TupledExpression
                                                        [ GrenSyntax.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } }
                                                            (GrenSyntax.Integer 0)
                                                        , GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 10 } }
                                                            (GrenSyntax.Negation
                                                                (GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } }
                                                                    (GrenSyntax.FunctionOrValue [] "x")
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                                (GrenSyntax.RecordExpr
                                    [ GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 6 } }
                                        ( GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } "a"
                                        , GrenSyntax.Node { start = { row = 1, column = 4 }, end = { row = 1, column = 6 } }
                                            (GrenSyntax.Negation
                                                (GrenSyntax.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } }
                                                    (GrenSyntax.FunctionOrValue [] "x")
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 17 } }
                                (GrenSyntax.OperatorApplication "-"
                                    (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                                        (GrenSyntax.Application
                                            [ GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 9 } }
                                                (GrenSyntax.FunctionOrValue [ "List" ] "sum")
                                            , GrenSyntax.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } }
                                                (GrenSyntax.ListExpr
                                                    [ GrenSyntax.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 12 } }
                                                        (GrenSyntax.FunctionOrValue [] "a")
                                                    , GrenSyntax.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 14 } }
                                                        (GrenSyntax.FunctionOrValue [] "b")
                                                    ]
                                                )
                                            ]
                                        )
                                    )
                                    (GrenSyntax.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 17 } }
                                        (GrenSyntax.FunctionOrValue [] "x")
                                    )
                                )
                            )
                )
            , Test.test "negated expression after = in let declaration"
                (\() ->
                    "let a=-x in a"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 14 } }
                                (GrenSyntax.LetExpression
                                    { declarations =
                                        [ GrenSyntax.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 9 } }
                                            (GrenSyntax.LetFunction
                                                { documentation = Nothing
                                                , signature = Nothing
                                                , declaration =
                                                    GrenSyntax.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 9 } }
                                                        { name = GrenSyntax.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } "a"
                                                        , arguments = []
                                                        , expression =
                                                            GrenSyntax.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 9 } }
                                                                (GrenSyntax.Negation
                                                                    (GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } }
                                                                        (GrenSyntax.FunctionOrValue [] "x")
                                                                    )
                                                                )
                                                        }
                                                }
                                            )
                                        ]
                                    , expression =
                                        GrenSyntax.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 14 } }
                                            (GrenSyntax.FunctionOrValue [] "a")
                                    }
                                )
                            )
                )
            , Test.test "negated expression after -> in case branch"
                (\() ->
                    "case 1 of\n    _->-a"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 10 } }
                                (GrenSyntax.CaseExpression
                                    { expression =
                                        GrenSyntax.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } }
                                            (GrenSyntax.Integer 1)
                                    , cases =
                                        [ ( GrenSyntax.Node { start = { row = 2, column = 5 }, end = { row = 2, column = 6 } }
                                                GrenSyntax.PatternIgnored
                                          , GrenSyntax.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 10 } }
                                                (GrenSyntax.Negation
                                                    (GrenSyntax.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } }
                                                        (GrenSyntax.FunctionOrValue [] "a")
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
                            (GrenSyntax.Node { end = { column = 14, row = 1 }, start = { column = 1, row = 1 } }
                                (GrenSyntax.LetExpression
                                    { declarations =
                                        [ GrenSyntax.Node { end = { column = 9, row = 1 }, start = { column = 5, row = 1 } }
                                            (GrenSyntax.LetFunction
                                                { declaration =
                                                    GrenSyntax.Node { end = { column = 9, row = 1 }, start = { column = 5, row = 1 } }
                                                        { arguments = []
                                                        , expression =
                                                            GrenSyntax.Node { end = { column = 9, row = 1 }, start = { column = 7, row = 1 } }
                                                                (GrenSyntax.Negation
                                                                    (GrenSyntax.Node { end = { column = 9, row = 1 }, start = { column = 8, row = 1 } }
                                                                        (GrenSyntax.FunctionOrValue [] "x")
                                                                    )
                                                                )
                                                        , name = GrenSyntax.Node { end = { column = 6, row = 1 }, start = { column = 5, row = 1 } } "a"
                                                        }
                                                , documentation = Nothing
                                                , signature = Nothing
                                                }
                                            )
                                        ]
                                    , expression =
                                        GrenSyntax.Node { end = { column = 14, row = 1 }, start = { column = 12, row = 1 } }
                                            (GrenSyntax.Negation
                                                (GrenSyntax.Node { end = { column = 14, row = 1 }, start = { column = 13, row = 1 } }
                                                    (GrenSyntax.FunctionOrValue [] "a")
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                                (GrenSyntax.FunctionDeclaration
                                    { signature = Nothing
                                    , documentation = Nothing
                                    , declaration =
                                        GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                                            { name = GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } "a"
                                            , arguments = []
                                            , expression =
                                                GrenSyntax.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 7 } }
                                                    (GrenSyntax.Negation
                                                        (GrenSyntax.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } }
                                                            (GrenSyntax.FunctionOrValue [] "x")
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                                (GrenSyntax.FunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                                            { name = GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "foo"
                                            , arguments = []
                                            , expression =
                                                GrenSyntax.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 7 } }
                                                    (GrenSyntax.Negation
                                                        (GrenSyntax.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } }
                                                            (GrenSyntax.Integer 1)
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                                (GrenSyntax.Application
                                    [ GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } } (GrenSyntax.FunctionOrValue [] "toFloat")
                                    , GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 11 } }
                                        (GrenSyntax.Negation (GrenSyntax.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 11 } } (GrenSyntax.Integer 5)))
                                    ]
                                )
                            )
                )
            , Test.test "negated expression for parenthesized"
                (\() ->
                    "a = -(x - y)"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                                (GrenSyntax.FunctionDeclaration
                                    { signature = Nothing
                                    , documentation = Nothing
                                    , declaration =
                                        GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                                            { name = GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } "a"
                                            , arguments = []
                                            , expression =
                                                GrenSyntax.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 13 } }
                                                    (GrenSyntax.Negation
                                                        (GrenSyntax.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 13 } }
                                                            (GrenSyntax.ParenthesizedExpression
                                                                (GrenSyntax.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 12 } }
                                                                    (GrenSyntax.OperatorApplication "-"
                                                                        (GrenSyntax.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 8 } }
                                                                            (GrenSyntax.FunctionOrValue [] "x")
                                                                        )
                                                                        (GrenSyntax.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 12 } }
                                                                            (GrenSyntax.FunctionOrValue [] "y")
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 33 } }
                                (GrenSyntax.FunctionDeclaration
                                    { signature = Nothing
                                    , documentation = Nothing
                                    , declaration =
                                        GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 33 } }
                                            { name = GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } "a"
                                            , arguments = []
                                            , expression =
                                                GrenSyntax.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 33 } }
                                                    (GrenSyntax.OperatorApplication "=="
                                                        (GrenSyntax.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 22 } }
                                                            (GrenSyntax.OperatorApplication "+"
                                                                (GrenSyntax.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 7 } }
                                                                    (GrenSyntax.Negation
                                                                        (GrenSyntax.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } }
                                                                            (GrenSyntax.Integer 1)
                                                                        )
                                                                    )
                                                                )
                                                                (GrenSyntax.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 22 } }
                                                                    (GrenSyntax.OperatorApplication "*"
                                                                        (GrenSyntax.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 13 } }
                                                                            (GrenSyntax.Negation
                                                                                (GrenSyntax.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 13 } }
                                                                                    (GrenSyntax.Integer 10)
                                                                                )
                                                                            )
                                                                        )
                                                                        (GrenSyntax.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 22 } }
                                                                            (GrenSyntax.OperatorApplication "^"
                                                                                (GrenSyntax.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 20 } }
                                                                                    (GrenSyntax.Negation
                                                                                        (GrenSyntax.Node { start = { row = 1, column = 17 }, end = { row = 1, column = 20 } }
                                                                                            (GrenSyntax.Integer 100)
                                                                                        )
                                                                                    )
                                                                                )
                                                                                (GrenSyntax.Node { start = { row = 1, column = 21 }, end = { row = 1, column = 22 } }
                                                                                    (GrenSyntax.Integer 2)
                                                                                )
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                        )
                                                        (GrenSyntax.Node { start = { row = 1, column = 26 }, end = { row = 1, column = 33 } }
                                                            (GrenSyntax.Negation
                                                                (GrenSyntax.Node { start = { row = 1, column = 27 }, end = { row = 1, column = 33 } }
                                                                    (GrenSyntax.Integer 100001)
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
                            (GrenSyntax.Node
                                { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                                (GrenSyntax.OperatorApplication "-"
                                    (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                                        (GrenSyntax.OperatorApplication "+"
                                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } (GrenSyntax.Integer 1))
                                            (GrenSyntax.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } (GrenSyntax.Integer 2))
                                        )
                                    )
                                    (GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (GrenSyntax.Integer 3))
                                )
                            )
                )
            , Test.test "pipe operation"
                (\() ->
                    "a |> b"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                                (GrenSyntax.OperatorApplication "|>"
                                    (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } (GrenSyntax.FunctionOrValue [] "a"))
                                    (GrenSyntax.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } (GrenSyntax.FunctionOrValue [] "b"))
                                )
                            )
                )
            , Test.test "function with higher order"
                (\() ->
                    "chompWhile (\\c -> c == ' ' || c == '\\n' || c == '\\r')"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 54 } }
                                (GrenSyntax.Application
                                    [ GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } } (GrenSyntax.FunctionOrValue [] "chompWhile")
                                    , GrenSyntax.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 54 } }
                                        (GrenSyntax.ParenthesizedExpression
                                            (GrenSyntax.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 53 } }
                                                (GrenSyntax.LambdaExpression
                                                    { args = [ GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } } (GrenSyntax.PatternVariable "c") ]
                                                    , expression =
                                                        GrenSyntax.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 53 } }
                                                            (GrenSyntax.OperatorApplication "||"
                                                                (GrenSyntax.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 27 } }
                                                                    (GrenSyntax.OperatorApplication "=="
                                                                        (GrenSyntax.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } } (GrenSyntax.FunctionOrValue [] "c"))
                                                                        (GrenSyntax.Node { start = { row = 1, column = 24 }, end = { row = 1, column = 27 } } (GrenSyntax.CharLiteral ' '))
                                                                    )
                                                                )
                                                                (GrenSyntax.Node { start = { row = 1, column = 31 }, end = { row = 1, column = 53 } }
                                                                    (GrenSyntax.OperatorApplication "||"
                                                                        (GrenSyntax.Node { start = { row = 1, column = 31 }, end = { row = 1, column = 40 } }
                                                                            (GrenSyntax.OperatorApplication "=="
                                                                                (GrenSyntax.Node { start = { row = 1, column = 31 }, end = { row = 1, column = 32 } } (GrenSyntax.FunctionOrValue [] "c"))
                                                                                (GrenSyntax.Node { start = { row = 1, column = 36 }, end = { row = 1, column = 40 } }
                                                                                    (GrenSyntax.CharLiteral '\n')
                                                                                )
                                                                            )
                                                                        )
                                                                        (GrenSyntax.Node { start = { row = 1, column = 44 }, end = { row = 1, column = 53 } }
                                                                            (GrenSyntax.OperatorApplication "=="
                                                                                (GrenSyntax.Node { start = { row = 1, column = 44 }, end = { row = 1, column = 45 } } (GrenSyntax.FunctionOrValue [] "c"))
                                                                                (GrenSyntax.Node { start = { row = 1, column = 49 }, end = { row = 1, column = 53 } }
                                                                                    (GrenSyntax.CharLiteral '\u{000D}')
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 24 } }
                                (GrenSyntax.Application
                                    [ GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (GrenSyntax.FunctionOrValue [] "foo")
                                    , GrenSyntax.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 24 } }
                                        (GrenSyntax.RecordAccess
                                            (GrenSyntax.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 22 } }
                                                (GrenSyntax.RecordUpdateExpression (GrenSyntax.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 8 } } "d")
                                                    [ GrenSyntax.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 21 } }
                                                        ( GrenSyntax.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 12 } } "b"
                                                        , GrenSyntax.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 20 } }
                                                            (GrenSyntax.Application
                                                                [ GrenSyntax.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 16 } } (GrenSyntax.FunctionOrValue [] "f")
                                                                , GrenSyntax.Node { start = { row = 1, column = 17 }, end = { row = 1, column = 18 } } (GrenSyntax.FunctionOrValue [] "x")
                                                                , GrenSyntax.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } } (GrenSyntax.FunctionOrValue [] "y")
                                                                ]
                                                            )
                                                        )
                                                    ]
                                                )
                                            )
                                            (GrenSyntax.Node { start = { row = 1, column = 23 }, end = { row = 1, column = 24 } } "b")
                                        )
                                    ]
                                )
                            )
                )
            , Test.test "should not consider a negative number parameter as the start of a new application"
                (\() ->
                    "Random.list -1 generator"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                (GrenSyntax.Application
                                    [ GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } (GrenSyntax.FunctionOrValue [ "Random" ] "list")
                                    , GrenSyntax.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 15 } }
                                        (GrenSyntax.Negation
                                            (GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } }
                                                (GrenSyntax.Integer 1)
                                            )
                                        )
                                    , GrenSyntax.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 25 } } (GrenSyntax.FunctionOrValue [] "generator")
                                    ]
                                )
                            )
                )
            , Test.test "negation can be applied on record access"
                (\() ->
                    "1 + -{x = 10}.x"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 16 } }
                                (GrenSyntax.OperatorApplication "+"
                                    (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } (GrenSyntax.Integer 1))
                                    (GrenSyntax.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 16 } }
                                        (GrenSyntax.Negation
                                            (GrenSyntax.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 16 } }
                                                (GrenSyntax.RecordAccess
                                                    (GrenSyntax.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 14 } }
                                                        (GrenSyntax.RecordExpr
                                                            [ GrenSyntax.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 13 } }
                                                                ( GrenSyntax.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 8 } } "x"
                                                                , GrenSyntax.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 13 } } (GrenSyntax.Integer 10)
                                                                )
                                                            ]
                                                        )
                                                    )
                                                    (GrenSyntax.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 16 } } "x")
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
                                (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                                    (GrenSyntax.LambdaExpression
                                        { args = [ GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } } GrenSyntax.PatternUnit ]
                                        , expression = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } (GrenSyntax.FunctionOrValue [] "foo")
                                        }
                                    )
                                )
                    )
                , Test.test "record lambda"
                    (\() ->
                        "\\{foo} -> foo"
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 14 } }
                                    (GrenSyntax.LambdaExpression
                                        { args = [ GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } (GrenSyntax.PatternRecord [ GrenSyntax.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 6 } } "foo" ]) ]
                                        , expression = GrenSyntax.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 14 } } (GrenSyntax.FunctionOrValue [] "foo")
                                        }
                                    )
                                )
                    )
                , Test.test "empty record lambda"
                    (\() ->
                        "\\{} -> foo"
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                                    (GrenSyntax.LambdaExpression
                                        { args = [ GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } } (GrenSyntax.PatternRecord []) ]
                                        , expression = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } (GrenSyntax.FunctionOrValue [] "foo")
                                        }
                                    )
                                )
                    )
                , Test.test "args lambda"
                    (\() ->
                        "\\a b -> a + b"
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 14 } }
                                    (GrenSyntax.LambdaExpression
                                        { args =
                                            [ GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (GrenSyntax.PatternVariable "a")
                                            , GrenSyntax.Node { start = { row = 1, column = 4 }, end = { row = 1, column = 5 } } (GrenSyntax.PatternVariable "b")
                                            ]
                                        , expression =
                                            GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 14 } }
                                                (GrenSyntax.OperatorApplication "+"
                                                    (GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (GrenSyntax.FunctionOrValue [] "a"))
                                                    (GrenSyntax.Node { start = { row = 1, column = 13 }, end = { row = 1, column = 14 } } (GrenSyntax.FunctionOrValue [] "b"))
                                                )
                                        }
                                    )
                                )
                    )
                , Test.test "tuple lambda"
                    (\() ->
                        "\\(a,b) -> a + b"
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 16 } }
                                    (GrenSyntax.LambdaExpression
                                        { args =
                                            [ GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } }
                                                (GrenSyntax.PatternTuple
                                                    [ GrenSyntax.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (GrenSyntax.PatternVariable "a")
                                                    , GrenSyntax.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } (GrenSyntax.PatternVariable "b")
                                                    ]
                                                )
                                            ]
                                        , expression =
                                            GrenSyntax.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 16 } }
                                                (GrenSyntax.OperatorApplication "+"
                                                    (GrenSyntax.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 12 } } (GrenSyntax.FunctionOrValue [] "a"))
                                                    (GrenSyntax.Node { start = { row = 1, column = 15 }, end = { row = 1, column = 16 } } (GrenSyntax.FunctionOrValue [] "b"))
                                                )
                                        }
                                    )
                                )
                    )
                , Test.test "lambda with => instead of ->"
                    (\() ->
                        "\\() => foo"
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                                    (GrenSyntax.LambdaExpression
                                        { args = [ GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } } GrenSyntax.PatternUnit ]
                                        , expression = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } (GrenSyntax.FunctionOrValue [] "foo")
                                        }
                                    )
                                )
                    )
                , Test.test "lambda with . instead of ->"
                    (\() ->
                        "\\().   foo"
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                                    (GrenSyntax.LambdaExpression
                                        { args = [ GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } } GrenSyntax.PatternUnit ]
                                        , expression = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } (GrenSyntax.FunctionOrValue [] "foo")
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
                                (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 4, column = 18 } }
                                    (GrenSyntax.LetExpression
                                        { declarations =
                                            [ GrenSyntax.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 12 } }
                                                (GrenSyntax.LetFunction
                                                    { documentation = Nothing
                                                    , signature = Nothing
                                                    , declaration =
                                                        GrenSyntax.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 12 } }
                                                            { name = GrenSyntax.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 6 } } "foo"
                                                            , arguments = []
                                                            , expression = GrenSyntax.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 12 } } (GrenSyntax.FunctionOrValue [] "bar")
                                                            }
                                                    }
                                                )
                                            , GrenSyntax.Node { start = { row = 4, column = 3 }, end = { row = 4, column = 13 } }
                                                (GrenSyntax.LetFunction
                                                    { documentation = Nothing
                                                    , signature = Nothing
                                                    , declaration =
                                                        GrenSyntax.Node { start = { row = 4, column = 3 }, end = { row = 4, column = 13 } }
                                                            { name = GrenSyntax.Node { start = { row = 4, column = 3 }, end = { row = 4, column = 7 } } "john"
                                                            , arguments = [ GrenSyntax.Node { start = { row = 4, column = 8 }, end = { row = 4, column = 9 } } (GrenSyntax.PatternVariable "n") ]
                                                            , expression = GrenSyntax.Node { start = { row = 4, column = 12 }, end = { row = 4, column = 13 } } (GrenSyntax.FunctionOrValue [] "n")
                                                            }
                                                    }
                                                )
                                            ]
                                        , expression = GrenSyntax.Node { start = { row = 4, column = 17 }, end = { row = 4, column = 18 } } (GrenSyntax.Integer 1)
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
                                (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 4, column = 6 } }
                                    (GrenSyntax.LetExpression
                                        { declarations =
                                            [ GrenSyntax.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } }
                                                (GrenSyntax.LetFunction
                                                    { documentation = Nothing
                                                    , signature = Nothing
                                                    , declaration =
                                                        GrenSyntax.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } }
                                                            { name = GrenSyntax.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 6 } } "bar"
                                                            , arguments = []
                                                            , expression = GrenSyntax.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } } (GrenSyntax.Integer 1)
                                                            }
                                                    }
                                                )
                                            ]
                                        , expression = GrenSyntax.Node { start = { row = 4, column = 3 }, end = { row = 4, column = 6 } } (GrenSyntax.FunctionOrValue [] "bar")
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
                                (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 4, column = 7 } }
                                    (GrenSyntax.LetExpression
                                        { declarations =
                                            [ GrenSyntax.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } }
                                                (GrenSyntax.LetFunction
                                                    { documentation = Nothing
                                                    , signature = Nothing
                                                    , declaration =
                                                        GrenSyntax.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } }
                                                            { name = GrenSyntax.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 6 } } "bar"
                                                            , arguments = []
                                                            , expression = GrenSyntax.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } } (GrenSyntax.Integer 1)
                                                            }
                                                    }
                                                )
                                            ]
                                        , expression = GrenSyntax.Node { start = { row = 4, column = 4 }, end = { row = 4, column = 7 } } (GrenSyntax.FunctionOrValue [] "bar")
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
                                (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 5, column = 6 } }
                                    (GrenSyntax.LetExpression
                                        { declarations =
                                            [ GrenSyntax.Node { start = { row = 2, column = 5 }, end = { row = 3, column = 12 } }
                                                (GrenSyntax.LetFunction
                                                    { documentation = Nothing
                                                    , signature =
                                                        Just
                                                            (GrenSyntax.Node { start = { row = 2, column = 5 }, end = { row = 2, column = 14 } }
                                                                { name = GrenSyntax.Node { start = { row = 2, column = 5 }, end = { row = 2, column = 8 } } "bar"
                                                                , typeAnnotation =
                                                                    GrenSyntax.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 14 } }
                                                                        (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 14 } } ( [], "Int" )) [])
                                                                }
                                                            )
                                                    , declaration =
                                                        GrenSyntax.Node { start = { row = 3, column = 5 }, end = { row = 3, column = 12 } }
                                                            { name = GrenSyntax.Node { start = { row = 3, column = 5 }, end = { row = 3, column = 8 } } "bar"
                                                            , arguments = []
                                                            , expression = GrenSyntax.Node { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } } (GrenSyntax.Integer 1)
                                                            }
                                                    }
                                                )
                                            ]
                                        , expression = GrenSyntax.Node { start = { row = 5, column = 3 }, end = { row = 5, column = 6 } } (GrenSyntax.FunctionOrValue [] "bar")
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
                                (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 7, column = 6 } }
                                    (GrenSyntax.LetExpression
                                        { declarations =
                                            [ GrenSyntax.Node { start = { row = 2, column = 5 }, end = { row = 5, column = 12 } }
                                                (GrenSyntax.LetFunction
                                                    { documentation = Nothing
                                                    , signature =
                                                        Just
                                                            (GrenSyntax.Node { start = { row = 2, column = 5 }, end = { row = 2, column = 14 } }
                                                                { name = GrenSyntax.Node { start = { row = 2, column = 5 }, end = { row = 2, column = 8 } } "bar"
                                                                , typeAnnotation = GrenSyntax.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 14 } } (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 14 } } ( [], "Int" )) [])
                                                                }
                                                            )
                                                    , declaration =
                                                        GrenSyntax.Node { start = { row = 5, column = 5 }, end = { row = 5, column = 12 } }
                                                            { name = GrenSyntax.Node { start = { row = 5, column = 5 }, end = { row = 5, column = 8 } } "bar"
                                                            , arguments = []
                                                            , expression = GrenSyntax.Node { start = { row = 5, column = 11 }, end = { row = 5, column = 12 } } (GrenSyntax.Integer 1)
                                                            }
                                                    }
                                                )
                                            ]
                                        , expression = GrenSyntax.Node { start = { row = 7, column = 3 }, end = { row = 7, column = 6 } } (GrenSyntax.FunctionOrValue [] "bar")
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
                                (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 5, column = 6 } }
                                    (GrenSyntax.LetExpression
                                        { declarations =
                                            [ GrenSyntax.Node { start = { row = 2, column = 5 }, end = { row = 3, column = 12 } }
                                                (GrenSyntax.LetFunction
                                                    { documentation = Nothing
                                                    , signature =
                                                        Just
                                                            (GrenSyntax.Node { start = { row = 2, column = 5 }, end = { row = 2, column = 10 } }
                                                                { name = GrenSyntax.Node { start = { row = 2, column = 5 }, end = { row = 2, column = 5 } } "bar"
                                                                , typeAnnotation =
                                                                    GrenSyntax.Node { start = { row = 2, column = 7 }, end = { row = 2, column = 10 } }
                                                                        (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 2, column = 7 }, end = { row = 2, column = 10 } } ( [], "Int" )) [])
                                                                }
                                                            )
                                                    , declaration =
                                                        GrenSyntax.Node { start = { row = 3, column = 5 }, end = { row = 3, column = 12 } }
                                                            { name = GrenSyntax.Node { start = { row = 3, column = 5 }, end = { row = 3, column = 8 } } "bar"
                                                            , arguments = []
                                                            , expression = GrenSyntax.Node { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } } (GrenSyntax.Integer 1)
                                                            }
                                                    }
                                                )
                                            ]
                                        , expression = GrenSyntax.Node { start = { row = 5, column = 3 }, end = { row = 5, column = 6 } } (GrenSyntax.FunctionOrValue [] "bar")
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
                                (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 4, column = 12 } }
                                    (GrenSyntax.LetExpression
                                        { declarations =
                                            [ GrenSyntax.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 24 } }
                                                (GrenSyntax.LetFunction
                                                    { documentation = Nothing
                                                    , signature = Nothing
                                                    , declaration =
                                                        GrenSyntax.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 24 } }
                                                            { name = GrenSyntax.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 14 } } "bar"
                                                            , arguments =
                                                                [ GrenSyntax.Node { start = { row = 2, column = 15 }, end = { row = 2, column = 18 } } (GrenSyntax.PatternVariant { moduleName = [], name = "Bar" } [])
                                                                , GrenSyntax.Node { start = { row = 2, column = 19 }, end = { row = 2, column = 20 } } (GrenSyntax.PatternVariable "m")
                                                                ]
                                                            , expression = GrenSyntax.Node { start = { row = 2, column = 23 }, end = { row = 2, column = 24 } } (GrenSyntax.Integer 1)
                                                            }
                                                    }
                                                )
                                            ]
                                        , expression = GrenSyntax.Node { start = { row = 4, column = 9 }, end = { row = 4, column = 12 } } (GrenSyntax.FunctionOrValue [] "bar")
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
                                (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 7, column = 6 } }
                                    (GrenSyntax.LetExpression
                                        { declarations =
                                            [ GrenSyntax.Node { start = { row = 2, column = 5 }, end = { row = 2, column = 10 } }
                                                (GrenSyntax.LetDestructuring (GrenSyntax.Node { start = { row = 2, column = 5 }, end = { row = 2, column = 6 } } GrenSyntax.PatternIgnored) (GrenSyntax.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } } (GrenSyntax.FunctionOrValue [] "b")))
                                            , GrenSyntax.Node { start = { row = 3, column = 5 }, end = { row = 3, column = 12 } }
                                                (GrenSyntax.LetDestructuring
                                                    (GrenSyntax.Node { start = { row = 3, column = 5 }, end = { row = 3, column = 8 } }
                                                        (GrenSyntax.PatternRecord [ GrenSyntax.Node { start = { row = 3, column = 6 }, end = { row = 3, column = 7 } } "a" ])
                                                    )
                                                    (GrenSyntax.Node { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } } (GrenSyntax.FunctionOrValue [] "b"))
                                                )
                                            , GrenSyntax.Node { start = { row = 4, column = 5 }, end = { row = 4, column = 15 } }
                                                (GrenSyntax.LetDestructuring
                                                    (GrenSyntax.Node { start = { row = 4, column = 5 }, end = { row = 4, column = 11 } }
                                                        (GrenSyntax.PatternTuple
                                                            [ GrenSyntax.Node { start = { row = 4, column = 6 }, end = { row = 4, column = 7 } } (GrenSyntax.PatternVariable "c")
                                                            , GrenSyntax.Node { start = { row = 4, column = 9 }, end = { row = 4, column = 10 } } (GrenSyntax.PatternVariable "d")
                                                            ]
                                                        )
                                                    )
                                                    (GrenSyntax.Node { start = { row = 4, column = 14 }, end = { row = 4, column = 15 } } (GrenSyntax.FunctionOrValue [] "e"))
                                                )
                                            , GrenSyntax.Node { start = { row = 5, column = 5 }, end = { row = 5, column = 19 } }
                                                (GrenSyntax.LetDestructuring
                                                    (GrenSyntax.Node { start = { row = 5, column = 5 }, end = { row = 5, column = 15 } }
                                                        (GrenSyntax.PatternParenthesized
                                                            (GrenSyntax.Node { start = { row = 5, column = 6 }, end = { row = 5, column = 14 } } (GrenSyntax.PatternVariant { moduleName = [], name = "Node" } [ GrenSyntax.Node { start = { row = 5, column = 11 }, end = { row = 5, column = 12 } } GrenSyntax.PatternIgnored, GrenSyntax.Node { start = { row = 5, column = 13 }, end = { row = 5, column = 14 } } (GrenSyntax.PatternVariable "f") ]))
                                                        )
                                                    )
                                                    (GrenSyntax.Node { start = { row = 5, column = 18 }, end = { row = 5, column = 19 } } (GrenSyntax.FunctionOrValue [] "g"))
                                                )
                                            ]
                                        , expression = GrenSyntax.Node { start = { row = 7, column = 5 }, end = { row = 7, column = 6 } } (GrenSyntax.Integer 1)
                                        }
                                    )
                                )
                    )
                , Test.test "On one line"
                    (\() ->
                        "let indent = String.length s in indent"
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 39 } }
                                    (GrenSyntax.LetExpression
                                        { declarations =
                                            [ GrenSyntax.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 29 } }
                                                (GrenSyntax.LetFunction
                                                    { documentation = Nothing
                                                    , signature = Nothing
                                                    , declaration =
                                                        GrenSyntax.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 29 } }
                                                            { name = GrenSyntax.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 11 } } "indent"
                                                            , arguments = []
                                                            , expression =
                                                                GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 29 } }
                                                                    (GrenSyntax.Application
                                                                        [ GrenSyntax.Node { start = { row = 1, column = 14 }, end = { row = 1, column = 27 } } (GrenSyntax.FunctionOrValue [ "String" ] "length")
                                                                        , GrenSyntax.Node { start = { row = 1, column = 28 }, end = { row = 1, column = 29 } } (GrenSyntax.FunctionOrValue [] "s")
                                                                        ]
                                                                    )
                                                            }
                                                    }
                                                )
                                            ]
                                        , expression = GrenSyntax.Node { start = { row = 1, column = 33 }, end = { row = 1, column = 39 } } (GrenSyntax.FunctionOrValue [] "indent")
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
                                (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 3, column = 9 } }
                                    (GrenSyntax.LetExpression
                                        { declarations =
                                            [ GrenSyntax.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 14 } }
                                                (GrenSyntax.LetFunction
                                                    { documentation = Nothing
                                                    , signature = Nothing
                                                    , declaration =
                                                        GrenSyntax.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 14 } }
                                                            { name = GrenSyntax.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } } "a"
                                                            , arguments = []
                                                            , expression = GrenSyntax.Node { start = { row = 2, column = 13 }, end = { row = 2, column = 14 } } (GrenSyntax.Integer 1)
                                                            }
                                                    }
                                                )
                                            ]
                                        , expression = GrenSyntax.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 9 } } (GrenSyntax.ListExpr [])
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
                                (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 3, column = 9 } }
                                    (GrenSyntax.LetExpression
                                        { declarations =
                                            [ GrenSyntax.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 14 } }
                                                (GrenSyntax.LetFunction
                                                    { documentation = Nothing
                                                    , signature = Nothing
                                                    , declaration =
                                                        GrenSyntax.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 14 } }
                                                            { name = GrenSyntax.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } } "a"
                                                            , arguments = []
                                                            , expression = GrenSyntax.Node { start = { row = 2, column = 13 }, end = { row = 2, column = 14 } } (GrenSyntax.Integer 1)
                                                            }
                                                    }
                                                )
                                            ]
                                        , expression = GrenSyntax.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 9 } } (GrenSyntax.RecordExpr [])
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
                                (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 3, column = 14 } }
                                    (GrenSyntax.LetExpression
                                        { declarations =
                                            [ GrenSyntax.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 14 } }
                                                (GrenSyntax.LetFunction
                                                    { documentation = Nothing
                                                    , signature = Nothing
                                                    , declaration =
                                                        GrenSyntax.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 14 } }
                                                            { name = GrenSyntax.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } } "a"
                                                            , arguments = []
                                                            , expression = GrenSyntax.Node { start = { row = 2, column = 13 }, end = { row = 2, column = 14 } } (GrenSyntax.Integer 1)
                                                            }
                                                    }
                                                )
                                            ]
                                        , expression =
                                            GrenSyntax.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 14 } }
                                                (GrenSyntax.LambdaExpression { args = [ GrenSyntax.Node { start = { row = 3, column = 8 }, end = { row = 3, column = 9 } } GrenSyntax.PatternIgnored ], expression = GrenSyntax.Node { start = { row = 3, column = 13 }, end = { row = 3, column = 14 } } (GrenSyntax.Integer 1) })
                                        }
                                    )
                                )
                    )
                , Test.test "let is not confused by a variable name starting with let"
                    (\() ->
                        "letterbox"
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } } (GrenSyntax.FunctionOrValue [] "letterbox"))
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
                                (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 3, column = 13 } }
                                    (GrenSyntax.CaseExpression
                                        { expression =
                                            GrenSyntax.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } (GrenSyntax.FunctionOrValue [] "f")
                                        , cases =
                                            [ ( GrenSyntax.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 7 } } (GrenSyntax.PatternVariant { moduleName = [], name = "True" } [])
                                              , GrenSyntax.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 12 } } (GrenSyntax.Integer 1)
                                              )
                                            , ( GrenSyntax.Node { start = { row = 3, column = 3 }, end = { row = 3, column = 8 } } (GrenSyntax.PatternVariant { moduleName = [], name = "False" } [])
                                              , GrenSyntax.Node { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } } (GrenSyntax.Integer 2)
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
                                (GrenSyntax.Node { end = { column = 11, row = 2 }, start = { column = 1, row = 1 } }
                                    (GrenSyntax.CaseExpression
                                        { cases =
                                            [ ( GrenSyntax.Node { end = { column = 6, row = 2 }, start = { column = 5, row = 2 } }
                                                    GrenSyntax.PatternIgnored
                                              , GrenSyntax.Node { end = { column = 11, row = 2 }, start = { column = 10, row = 2 } }
                                                    (GrenSyntax.Integer 1)
                                              )
                                            ]
                                        , expression =
                                            GrenSyntax.Node { end = { column = 8, row = 1 }, start = { column = 6, row = 1 } }
                                                (GrenSyntax.ListExpr [])
                                        }
                                    )
                                )
                    )
                , Test.test "allow no symbol at all between case pattern and result"
                    (\() ->
                        """case [] of
    _    1"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                (GrenSyntax.Node { end = { column = 11, row = 2 }, start = { column = 1, row = 1 } }
                                    (GrenSyntax.CaseExpression
                                        { cases =
                                            [ ( GrenSyntax.Node { end = { column = 6, row = 2 }, start = { column = 5, row = 2 } }
                                                    GrenSyntax.PatternIgnored
                                              , GrenSyntax.Node { end = { column = 11, row = 2 }, start = { column = 10, row = 2 } }
                                                    (GrenSyntax.Integer 1)
                                              )
                                            ]
                                        , expression =
                                            GrenSyntax.Node { end = { column = 8, row = 1 }, start = { column = 6, row = 1 } }
                                                (GrenSyntax.ListExpr [])
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
                                (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 3, column = 13 } }
                                    (GrenSyntax.CaseExpression
                                        { expression =
                                            GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } }
                                                (GrenSyntax.FunctionOrValue [] "f")
                                        , cases =
                                            [ ( GrenSyntax.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 7 } }
                                                    (GrenSyntax.PatternVariant { moduleName = [], name = "True" } [])
                                              , GrenSyntax.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 12 } }
                                                    (GrenSyntax.Integer 1)
                                              )
                                            , ( GrenSyntax.Node { start = { row = 3, column = 3 }, end = { row = 3, column = 8 } }
                                                    (GrenSyntax.PatternVariant { moduleName = [], name = "False" } [])
                                              , GrenSyntax.Node { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } }
                                                    (GrenSyntax.Integer 2)
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
                                (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 3, column = 13 } }
                                    (GrenSyntax.CaseExpression
                                        { expression =
                                            GrenSyntax.Node { end = { column = 7, row = 1 }, start = { column = 1, row = 1 } }
                                                (GrenSyntax.OperatorApplication "|>"
                                                    (GrenSyntax.Node { end = { column = 2, row = 1 }, start = { column = 1, row = 1 } }
                                                        (GrenSyntax.FunctionOrValue [] "f")
                                                    )
                                                    (GrenSyntax.Node { end = { column = 7, row = 1 }, start = { column = 6, row = 1 } }
                                                        (GrenSyntax.FunctionOrValue [] "g")
                                                    )
                                                )
                                        , cases =
                                            [ ( GrenSyntax.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 7 } }
                                                    (GrenSyntax.PatternVariant { moduleName = [], name = "True" } [])
                                              , GrenSyntax.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 12 } }
                                                    (GrenSyntax.Integer 1)
                                              )
                                            , ( GrenSyntax.Node { start = { row = 3, column = 3 }, end = { row = 3, column = 8 } }
                                                    (GrenSyntax.PatternVariant { moduleName = [], name = "False" } [])
                                              , GrenSyntax.Node { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } }
                                                    (GrenSyntax.Integer 2)
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
                                (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 15 } }
                                    (GrenSyntax.CaseExpression
                                        { expression =
                                            GrenSyntax.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } (GrenSyntax.FunctionOrValue [] "f")
                                        , cases =
                                            [ ( GrenSyntax.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } } (GrenSyntax.PatternVariant { moduleName = [ "Foo" ], name = "Bar" } [])
                                              , GrenSyntax.Node { start = { row = 2, column = 14 }, end = { row = 2, column = 15 } } (GrenSyntax.Integer 1)
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
                                (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 7 } }
                                    (GrenSyntax.CaseExpression
                                        { expression =
                                            GrenSyntax.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } (GrenSyntax.FunctionOrValue [] "f")
                                        , cases =
                                            [ ( GrenSyntax.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 4 } } (GrenSyntax.PatternVariable "x")
                                              , GrenSyntax.Node { start = { row = 2, column = 6 }, end = { row = 2, column = 7 } } (GrenSyntax.Integer 1)
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
                                (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 2, column = 21 } }
                                    (GrenSyntax.CaseExpression
                                        { expression = GrenSyntax.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } (GrenSyntax.FunctionOrValue [] "x")
                                        , cases =
                                            [ ( GrenSyntax.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 15 } } (GrenSyntax.PatternVariant { moduleName = [], name = "True" } [])
                                              , GrenSyntax.Node { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } } (GrenSyntax.Integer 1)
                                              )
                                            , ( GrenSyntax.Node { start = { row = 2, column = 11 }, end = { row = 2, column = 16 } } (GrenSyntax.PatternVariant { moduleName = [], name = "False" } [])
                                              , GrenSyntax.Node { start = { row = 2, column = 20 }, end = { row = 2, column = 21 } } (GrenSyntax.Integer 2)
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
                                (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 7, column = 15 } }
                                    (GrenSyntax.CaseExpression
                                        { expression =
                                            GrenSyntax.Node
                                                { start = { row = 1, column = 6 }
                                                , end = { row = 1, column = 7 }
                                                }
                                                (GrenSyntax.FunctionOrValue [] "x")
                                        , cases =
                                            [ ( GrenSyntax.Node { start = { row = 2, column = 9 }, end = { row = 2, column = 39 } } (GrenSyntax.PatternString "single line triple quote")
                                              , GrenSyntax.Node { start = { row = 3, column = 13 }, end = { row = 3, column = 14 } } (GrenSyntax.Integer 1)
                                              )
                                            , ( GrenSyntax.Node { start = { row = 4, column = 9 }, end = { row = 5, column = 28 } } (GrenSyntax.PatternString "multi line\n            triple quote")
                                              , GrenSyntax.Node { start = { row = 6, column = 13 }, end = { row = 6, column = 14 } } (GrenSyntax.Integer 2)
                                              )
                                            , ( GrenSyntax.Node { start = { row = 7, column = 9 }, end = { row = 7, column = 10 } } GrenSyntax.PatternIgnored
                                              , GrenSyntax.Node { start = { row = 7, column = 14 }, end = { row = 7, column = 15 } } (GrenSyntax.Integer 3)
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
                                (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 6, column = 6 } }
                                    (GrenSyntax.CaseExpression
                                        { expression = GrenSyntax.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 9 } } (GrenSyntax.FunctionOrValue [] "msg")
                                        , cases =
                                            [ ( GrenSyntax.Node { start = { row = 2, column = 3 }, end = { row = 2, column = 12 } } (GrenSyntax.PatternVariant { moduleName = [], name = "Increment" } [])
                                              , GrenSyntax.Node { start = { row = 3, column = 5 }, end = { row = 3, column = 6 } } (GrenSyntax.Integer 1)
                                              )
                                            , ( GrenSyntax.Node { start = { row = 5, column = 3 }, end = { row = 5, column = 12 } } (GrenSyntax.PatternVariant { moduleName = [], name = "Decrement" } [])
                                              , GrenSyntax.Node { start = { row = 6, column = 5 }, end = { row = 6, column = 6 } } (GrenSyntax.Integer 2)
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 37 } }
                                (GrenSyntax.GLSLExpression " precision mediump float; ")
                            )
                )
            ]
        , Test.describe "pattern"
            [ Test.test "Unit"
                (\() ->
                    "()"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } GrenSyntax.PatternUnit)
                )
            , Test.test "Unit with inner layout"
                (\() ->
                    """(
   -- comment
   )"""
                        |> expectSyntaxWithComments GrenParserLenient.pattern
                            { syntax = GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 3, column = 5 } } GrenSyntax.PatternUnit
                            , comments = [ GrenSyntax.Node { start = { row = 2, column = 4 }, end = { row = 2, column = 14 } } "-- comment" ]
                            }
                )
            , Test.test "String"
                (\() ->
                    "\"Foo\""
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } (GrenSyntax.PatternString "Foo"))
                )
            , Test.test "Char"
                (\() ->
                    "'f'"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (GrenSyntax.PatternChar 'f'))
                )
            , Test.test "Wildcard"
                (\() ->
                    "_"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } GrenSyntax.PatternIgnored)
                )
            , Test.test "Parenthesized"
                (\() ->
                    "(x)"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                (GrenSyntax.PatternParenthesized
                                    (GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (GrenSyntax.PatternVariable "x"))
                                )
                            )
                )
            , Test.test "Int"
                (\() ->
                    "1"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } (GrenSyntax.PatternInt 1))
                )
            , Test.test "Hex int"
                (\() ->
                    "0x1"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (GrenSyntax.PatternHex 1))
                )
            , Test.test "Float should not be valid" (\() -> expectFailsToParse GrenParserLenient.pattern "1.0")
            , Test.test "Uncons"
                (\() ->
                    "n :: tail"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                                (GrenSyntax.PatternListCons (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } (GrenSyntax.PatternVariable "n"))
                                    (GrenSyntax.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 10 } } (GrenSyntax.PatternVariable "tail"))
                                )
                            )
                )
            , Test.test "Uncons multiple"
                (\() ->
                    "a :: b :: cUp"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 14 } }
                                (GrenSyntax.PatternListCons
                                    (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } (GrenSyntax.PatternVariable "a"))
                                    (GrenSyntax.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 14 } }
                                        (GrenSyntax.PatternListCons (GrenSyntax.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } (GrenSyntax.PatternVariable "b"))
                                            (GrenSyntax.Node { start = { row = 1, column = 11 }, end = { row = 1, column = 14 } } (GrenSyntax.PatternVariable "cUp"))
                                        )
                                    )
                                )
                            )
                )
            , Test.test "Uncons with parens"
                (\() ->
                    "(X x) :: xs"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                (GrenSyntax.PatternListCons
                                    (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                                        (GrenSyntax.PatternParenthesized
                                            (GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 5 } }
                                                (GrenSyntax.PatternVariant { moduleName = [], name = "X" }
                                                    [ GrenSyntax.Node { start = { row = 1, column = 4 }, end = { row = 1, column = 5 } } (GrenSyntax.PatternVariable "x") ]
                                                )
                                            )
                                        )
                                    )
                                    (GrenSyntax.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 12 } } (GrenSyntax.PatternVariable "xs"))
                                )
                            )
                )
            , Test.test "Empty list"
                (\() ->
                    "[]"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } (GrenSyntax.PatternListExact []))
                )
            , Test.test "Empty list pattern with whitespace"
                (\() ->
                    "[ ]"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (GrenSyntax.PatternListExact []))
                )
            , Test.test "Single element list"
                (\() ->
                    "[1]"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (GrenSyntax.PatternListExact [ GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (GrenSyntax.PatternInt 1) ]))
                )
            , Test.test "Single element list with trailing whitespace"
                (\() ->
                    "[1 ]"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } (GrenSyntax.PatternListExact [ GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (GrenSyntax.PatternInt 1) ]))
                )
            , Test.test "Single element list with leading whitespace"
                (\() ->
                    "[ 1]"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } (GrenSyntax.PatternListExact [ GrenSyntax.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (GrenSyntax.PatternInt 1) ]))
                )
            , Test.test "list with prefix extra comma"
                (\() ->
                    "[,1]"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } (GrenSyntax.PatternListExact [ GrenSyntax.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (GrenSyntax.PatternInt 1) ]))
                )
            , Test.test "list with extra comma between elements"
                (\() ->
                    "[1,,2]"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                                (GrenSyntax.PatternListExact
                                    [ GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } }
                                        (GrenSyntax.PatternInt 1)
                                    , GrenSyntax.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } }
                                        (GrenSyntax.PatternInt 2)
                                    ]
                                )
                            )
                )
            , Test.test "Empty record"
                (\() ->
                    "{}"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } (GrenSyntax.PatternRecord []))
                )
            , Test.test "Empty record with whitespace"
                (\() ->
                    "{ }"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (GrenSyntax.PatternRecord []))
                )
            , Test.test "Record"
                (\() ->
                    "{a,b}"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                                (GrenSyntax.PatternRecord
                                    [ GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } "a"
                                    , GrenSyntax.Node { start = { row = 1, column = 4 }, end = { row = 1, column = 5 } } "b"
                                    ]
                                )
                            )
                )
            , Test.test "Record pattern with whitespace"
                (\() ->
                    "{a , b}"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                                (GrenSyntax.PatternRecord
                                    [ GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } "a"
                                    , GrenSyntax.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } "b"
                                    ]
                                )
                            )
                )
            , Test.test "Record pattern with extra comma between fields"
                (\() ->
                    "{a,, b}"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                                (GrenSyntax.PatternRecord
                                    [ GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } "a"
                                    , GrenSyntax.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } "b"
                                    ]
                                )
                            )
                )
            , Test.test "Record pattern with prefixed comma"
                (\() ->
                    "{ , a , b }"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                (GrenSyntax.PatternRecord
                                    [ GrenSyntax.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } "a"
                                    , GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } "b"
                                    ]
                                )
                            )
                )
            , Test.test "Record pattern with field name colliding with keyword"
                (\() ->
                    "{ ,as , b }"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                (GrenSyntax.PatternRecord
                                    [ GrenSyntax.Node { start = { row = 1, column = 4 }, end = { row = 1, column = 6 } } "as_"
                                    , GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } "b"
                                    ]
                                )
                            )
                )
            , Test.test "Record pattern with trailing whitespace"
                (\() ->
                    "{a }"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }
                                (GrenSyntax.PatternRecord
                                    [ GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } "a" ]
                                )
                            )
                )
            , Test.test "Record pattern with leading whitespace"
                (\() ->
                    "{ a}"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }
                                (GrenSyntax.PatternRecord
                                    [ GrenSyntax.Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } "a" ]
                                )
                            )
                )
            , Test.test "Named"
                (\() ->
                    "True"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }
                                (GrenSyntax.PatternVariant { moduleName = [], name = "True" } [])
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                (GrenSyntax.PatternVariant { moduleName = [ "Basics" ], name = "True" } [])
                            )
                )
            , Test.test "Named pattern with data"
                (\() ->
                    "Set x"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                                (GrenSyntax.PatternVariant
                                    { moduleName = [], name = "Set" }
                                    [ GrenSyntax.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } (GrenSyntax.PatternVariable "x") ]
                                )
                            )
                )
            , Test.test "Qualified named pattern with data"
                (\() ->
                    "Set.Set x"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                                (GrenSyntax.PatternVariant
                                    { moduleName = [ "Set" ], name = "Set" }
                                    [ GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (GrenSyntax.PatternVariable "x") ]
                                )
                            )
                )
            , Test.test "Tuple"
                (\() ->
                    "(model, cmd)"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                                (GrenSyntax.PatternTuple
                                    [ GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } (GrenSyntax.PatternVariable "model")
                                    , GrenSyntax.Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } (GrenSyntax.PatternVariable "cmd")
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                                (GrenSyntax.PatternTuple
                                    [ GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (GrenSyntax.PatternVariable "a")
                                    , GrenSyntax.Node { start = { row = 1, column = 4 }, end = { row = 1, column = 9 } } (GrenSyntax.PatternRecord [ GrenSyntax.Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } "b", GrenSyntax.Node { start = { row = 1, column = 7 }, end = { row = 1, column = 8 } } "c" ])
                                    , GrenSyntax.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 12 } } GrenSyntax.PatternUnit
                                    ]
                                )
                            )
                )
            , Test.test "As pattern"
                (\() ->
                    "x as y"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                                (GrenSyntax.PatternAs
                                    (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } (GrenSyntax.PatternVariable "x"))
                                    (GrenSyntax.Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } "y")
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
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 28 } }
                                (GrenSyntax.PatternAs
                                    (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 16 } }
                                        (GrenSyntax.PatternRecord
                                            [ GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } "model"
                                            , GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 15 } } "context"
                                            ]
                                        )
                                    )
                                    (GrenSyntax.Node { start = { row = 1, column = 20 }, end = { row = 1, column = 28 } } "appState")
                                )
                            )
                )
            , Test.test "Complex"
                (\() ->
                    "(Index irec as index, docVector)"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 33 } }
                                (GrenSyntax.PatternTuple
                                    [ GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 21 } }
                                        (GrenSyntax.PatternAs
                                            (GrenSyntax.Node { start = { row = 1, column = 2 }, end = { row = 1, column = 12 } }
                                                (GrenSyntax.PatternVariant { moduleName = [], name = "Index" }
                                                    [ GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 12 } } (GrenSyntax.PatternVariable "irec") ]
                                                )
                                            )
                                            (GrenSyntax.Node { start = { row = 1, column = 16 }, end = { row = 1, column = 21 } } "index")
                                        )
                                    , GrenSyntax.Node { start = { row = 1, column = 23 }, end = { row = 1, column = 32 } } (GrenSyntax.PatternVariable "docVector")
                                    ]
                                )
                            )
                )
            , Test.test "Complex pattern 2"
                (\() ->
                    "RBNode_gren_builtin col (RBNode_gren_builtin Red  (RBNode_gren_builtin Red xv))"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            (GrenSyntax.Node { end = { column = 80, row = 1 }, start = { column = 1, row = 1 } }
                                (GrenSyntax.PatternVariant { moduleName = [], name = "RBNode_gren_builtin" }
                                    [ GrenSyntax.Node { end = { column = 24, row = 1 }, start = { column = 21, row = 1 } }
                                        (GrenSyntax.PatternVariable "col")
                                    , GrenSyntax.Node { end = { column = 80, row = 1 }, start = { column = 25, row = 1 } }
                                        (GrenSyntax.PatternParenthesized
                                            (GrenSyntax.Node { end = { column = 79, row = 1 }, start = { column = 26, row = 1 } }
                                                (GrenSyntax.PatternVariant { moduleName = [], name = "RBNode_gren_builtin" }
                                                    [ GrenSyntax.Node { end = { column = 49, row = 1 }, start = { column = 46, row = 1 } }
                                                        (GrenSyntax.PatternVariant { moduleName = [], name = "Red" } [])
                                                    , GrenSyntax.Node { end = { column = 79, row = 1 }, start = { column = 51, row = 1 } }
                                                        (GrenSyntax.PatternParenthesized
                                                            (GrenSyntax.Node { end = { column = 78, row = 1 }, start = { column = 52, row = 1 } }
                                                                (GrenSyntax.PatternVariant { moduleName = [], name = "RBNode_gren_builtin" }
                                                                    [ GrenSyntax.Node { end = { column = 75, row = 1 }, start = { column = 72, row = 1 } }
                                                                        (GrenSyntax.PatternVariant { moduleName = [], name = "Red" } [])
                                                                    , GrenSyntax.Node { end = { column = 78, row = 1 }, start = { column = 76, row = 1 } }
                                                                        (GrenSyntax.PatternVariable "xv")
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
                                    GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                                            , exposingList =
                                                GrenSyntax.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } (GrenSyntax.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                                            }
                                        )
                                , imports =
                                    [ GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 14 } }
                                        { moduleName = GrenSyntax.Node { start = { row = 3, column = 8 }, end = { row = 3, column = 14 } } [ "String" ]
                                        , moduleAlias = Nothing
                                        , exposingList = Nothing
                                        }
                                    ]
                                , declarations =
                                    [ GrenSyntax.Node { start = { row = 5, column = 1 }, end = { row = 7, column = 8 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Just (GrenSyntax.Node { start = { row = 5, column = 1 }, end = { row = 6, column = 3 } } "{-| The docs\n-}")
                                            , signature = Nothing
                                            , declaration =
                                                GrenSyntax.Node { start = { row = 7, column = 1 }, end = { row = 7, column = 8 } }
                                                    { name = GrenSyntax.Node { start = { row = 7, column = 1 }, end = { row = 7, column = 4 } } "bar"
                                                    , arguments = []
                                                    , expression = GrenSyntax.Node { start = { row = 7, column = 7 }, end = { row = 7, column = 8 } } (GrenSyntax.Integer 1)
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
                                    GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                                            , exposingList = GrenSyntax.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } (GrenSyntax.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                                            }
                                        )
                                , imports =
                                    [ GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 14 } }
                                        { moduleName = GrenSyntax.Node { start = { row = 3, column = 8 }, end = { row = 3, column = 14 } } [ "String" ]
                                        , moduleAlias = Nothing
                                        , exposingList = Nothing
                                        }
                                    ]
                                , declarations =
                                    [ GrenSyntax.Node { start = { row = 5, column = 1 }, end = { row = 8, column = 8 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation =
                                                Just (GrenSyntax.Node { start = { row = 5, column = 1 }, end = { row = 6, column = 3 } } "{-| The docs\n-}")
                                            , signature =
                                                Just
                                                    (GrenSyntax.Node { start = { row = 7, column = 1 }, end = { row = 7, column = 10 } }
                                                        { name = GrenSyntax.Node { start = { row = 7, column = 1 }, end = { row = 7, column = 4 } } "bar"
                                                        , typeAnnotation =
                                                            GrenSyntax.Node { start = { row = 7, column = 7 }, end = { row = 7, column = 10 } } (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 7, column = 7 }, end = { row = 7, column = 10 } } ( [], "Int" )) [])
                                                        }
                                                    )
                                            , declaration =
                                                GrenSyntax.Node { start = { row = 8, column = 1 }, end = { row = 8, column = 8 } }
                                                    { name = GrenSyntax.Node { start = { row = 8, column = 1 }, end = { row = 8, column = 4 } } "bar"
                                                    , arguments = []
                                                    , expression = GrenSyntax.Node { start = { row = 8, column = 7 }, end = { row = 8, column = 8 } } (GrenSyntax.Integer 1)
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
                                    GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                                            , exposingList = GrenSyntax.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } (GrenSyntax.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ GrenSyntax.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 8 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                GrenSyntax.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 8 } }
                                                    { name = GrenSyntax.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 4 } } "bar"
                                                    , arguments = []
                                                    , expression = GrenSyntax.Node { start = { row = 4, column = 7 }, end = { row = 4, column = 8 } } (GrenSyntax.Integer 1)
                                                    }
                                            }
                                        )
                                    ]
                                , comments = [ GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 10 } } "--The Doc" ]
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
                                    GrenSyntax.Node { start = { row = 2, column = 1 }, end = { row = 2, column = 25 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = GrenSyntax.Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } [ "Bar" ]
                                            , exposingList = GrenSyntax.Node { start = { row = 2, column = 12 }, end = { row = 2, column = 25 } } (GrenSyntax.All { start = { row = 2, column = 22 }, end = { row = 2, column = 24 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ GrenSyntax.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 24 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                GrenSyntax.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 24 } }
                                                    { name = GrenSyntax.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 4 } } "bar"
                                                    , arguments = []
                                                    , expression = GrenSyntax.Node { start = { row = 5, column = 23 }, end = { row = 5, column = 24 } } (GrenSyntax.Integer 1)
                                                    }
                                            }
                                        )
                                    ]
                                , comments =
                                    [ GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } } "-- comment 1"
                                    , GrenSyntax.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 13 } } "-- comment 2"
                                    , GrenSyntax.Node { start = { row = 5, column = 7 }, end = { row = 5, column = 22 } } "{- comment 3 -}"
                                    , GrenSyntax.Node { start = { row = 5, column = 25 }, end = { row = 5, column = 37 } } "-- comment 4"
                                    , GrenSyntax.Node { start = { row = 6, column = 2 }, end = { row = 6, column = 14 } } "-- comment 5"
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
                                    GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                                            , exposingList =
                                                GrenSyntax.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } (GrenSyntax.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ GrenSyntax.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 8 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                GrenSyntax.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 8 } }
                                                    { name = GrenSyntax.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 4 } } "bar"
                                                    , arguments = []
                                                    , expression = GrenSyntax.Node { start = { row = 4, column = 7 }, end = { row = 4, column = 8 } } (GrenSyntax.Integer 1)
                                                    }
                                            }
                                        )
                                    ]
                                , comments = [ GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 14 } } "{- The Doc -}" ]
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
                                    GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                                            , exposingList = GrenSyntax.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } (GrenSyntax.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                                            }
                                        )
                                , imports =
                                    [ GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 14 } }
                                        { moduleName = GrenSyntax.Node { start = { row = 3, column = 8 }, end = { row = 3, column = 14 } } [ "String" ]
                                        , moduleAlias = Nothing
                                        , exposingList = Nothing
                                        }
                                    ]
                                , declarations =
                                    [ GrenSyntax.Node { start = { row = 5, column = 1 }, end = { row = 7, column = 23 } }
                                        (GrenSyntax.AliasDeclaration
                                            { documentation =
                                                Just (GrenSyntax.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 15 } } "{-| The Doc -}")
                                            , name = GrenSyntax.Node { start = { row = 6, column = 12 }, end = { row = 6, column = 15 } } "Foo"
                                            , generics = []
                                            , typeAnnotation =
                                                GrenSyntax.Node { start = { row = 7, column = 6 }, end = { row = 7, column = 23 } }
                                                    (GrenSyntax.TypeAnnotationRecord
                                                        [ GrenSyntax.Node { start = { row = 7, column = 8 }, end = { row = 7, column = 21 } }
                                                            ( GrenSyntax.Node { start = { row = 7, column = 8 }, end = { row = 7, column = 12 } } "name"
                                                            , GrenSyntax.Node { start = { row = 7, column = 15 }, end = { row = 7, column = 21 } } (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 7, column = 15 }, end = { row = 7, column = 21 } } ( [], "String" )) [])
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
                                { moduleDefinition = GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } } (GrenSyntax.NormalModule { moduleName = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ], exposingList = GrenSyntax.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } (GrenSyntax.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }) })
                                , imports =
                                    [ GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 14 } }
                                        { moduleName = GrenSyntax.Node { start = { row = 3, column = 8 }, end = { row = 3, column = 14 } } [ "String" ]
                                        , moduleAlias = Nothing
                                        , exposingList = Nothing
                                        }
                                    ]
                                , declarations =
                                    [ GrenSyntax.Node { start = { row = 5, column = 1 }, end = { row = 8, column = 10 } }
                                        (GrenSyntax.CustomTypeDeclaration
                                            { documentation =
                                                Just
                                                    (GrenSyntax.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 15 } }
                                                        "{-| The Doc -}"
                                                    )
                                            , name = GrenSyntax.Node { start = { row = 6, column = 6 }, end = { row = 6, column = 9 } } "Foo"
                                            , generics = []
                                            , constructors =
                                                [ GrenSyntax.Node { start = { row = 7, column = 6 }, end = { row = 7, column = 9 } }
                                                    { name = GrenSyntax.Node { start = { row = 7, column = 6 }, end = { row = 7, column = 9 } } "Red", arguments = [] }
                                                , GrenSyntax.Node { start = { row = 8, column = 6 }, end = { row = 8, column = 10 } }
                                                    { name = GrenSyntax.Node { start = { row = 8, column = 6 }, end = { row = 8, column = 10 } } "Blue", arguments = [] }
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
                                    GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 42 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 27 } } [ "Simplify", "AstHelpers" ]
                                            , exposingList =
                                                GrenSyntax.Node { start = { row = 1, column = 28 }, end = { row = 1, column = 42 } }
                                                    (GrenSyntax.Explicit
                                                        [ GrenSyntax.Node { start = { row = 1, column = 38 }, end = { row = 1, column = 41 } } (GrenSyntax.FunctionExpose "log")
                                                        ]
                                                    )
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ GrenSyntax.Node { start = { row = 4, column = 1 }, end = { row = 6, column = 21 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Just (GrenSyntax.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 17 } } { name = GrenSyntax.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 4 } } "log", typeAnnotation = GrenSyntax.Node { start = { row = 4, column = 7 }, end = { row = 4, column = 17 } } (GrenSyntax.TypeAnnotationFunction (GrenSyntax.Node { start = { row = 4, column = 7 }, end = { row = 4, column = 10 } } (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 4, column = 7 }, end = { row = 4, column = 10 } } ( [], "Int" )) [])) (GrenSyntax.Node { start = { row = 4, column = 14 }, end = { row = 4, column = 17 } } (GrenSyntax.TypeAnnotationConstruct (GrenSyntax.Node { start = { row = 4, column = 14 }, end = { row = 4, column = 17 } } ( [], "Int" )) []))) })
                                            , declaration =
                                                GrenSyntax.Node { start = { row = 5, column = 1 }, end = { row = 6, column = 21 } }
                                                    { name = GrenSyntax.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 4 } } "log"
                                                    , arguments = [ GrenSyntax.Node { start = { row = 5, column = 5 }, end = { row = 5, column = 6 } } (GrenSyntax.PatternVariable "a") ]
                                                    , expression =
                                                        GrenSyntax.Node { start = { row = 6, column = 5 }, end = { row = 6, column = 21 } }
                                                            (GrenSyntax.Application
                                                                [ GrenSyntax.Node { start = { row = 6, column = 5 }, end = { row = 6, column = 14 } } (GrenSyntax.FunctionOrValue [ "Debug" ] "log")
                                                                , GrenSyntax.Node { start = { row = 6, column = 15 }, end = { row = 6, column = 19 } } (GrenSyntax.Literal "ok")
                                                                , GrenSyntax.Node { start = { row = 6, column = 20 }, end = { row = 6, column = 21 } } (GrenSyntax.FunctionOrValue [] "a")
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
                                    GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                                            , exposingList = GrenSyntax.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } (GrenSyntax.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 24 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 24 } }
                                                    { name = GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 4 } } "bar"
                                                    , arguments = []
                                                    , expression =
                                                        GrenSyntax.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 24 } }
                                                            (GrenSyntax.OperatorApplication "*"
                                                                (GrenSyntax.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 14 } }
                                                                    (GrenSyntax.ParenthesizedExpression
                                                                        (GrenSyntax.Node { start = { row = 3, column = 8 }, end = { row = 3, column = 13 } }
                                                                            (GrenSyntax.OperatorApplication "+"
                                                                                (GrenSyntax.Node { start = { row = 3, column = 8 }, end = { row = 3, column = 9 } } (GrenSyntax.FunctionOrValue [] "x"))
                                                                                (GrenSyntax.Node { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } } (GrenSyntax.Integer 1))
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                                (GrenSyntax.Node { start = { row = 3, column = 17 }, end = { row = 3, column = 24 } }
                                                                    (GrenSyntax.ParenthesizedExpression
                                                                        (GrenSyntax.Node { start = { row = 3, column = 18 }, end = { row = 3, column = 23 } }
                                                                            (GrenSyntax.OperatorApplication "*"
                                                                                (GrenSyntax.Node { start = { row = 3, column = 18 }, end = { row = 3, column = 19 } } (GrenSyntax.Integer 2))
                                                                                (GrenSyntax.Node { start = { row = 3, column = 22 }, end = { row = 3, column = 23 } } (GrenSyntax.FunctionOrValue [] "y"))
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
                                    GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                                            , exposingList = GrenSyntax.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } (GrenSyntax.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 16 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 16 } }
                                                    { name = GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 4 } } "bar"
                                                    , arguments = []
                                                    , expression =
                                                        GrenSyntax.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 16 } }
                                                            (GrenSyntax.OperatorApplication "+"
                                                                (GrenSyntax.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 8 } } (GrenSyntax.FunctionOrValue [] "x"))
                                                                (GrenSyntax.Node { start = { row = 3, column = 11 }, end = { row = 3, column = 16 } }
                                                                    (GrenSyntax.OperatorApplication "*"
                                                                        (GrenSyntax.Node { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } } (GrenSyntax.Integer 1))
                                                                        (GrenSyntax.Node { start = { row = 3, column = 15 }, end = { row = 3, column = 16 } } (GrenSyntax.Integer 2))
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
                                    GrenSyntax.Node
                                        { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                                            , exposingList = GrenSyntax.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } (GrenSyntax.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 16 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 16 } }
                                                    { name = GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 4 } } "bar"
                                                    , arguments = []
                                                    , expression =
                                                        GrenSyntax.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 16 } }
                                                            (GrenSyntax.OperatorApplication "+"
                                                                (GrenSyntax.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 12 } }
                                                                    (GrenSyntax.OperatorApplication "*"
                                                                        (GrenSyntax.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 8 } } (GrenSyntax.FunctionOrValue [] "x"))
                                                                        (GrenSyntax.Node { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } } (GrenSyntax.Integer 1))
                                                                    )
                                                                )
                                                                (GrenSyntax.Node { start = { row = 3, column = 15 }, end = { row = 3, column = 16 } } (GrenSyntax.Integer 2))
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
                                    GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                                            , exposingList = GrenSyntax.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } (GrenSyntax.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 15 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 15 } }
                                                    { name = GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 4 } } "bar"
                                                    , arguments = []
                                                    , expression =
                                                        GrenSyntax.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 15 } }
                                                            (GrenSyntax.Negation
                                                                (GrenSyntax.Node { start = { row = 3, column = 8 }, end = { row = 3, column = 15 } }
                                                                    (GrenSyntax.ParenthesizedExpression
                                                                        (GrenSyntax.Node { start = { row = 3, column = 9 }, end = { row = 3, column = 14 } }
                                                                            (GrenSyntax.OperatorApplication
                                                                                "*"
                                                                                (GrenSyntax.Node { start = { row = 3, column = 9 }, end = { row = 3, column = 10 } } (GrenSyntax.Integer 1))
                                                                                (GrenSyntax.Node { start = { row = 3, column = 13 }, end = { row = 3, column = 14 } } (GrenSyntax.Integer 2))
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
                                    GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Bar" ]
                                            , exposingList = GrenSyntax.Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } } (GrenSyntax.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 16 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 16 } }
                                                    { name = GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 4 } } "bar"
                                                    , arguments = []
                                                    , expression =
                                                        GrenSyntax.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 16 } }
                                                            (GrenSyntax.RecordAccess
                                                                (GrenSyntax.Node { start = { row = 3, column = 7 }, end = { row = 3, column = 14 } }
                                                                    (GrenSyntax.ParenthesizedExpression
                                                                        (GrenSyntax.Node { start = { row = 3, column = 8 }, end = { row = 3, column = 13 } }
                                                                            (GrenSyntax.OperatorApplication
                                                                                "*"
                                                                                (GrenSyntax.Node { start = { row = 3, column = 8 }, end = { row = 3, column = 9 } } (GrenSyntax.Integer 1))
                                                                                (GrenSyntax.Node { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } } (GrenSyntax.Integer 2))
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                                (GrenSyntax.Node { start = { row = 3, column = 15 }, end = { row = 3, column = 16 } } "x")
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
                                    GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 23 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } } [ "A" ]
                                            , exposingList =
                                                GrenSyntax.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 23 } }
                                                    (GrenSyntax.All { start = { row = 1, column = 20 }, end = { row = 1, column = 22 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 29 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 29 } }
                                                    { name = GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 6 } } "bool1"
                                                    , arguments = []
                                                    , expression =
                                                        GrenSyntax.Node { start = { row = 3, column = 9 }, end = { row = 3, column = 29 } }
                                                            (GrenSyntax.OperatorApplication "||"
                                                                (GrenSyntax.Node { start = { row = 3, column = 9 }, end = { row = 3, column = 21 } }
                                                                    (GrenSyntax.OperatorApplication "&&"
                                                                        (GrenSyntax.Node { start = { row = 3, column = 9 }, end = { row = 3, column = 13 } } (GrenSyntax.FunctionOrValue [] "True"))
                                                                        (GrenSyntax.Node { start = { row = 3, column = 17 }, end = { row = 3, column = 21 } } (GrenSyntax.FunctionOrValue [] "True"))
                                                                    )
                                                                )
                                                                (GrenSyntax.Node { start = { row = 3, column = 25 }, end = { row = 3, column = 29 } } (GrenSyntax.FunctionOrValue [] "True"))
                                                            )
                                                    }
                                            }
                                        )
                                    , GrenSyntax.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 29 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                GrenSyntax.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 29 } }
                                                    { name = GrenSyntax.Node { start = { row = 4, column = 1 }, end = { row = 4, column = 6 } } "bool2"
                                                    , arguments = []
                                                    , expression =
                                                        GrenSyntax.Node { start = { row = 4, column = 9 }, end = { row = 4, column = 29 } }
                                                            (GrenSyntax.OperatorApplication "||"
                                                                (GrenSyntax.Node { start = { row = 4, column = 9 }, end = { row = 4, column = 13 } } (GrenSyntax.FunctionOrValue [] "True"))
                                                                (GrenSyntax.Node { start = { row = 4, column = 17 }, end = { row = 4, column = 29 } }
                                                                    (GrenSyntax.OperatorApplication "&&"
                                                                        (GrenSyntax.Node { start = { row = 4, column = 17 }, end = { row = 4, column = 21 } } (GrenSyntax.FunctionOrValue [] "True"))
                                                                        (GrenSyntax.Node { start = { row = 4, column = 25 }, end = { row = 4, column = 29 } } (GrenSyntax.FunctionOrValue [] "True"))
                                                                    )
                                                                )
                                                            )
                                                    }
                                            }
                                        )
                                    , GrenSyntax.Node { start = { row = 6, column = 1 }, end = { row = 6, column = 25 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                GrenSyntax.Node { start = { row = 6, column = 1 }, end = { row = 6, column = 25 } }
                                                    { name = GrenSyntax.Node { start = { row = 6, column = 1 }, end = { row = 6, column = 9 } } "numeric1"
                                                    , arguments = []
                                                    , expression =
                                                        GrenSyntax.Node { start = { row = 6, column = 12 }, end = { row = 6, column = 25 } }
                                                            (GrenSyntax.OperatorApplication "+"
                                                                (GrenSyntax.Node { start = { row = 6, column = 12 }, end = { row = 6, column = 21 } }
                                                                    (GrenSyntax.OperatorApplication "*"
                                                                        (GrenSyntax.Node { start = { row = 6, column = 12 }, end = { row = 6, column = 17 } }
                                                                            (GrenSyntax.OperatorApplication "^"
                                                                                (GrenSyntax.Node { start = { row = 6, column = 12 }, end = { row = 6, column = 13 } } (GrenSyntax.Integer 1))
                                                                                (GrenSyntax.Node { start = { row = 6, column = 16 }, end = { row = 6, column = 17 } } (GrenSyntax.Integer 2))
                                                                            )
                                                                        )
                                                                        (GrenSyntax.Node { start = { row = 6, column = 20 }, end = { row = 6, column = 21 } } (GrenSyntax.Integer 3))
                                                                    )
                                                                )
                                                                (GrenSyntax.Node
                                                                    { start = { row = 6, column = 24 }, end = { row = 6, column = 25 } }
                                                                    (GrenSyntax.Integer 4)
                                                                )
                                                            )
                                                    }
                                            }
                                        )
                                    , GrenSyntax.Node { start = { row = 7, column = 1 }, end = { row = 7, column = 25 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                GrenSyntax.Node { start = { row = 7, column = 1 }, end = { row = 7, column = 25 } }
                                                    { name = GrenSyntax.Node { start = { row = 7, column = 1 }, end = { row = 7, column = 9 } } "numeric2"
                                                    , arguments = []
                                                    , expression =
                                                        GrenSyntax.Node { start = { row = 7, column = 12 }, end = { row = 7, column = 25 } }
                                                            (GrenSyntax.OperatorApplication "+"
                                                                (GrenSyntax.Node { start = { row = 7, column = 12 }, end = { row = 7, column = 13 } } (GrenSyntax.Integer 1))
                                                                (GrenSyntax.Node { start = { row = 7, column = 16 }, end = { row = 7, column = 25 } }
                                                                    (GrenSyntax.OperatorApplication "*"
                                                                        (GrenSyntax.Node { start = { row = 7, column = 16 }, end = { row = 7, column = 17 } } (GrenSyntax.Integer 2))
                                                                        (GrenSyntax.Node { start = { row = 7, column = 20 }, end = { row = 7, column = 25 } }
                                                                            (GrenSyntax.OperatorApplication "^"
                                                                                (GrenSyntax.Node { start = { row = 7, column = 20 }, end = { row = 7, column = 21 } } (GrenSyntax.Integer 3))
                                                                                (GrenSyntax.Node { start = { row = 7, column = 24 }, end = { row = 7, column = 25 } } (GrenSyntax.Integer 4))
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
                                    GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 23 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } } [ "A" ]
                                            , exposingList =
                                                GrenSyntax.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 23 } }
                                                    (GrenSyntax.All { start = { row = 1, column = 20 }, end = { row = 1, column = 22 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 21 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 21 } }
                                                    { name = GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 9 } } "numeric1"
                                                    , arguments = []
                                                    , expression =
                                                        GrenSyntax.Node { start = { row = 3, column = 12 }, end = { row = 3, column = 21 } }
                                                            (GrenSyntax.OperatorApplication "-"
                                                                (GrenSyntax.Node { start = { row = 3, column = 12 }, end = { row = 3, column = 17 } }
                                                                    (GrenSyntax.OperatorApplication "+"
                                                                        (GrenSyntax.Node { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } } (GrenSyntax.Integer 1))
                                                                        (GrenSyntax.Node { start = { row = 3, column = 16 }, end = { row = 3, column = 17 } } (GrenSyntax.Integer 2))
                                                                    )
                                                                )
                                                                (GrenSyntax.Node { start = { row = 3, column = 20 }, end = { row = 3, column = 21 } } (GrenSyntax.Integer 3))
                                                            )
                                                    }
                                            }
                                        )
                                    , GrenSyntax.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 24 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                GrenSyntax.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 24 } }
                                                    { name = GrenSyntax.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 10 } } "pipeline1"
                                                    , arguments = []
                                                    , expression =
                                                        GrenSyntax.Node { start = { row = 5, column = 13 }, end = { row = 5, column = 24 } }
                                                            (GrenSyntax.OperatorApplication "|>"
                                                                (GrenSyntax.Node { start = { row = 5, column = 13 }, end = { row = 5, column = 19 } }
                                                                    (GrenSyntax.OperatorApplication "|>"
                                                                        (GrenSyntax.Node { start = { row = 5, column = 13 }, end = { row = 5, column = 14 } } (GrenSyntax.Integer 1))
                                                                        (GrenSyntax.Node { start = { row = 5, column = 18 }, end = { row = 5, column = 19 } } (GrenSyntax.Integer 2))
                                                                    )
                                                                )
                                                                (GrenSyntax.Node { start = { row = 5, column = 23 }, end = { row = 5, column = 24 } } (GrenSyntax.Integer 3))
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
                                    GrenSyntax.Node { start = { row = 1, column = 1 }, end = { row = 1, column = 23 } }
                                        (GrenSyntax.NormalModule
                                            { moduleName = GrenSyntax.Node { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } } [ "A" ]
                                            , exposingList =
                                                GrenSyntax.Node { start = { row = 1, column = 10 }, end = { row = 1, column = 23 } }
                                                    (GrenSyntax.All { start = { row = 1, column = 20 }, end = { row = 1, column = 22 } })
                                            }
                                        )
                                , imports = []
                                , declarations =
                                    [ GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 22 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 22 } }
                                                    { name = GrenSyntax.Node { start = { row = 3, column = 1 }, end = { row = 3, column = 10 } } "pipeline0"
                                                    , arguments = []
                                                    , expression =
                                                        GrenSyntax.Node { start = { row = 3, column = 13 }, end = { row = 3, column = 22 } }
                                                            (GrenSyntax.OperatorApplication "|>"
                                                                (GrenSyntax.Node { start = { row = 3, column = 13 }, end = { row = 3, column = 18 } }
                                                                    (GrenSyntax.OperatorApplication "|>"
                                                                        (GrenSyntax.Node { start = { row = 3, column = 13 }, end = { row = 3, column = 14 } } (GrenSyntax.Integer 1))
                                                                        (GrenSyntax.Node { start = { row = 3, column = 17 }, end = { row = 3, column = 18 } } (GrenSyntax.Integer 2))
                                                                    )
                                                                )
                                                                (GrenSyntax.Node { start = { row = 3, column = 21 }, end = { row = 3, column = 22 } } (GrenSyntax.Integer 3))
                                                            )
                                                    }
                                            }
                                        )
                                    , GrenSyntax.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 24 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                GrenSyntax.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 24 } }
                                                    { name = GrenSyntax.Node { start = { row = 5, column = 1 }, end = { row = 5, column = 10 } } "pipeline1"
                                                    , arguments = []
                                                    , expression =
                                                        GrenSyntax.Node { start = { row = 5, column = 13 }, end = { row = 5, column = 24 } }
                                                            (GrenSyntax.OperatorApplication "|>"
                                                                (GrenSyntax.Node { start = { row = 5, column = 13 }, end = { row = 5, column = 19 } }
                                                                    (GrenSyntax.OperatorApplication "|>"
                                                                        (GrenSyntax.Node { start = { row = 5, column = 13 }, end = { row = 5, column = 14 } } (GrenSyntax.Integer 1))
                                                                        (GrenSyntax.Node { start = { row = 5, column = 18 }, end = { row = 5, column = 19 } } (GrenSyntax.Integer 2))
                                                                    )
                                                                )
                                                                (GrenSyntax.Node { start = { row = 5, column = 23 }, end = { row = 5, column = 24 } } (GrenSyntax.Integer 3))
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
                                    [ GrenSyntax.Node { end = { column = 19, row = 3 }, start = { column = 1, row = 3 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { declaration =
                                                GrenSyntax.Node { end = { column = 19, row = 3 }, start = { column = 1, row = 3 } }
                                                    { arguments = []
                                                    , expression =
                                                        GrenSyntax.Node { end = { column = 19, row = 3 }, start = { column = 13, row = 3 } }
                                                            (GrenSyntax.OperatorApplication "/="
                                                                (GrenSyntax.Node { end = { column = 14, row = 3 }, start = { column = 13, row = 3 } }
                                                                    (GrenSyntax.Integer 1)
                                                                )
                                                                (GrenSyntax.Node { end = { column = 19, row = 3 }, start = { column = 18, row = 3 } }
                                                                    (GrenSyntax.Integer 2)
                                                                )
                                                            )
                                                    , name = GrenSyntax.Node { end = { column = 10, row = 3 }, start = { column = 1, row = 3 } } "pipeline0"
                                                    }
                                            , documentation = Nothing
                                            , signature = Nothing
                                            }
                                        )
                                    , GrenSyntax.Node { end = { column = 19, row = 5 }, start = { column = 1, row = 5 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { declaration =
                                                GrenSyntax.Node { end = { column = 19, row = 5 }, start = { column = 1, row = 5 } }
                                                    { arguments = []
                                                    , expression =
                                                        GrenSyntax.Node { end = { column = 19, row = 5 }, start = { column = 13, row = 5 } }
                                                            (GrenSyntax.OperatorApplication "/="
                                                                (GrenSyntax.Node { end = { column = 14, row = 5 }, start = { column = 13, row = 5 } }
                                                                    (GrenSyntax.Integer 1)
                                                                )
                                                                (GrenSyntax.Node { end = { column = 19, row = 5 }, start = { column = 18, row = 5 } }
                                                                    (GrenSyntax.Integer 2)
                                                                )
                                                            )
                                                    , name = GrenSyntax.Node { end = { column = 10, row = 5 }, start = { column = 1, row = 5 } } "pipeline1"
                                                    }
                                            , documentation = Nothing
                                            , signature = Nothing
                                            }
                                        )
                                    ]
                                , imports = []
                                , moduleDefinition =
                                    GrenSyntax.Node { end = { column = 23, row = 1 }, start = { column = 1, row = 1 } }
                                        (GrenSyntax.NormalModule
                                            { exposingList =
                                                GrenSyntax.Node { end = { column = 23, row = 1 }, start = { column = 10, row = 1 } }
                                                    (GrenSyntax.All { end = { column = 22, row = 1 }, start = { column = 20, row = 1 } })
                                            , moduleName = GrenSyntax.Node { end = { column = 9, row = 1 }, start = { column = 8, row = 1 } } [ "A" ]
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
                                    [ GrenSyntax.Node { end = { column = 20, row = 3 }, start = { column = 1, row = 3 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { declaration =
                                                GrenSyntax.Node { end = { column = 20, row = 3 }, start = { column = 1, row = 3 } }
                                                    { arguments = []
                                                    , expression =
                                                        GrenSyntax.Node { end = { column = 20, row = 3 }, start = { column = 13, row = 3 } }
                                                            (GrenSyntax.OperatorApplication "/="
                                                                (GrenSyntax.Node { end = { column = 14, row = 3 }, start = { column = 13, row = 3 } }
                                                                    (GrenSyntax.Integer 1)
                                                                )
                                                                (GrenSyntax.Node { end = { column = 20, row = 3 }, start = { column = 19, row = 3 } }
                                                                    (GrenSyntax.Integer 2)
                                                                )
                                                            )
                                                    , name = GrenSyntax.Node { end = { column = 10, row = 3 }, start = { column = 1, row = 3 } } "pipeline0"
                                                    }
                                            , documentation = Nothing
                                            , signature = Nothing
                                            }
                                        )
                                    , GrenSyntax.Node { end = { column = 19, row = 5 }, start = { column = 1, row = 5 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { declaration =
                                                GrenSyntax.Node { end = { column = 19, row = 5 }, start = { column = 1, row = 5 } }
                                                    { arguments = []
                                                    , expression =
                                                        GrenSyntax.Node { end = { column = 19, row = 5 }, start = { column = 13, row = 5 } }
                                                            (GrenSyntax.OperatorApplication "/="
                                                                (GrenSyntax.Node { end = { column = 14, row = 5 }, start = { column = 13, row = 5 } }
                                                                    (GrenSyntax.Integer 1)
                                                                )
                                                                (GrenSyntax.Node { end = { column = 19, row = 5 }, start = { column = 18, row = 5 } }
                                                                    (GrenSyntax.Integer 2)
                                                                )
                                                            )
                                                    , name = GrenSyntax.Node { end = { column = 10, row = 5 }, start = { column = 1, row = 5 } } "pipeline1"
                                                    }
                                            , documentation = Nothing
                                            , signature = Nothing
                                            }
                                        )
                                    ]
                                , imports = []
                                , moduleDefinition =
                                    GrenSyntax.Node { end = { column = 23, row = 1 }, start = { column = 1, row = 1 } }
                                        (GrenSyntax.NormalModule
                                            { exposingList =
                                                GrenSyntax.Node { end = { column = 23, row = 1 }, start = { column = 10, row = 1 } }
                                                    (GrenSyntax.All { end = { column = 22, row = 1 }, start = { column = 20, row = 1 } })
                                            , moduleName = GrenSyntax.Node { end = { column = 9, row = 1 }, start = { column = 8, row = 1 } } [ "A" ]
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
                                    [ GrenSyntax.Node { end = { column = 20, row = 3 }, start = { column = 1, row = 3 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { declaration =
                                                GrenSyntax.Node { end = { column = 20, row = 3 }, start = { column = 1, row = 3 } }
                                                    { arguments = []
                                                    , expression =
                                                        GrenSyntax.Node { end = { column = 20, row = 3 }, start = { column = 13, row = 3 } }
                                                            (GrenSyntax.OperatorApplication "=="
                                                                (GrenSyntax.Node { end = { column = 14, row = 3 }, start = { column = 13, row = 3 } }
                                                                    (GrenSyntax.Integer 1)
                                                                )
                                                                (GrenSyntax.Node { end = { column = 20, row = 3 }, start = { column = 19, row = 3 } }
                                                                    (GrenSyntax.Integer 2)
                                                                )
                                                            )
                                                    , name = GrenSyntax.Node { end = { column = 10, row = 3 }, start = { column = 1, row = 3 } } "pipeline0"
                                                    }
                                            , documentation = Nothing
                                            , signature = Nothing
                                            }
                                        )
                                    , GrenSyntax.Node { end = { column = 19, row = 5 }, start = { column = 1, row = 5 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { declaration =
                                                GrenSyntax.Node { end = { column = 19, row = 5 }, start = { column = 1, row = 5 } }
                                                    { arguments = []
                                                    , expression =
                                                        GrenSyntax.Node { end = { column = 19, row = 5 }, start = { column = 13, row = 5 } }
                                                            (GrenSyntax.OperatorApplication "=="
                                                                (GrenSyntax.Node { end = { column = 14, row = 5 }, start = { column = 13, row = 5 } }
                                                                    (GrenSyntax.Integer 1)
                                                                )
                                                                (GrenSyntax.Node { end = { column = 19, row = 5 }, start = { column = 18, row = 5 } }
                                                                    (GrenSyntax.Integer 2)
                                                                )
                                                            )
                                                    , name = GrenSyntax.Node { end = { column = 10, row = 5 }, start = { column = 1, row = 5 } } "pipeline1"
                                                    }
                                            , documentation = Nothing
                                            , signature = Nothing
                                            }
                                        )
                                    ]
                                , imports = []
                                , moduleDefinition =
                                    GrenSyntax.Node { end = { column = 23, row = 1 }, start = { column = 1, row = 1 } }
                                        (GrenSyntax.NormalModule
                                            { exposingList =
                                                GrenSyntax.Node { end = { column = 23, row = 1 }, start = { column = 10, row = 1 } }
                                                    (GrenSyntax.All { end = { column = 22, row = 1 }, start = { column = 20, row = 1 } })
                                            , moduleName = GrenSyntax.Node { end = { column = 9, row = 1 }, start = { column = 8, row = 1 } } [ "A" ]
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
                                    [ GrenSyntax.Node { end = { column = 19, row = 3 }, start = { column = 1, row = 3 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { declaration =
                                                GrenSyntax.Node { end = { column = 19, row = 3 }, start = { column = 1, row = 3 } }
                                                    { arguments = []
                                                    , expression =
                                                        GrenSyntax.Node { end = { column = 19, row = 3 }, start = { column = 13, row = 3 } }
                                                            (GrenSyntax.OperatorApplication "^"
                                                                (GrenSyntax.Node { end = { column = 14, row = 3 }, start = { column = 13, row = 3 } }
                                                                    (GrenSyntax.Integer 1)
                                                                )
                                                                (GrenSyntax.Node { end = { column = 19, row = 3 }, start = { column = 18, row = 3 } }
                                                                    (GrenSyntax.Integer 2)
                                                                )
                                                            )
                                                    , name = GrenSyntax.Node { end = { column = 10, row = 3 }, start = { column = 1, row = 3 } } "pipeline0"
                                                    }
                                            , documentation = Nothing
                                            , signature = Nothing
                                            }
                                        )
                                    , GrenSyntax.Node { end = { column = 18, row = 5 }, start = { column = 1, row = 5 } }
                                        (GrenSyntax.FunctionDeclaration
                                            { declaration =
                                                GrenSyntax.Node { end = { column = 18, row = 5 }, start = { column = 1, row = 5 } }
                                                    { arguments = []
                                                    , expression =
                                                        GrenSyntax.Node { end = { column = 18, row = 5 }, start = { column = 13, row = 5 } }
                                                            (GrenSyntax.OperatorApplication "^"
                                                                (GrenSyntax.Node { end = { column = 14, row = 5 }, start = { column = 13, row = 5 } }
                                                                    (GrenSyntax.Integer 1)
                                                                )
                                                                (GrenSyntax.Node { end = { column = 18, row = 5 }, start = { column = 17, row = 5 } }
                                                                    (GrenSyntax.Integer 2)
                                                                )
                                                            )
                                                    , name = GrenSyntax.Node { end = { column = 10, row = 5 }, start = { column = 1, row = 5 } } "pipeline1"
                                                    }
                                            , documentation = Nothing
                                            , signature = Nothing
                                            }
                                        )
                                    ]
                                , imports = []
                                , moduleDefinition =
                                    GrenSyntax.Node { end = { column = 23, row = 1 }, start = { column = 1, row = 1 } }
                                        (GrenSyntax.NormalModule
                                            { exposingList =
                                                GrenSyntax.Node { end = { column = 23, row = 1 }, start = { column = 10, row = 1 } }
                                                    (GrenSyntax.All { end = { column = 22, row = 1 }, start = { column = 20, row = 1 } })
                                            , moduleName = GrenSyntax.Node { end = { column = 9, row = 1 }, start = { column = 8, row = 1 } } [ "A" ]
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
    -> { syntax : a, comments : List (GrenSyntax.Node String) }
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
