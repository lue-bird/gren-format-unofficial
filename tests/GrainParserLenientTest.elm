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
                                (Just { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }, value = "--bar" })
                    )
                , Test.test "singleLineComment state"
                    (\() ->
                        GrenParserLenient.run GrenParserLenient.singleLineComment "--bar"
                            |> Expect.equal
                                (Just { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }, value = "--bar" })
                    )
                , Test.test "singleLineComment including 2-part utf-16 char range"
                    (\() ->
                        GrenParserLenient.run GrenParserLenient.singleLineComment """--bar\u{D83D}\u{DD27}"""
                            |> Expect.equal
                                (Just { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }, value = """--bar\u{D83D}\u{DD27}""" })
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
                                (Just
                                    { range = { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } }
                                    , value = "{-foo\nbar-}"
                                    }
                                )
                    )
                , Test.test "multilineComment range"
                    (\() ->
                        GrenParserLenient.run GrenParserLenient.multiLineComment "{-foo\nbar-}"
                            |> Expect.equal
                                (Just
                                    { range = { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } }
                                    , value = "{-foo\nbar-}"
                                    }
                                )
                    )
                , Test.test "multilineComment including 2-part utf-16 char range"
                    (\() ->
                        GrenParserLenient.run GrenParserLenient.multiLineComment
                            """{-foo
bar\u{D83D}\u{DD27}-}"""
                            |> Expect.equal
                                (Just
                                    { range = { start = { row = 1, column = 1 }, end = { row = 2, column = 7 } }
                                    , value =
                                        """{-foo
bar\u{D83D}\u{DD27}-}"""
                                    }
                                )
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
                                (Just
                                    { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                    , value = "{- {- -} -}"
                                    }
                                )
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
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 26 } }
                            , value =
                                GrenSyntax.NormalModule
                                    { moduleName =
                                        { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }, value = [ "Foo" ] }
                                    , exposingList =
                                        { range = { start = { row = 1, column = 12 }, end = { row = 1, column = 26 } }
                                        , value =
                                            GrenSyntax.Explicit
                                                [ { range = { start = { row = 1, column = 22 }, end = { row = 1, column = 25 } }
                                                  , value = GrenSyntax.TypeOrAliasExpose "Bar"
                                                  }
                                                ]
                                        }
                                    }
                            }
                )
            , Test.test "port moduleDefinition"
                (\() ->
                    "port module Foo exposing (Bar)"
                        |> expectSyntaxWithoutComments GrenParserLenient.moduleHeader
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 31 } }
                            , value =
                                GrenSyntax.PortModule
                                    { moduleName =
                                        { range = { start = { row = 1, column = 13 }, end = { row = 1, column = 16 } }, value = [ "Foo" ] }
                                    , exposingList =
                                        { range = { start = { row = 1, column = 17 }, end = { row = 1, column = 31 } }
                                        , value =
                                            GrenSyntax.Explicit
                                                [ { range = { start = { row = 1, column = 27 }, end = { row = 1, column = 30 } }
                                                  , value = GrenSyntax.TypeOrAliasExpose "Bar"
                                                  }
                                                ]
                                        }
                                    }
                            }
                )
            , Test.test "port moduleDefinition with spacing"
                (\() ->
                    "port module Foo exposing ( Bar )"
                        |> expectSyntaxWithoutComments GrenParserLenient.moduleHeader
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 33 } }
                            , value =
                                GrenSyntax.PortModule
                                    { moduleName =
                                        { range = { start = { row = 1, column = 13 }, end = { row = 1, column = 16 } }, value = [ "Foo" ] }
                                    , exposingList =
                                        { range = { start = { row = 1, column = 17 }, end = { row = 1, column = 33 } }
                                        , value =
                                            GrenSyntax.Explicit
                                                [ { range = { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } }
                                                  , value = GrenSyntax.TypeOrAliasExpose "Bar"
                                                  }
                                                ]
                                        }
                                    }
                            }
                )
            , Test.test "effect moduleDefinition"
                (\() ->
                    "effect module Foo where {command = MyCmd, subscription = MySub } exposing (Bar)"
                        |> expectSyntaxWithoutComments GrenParserLenient.moduleHeader
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 80 } }
                            , value =
                                GrenSyntax.EffectModule
                                    { moduleName =
                                        { range = { start = { row = 1, column = 15 }, end = { row = 1, column = 18 } }, value = [ "Foo" ] }
                                    , exposingList =
                                        { range = { start = { row = 1, column = 66 }, end = { row = 1, column = 80 } }
                                        , value =
                                            GrenSyntax.Explicit
                                                [ { range = { start = { row = 1, column = 76 }, end = { row = 1, column = 79 } }
                                                  , value = GrenSyntax.TypeOrAliasExpose "Bar"
                                                  }
                                                ]
                                        }
                                    , command =
                                        Just
                                            { range = { start = { row = 1, column = 36 }, end = { row = 1, column = 41 } }, value = "MyCmd" }
                                    , subscription =
                                        Just
                                            { range = { start = { row = 1, column = 58 }, end = { row = 1, column = 63 } }, value = "MySub" }
                                    }
                            }
                )
            , Test.test "unformatted"
                (\() ->
                    "module \n Foo \n exposing  (..)"
                        |> expectSyntaxWithoutComments GrenParserLenient.moduleHeader
                            { range = { start = { row = 1, column = 1 }, end = { row = 3, column = 16 } }
                            , value =
                                GrenSyntax.NormalModule
                                    { moduleName =
                                        { range = { start = { row = 2, column = 2 }, end = { row = 2, column = 5 } }, value = [ "Foo" ] }
                                    , exposingList =
                                        { range = { start = { row = 3, column = 2 }, end = { row = 3, column = 16 } }
                                        , value = GrenSyntax.All { start = { row = 3, column = 13 }, end = { row = 3, column = 15 } }
                                        }
                                    }
                            }
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
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                            , value =
                                GrenSyntax.NormalModule
                                    { moduleName =
                                        { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }, value = [ "Foo" ] }
                                    , exposingList =
                                        { range = { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } }
                                        , value = GrenSyntax.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                                        }
                                    }
                            }
                )
            , Test.test "exposing ..."
                (\() ->
                    "module Foo exposing (...)"
                        |> expectSyntaxWithoutComments GrenParserLenient.moduleHeader
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 26 } }
                            , value =
                                GrenSyntax.NormalModule
                                    { moduleName =
                                        { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }, value = [ "Foo" ] }
                                    , exposingList =
                                        { range = { start = { row = 1, column = 12 }, end = { row = 1, column = 26 } }
                                        , value = GrenSyntax.All { start = { row = 1, column = 22 }, end = { row = 1, column = 25 } }
                                        }
                                    }
                            }
                )
            , Test.test "module name with _"
                (\() ->
                    "module I_en_gb exposing (..)"
                        |> expectSyntaxWithoutComments GrenParserLenient.moduleHeader
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 29 } }
                            , value =
                                GrenSyntax.NormalModule
                                    { moduleName =
                                        { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 15 } }
                                        , value = [ "I_en_gb" ]
                                        }
                                    , exposingList =
                                        { range = { start = { row = 1, column = 16 }, end = { row = 1, column = 29 } }
                                        , value = GrenSyntax.All { start = { row = 1, column = 26 }, end = { row = 1, column = 28 } }
                                        }
                                    }
                            }
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
                                , comments = [ { range = { start = { row = 2, column = 6 }, end = { row = 2, column = 12 } }, value = "-- foo" } ]
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
                                    [ { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } }
                                      , value = GrenSyntax.TypeOrAliasExpose "Model"
                                      }
                                    , { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 15 } }
                                      , value =
                                            GrenSyntax.TypeExpose
                                                { name = "Msg"
                                                , open = Just { start = { row = 1, column = 11 }, end = { row = 1, column = 15 } }
                                                }
                                      }
                                    , { range = { start = { row = 1, column = 16 }, end = { row = 1, column = 24 } }
                                      , value =
                                            GrenSyntax.TypeExpose
                                                { name = "Info"
                                                , open = Just { start = { row = 1, column = 20 }, end = { row = 1, column = 24 } }
                                                }
                                      }
                                    , { range = { start = { row = 1, column = 25 }, end = { row = 1, column = 29 } }
                                      , value = GrenSyntax.FunctionExpose "init"
                                      }
                                    , { range = { start = { row = 1, column = 30 }, end = { row = 1, column = 34 } }
                                      , value = GrenSyntax.InfixExpose "::"
                                      }
                                    ]
                                )
                    )
                , Test.test "exposingList with spacing on one line"
                    (\() ->
                        "(Model, Msg, Info   (..)   ,init,(::) )"
                            |> expectSyntaxWithoutComments GrenParserLenient.exposing_
                                (GrenSyntax.Explicit
                                    [ { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } }
                                      , value = GrenSyntax.TypeOrAliasExpose "Model"
                                      }
                                    , { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } }
                                      , value = GrenSyntax.TypeOrAliasExpose "Msg"
                                      }
                                    , { range = { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } }
                                      , value =
                                            GrenSyntax.TypeExpose
                                                { name = "Info"
                                                , open = Just { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } }
                                                }
                                      }
                                    , { range = { start = { row = 1, column = 29 }, end = { row = 1, column = 33 } }
                                      , value = GrenSyntax.FunctionExpose "init"
                                      }
                                    , { range = { start = { row = 1, column = 34 }, end = { row = 1, column = 38 } }
                                      , value = GrenSyntax.InfixExpose "::"
                                      }
                                    ]
                                )
                    )
                , Test.test "exposingList with extra commas between exposes"
                    (\() ->
                        "(Model,,Msg,,Info   (..)  ,,init,(::) )"
                            |> expectSyntaxWithoutComments GrenParserLenient.exposing_
                                (GrenSyntax.Explicit
                                    [ { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } }
                                      , value = GrenSyntax.TypeOrAliasExpose "Model"
                                      }
                                    , { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } }
                                      , value = GrenSyntax.TypeOrAliasExpose "Msg"
                                      }
                                    , { range = { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } }
                                      , value =
                                            GrenSyntax.TypeExpose
                                                { name = "Info"
                                                , open = Just { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } }
                                                }
                                      }
                                    , { range = { start = { row = 1, column = 29 }, end = { row = 1, column = 33 } }
                                      , value = GrenSyntax.FunctionExpose "init"
                                      }
                                    , { range = { start = { row = 1, column = 34 }, end = { row = 1, column = 38 } }
                                      , value = GrenSyntax.InfixExpose "::"
                                      }
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
                                    [ { range = { start = { row = 2, column = 7 }, end = { row = 2, column = 8 } }
                                      , value = GrenSyntax.TypeOrAliasExpose "A"
                                      }
                                    , { range = { start = { row = 3, column = 7 }, end = { row = 3, column = 13 } }
                                      , value =
                                            GrenSyntax.TypeExpose
                                                { name = "B"
                                                , open = Just { start = { row = 3, column = 8 }, end = { row = 3, column = 13 } }
                                                }
                                      }
                                    , { range = { start = { row = 4, column = 7 }, end = { row = 4, column = 16 } }
                                      , value =
                                            GrenSyntax.TypeExpose
                                                { name = "Info"
                                                , open = Just { start = { row = 4, column = 12 }, end = { row = 4, column = 16 } }
                                                }
                                      }
                                    , { range = { start = { row = 5, column = 12 }, end = { row = 5, column = 16 } }
                                      , value = GrenSyntax.FunctionExpose "init"
                                      }
                                    , { range = { start = { row = 6, column = 2 }, end = { row = 6, column = 6 } }
                                      , value = GrenSyntax.InfixExpose "::"
                                      }
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
                                    [ { range = { start = { row = 2, column = 7 }, end = { row = 2, column = 8 } }
                                      , value = GrenSyntax.TypeOrAliasExpose "A"
                                      }
                                    , { range = { start = { row = 3, column = 7 }, end = { row = 3, column = 12 } }
                                      , value =
                                            GrenSyntax.TypeExpose
                                                { name = "B"
                                                , open = Just { start = { row = 3, column = 8 }, end = { row = 3, column = 12 } }
                                                }
                                      }
                                    , { range = { start = { row = 4, column = 7 }, end = { row = 4, column = 16 } }
                                      , value =
                                            GrenSyntax.TypeExpose
                                                { name = "Info"
                                                , open = Just { start = { row = 4, column = 12 }, end = { row = 4, column = 16 } }
                                                }
                                      }
                                    , { range = { start = { row = 5, column = 12 }, end = { row = 5, column = 16 } }
                                      , value = GrenSyntax.FunctionExpose "init"
                                      }
                                    , { range = { start = { row = 6, column = 2 }, end = { row = 6, column = 6 } }
                                      , value = GrenSyntax.InfixExpose "::"
                                      }
                                    ]
                                )
                    )
                , Test.test "Comments inside the exposing clause"
                    (\() ->
                        "(foo\n --bar\n )"
                            |> expectSyntaxWithComments GrenParserLenient.exposing_
                                { syntax =
                                    GrenSyntax.Explicit
                                        [ { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 5 } }
                                          , value = GrenSyntax.FunctionExpose "foo"
                                          }
                                        ]
                                , comments = [ { range = { start = { row = 2, column = 2 }, end = { row = 2, column = 7 } }, value = "--bar" } ]
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
                                    { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                                    , value =
                                        GrenSyntax.NormalModule
                                            { moduleName =
                                                { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 18 } }
                                                , value = [ "TestModule" ]
                                                }
                                            , exposingList =
                                                { range = { start = { row = 1, column = 19 }, end = { row = 1, column = 32 } }
                                                , value = GrenSyntax.All { start = { row = 1, column = 29 }, end = { row = 1, column = 31 } }
                                                }
                                            }
                                    }
                                , imports = []
                                , declarations =
                                    [ { range = { start = { row = 3, column = 1 }, end = { row = 7, column = 10 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { documentation = Nothing
                                                , signature = Nothing
                                                , declaration =
                                                    { range = { start = { row = 3, column = 1 }, end = { row = 7, column = 10 } }
                                                    , value =
                                                        { name = { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 2 } }, value = "a" }
                                                        , parameters = []
                                                        , expression =
                                                            { range = { start = { row = 4, column = 5 }, end = { row = 7, column = 10 } }
                                                            , value =
                                                                GrenSyntax.ExpressionIfThenElse
                                                                    { condition =
                                                                        { range = { start = { row = 4, column = 8 }, end = { row = 4, column = 12 } }
                                                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "cond" }
                                                                        }
                                                                    , onTrue =
                                                                        { range = { start = { row = 5, column = 9 }, end = { row = 5, column = 10 } }
                                                                        , value = GrenSyntax.ExpressionInteger 1
                                                                        }
                                                                    , onFalse =
                                                                        { range = { start = { row = 7, column = 9 }, end = { row = 7, column = 10 } }
                                                                        , value = GrenSyntax.ExpressionInteger 2
                                                                        }
                                                                    }
                                                            }
                                                        }
                                                    }
                                                }
                                      }
                                    , { range = { start = { row = 11, column = 1 }, end = { row = 13, column = 6 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { documentation =
                                                    Just
                                                        { range = { start = { row = 11, column = 1 }, end = { row = 12, column = 3 } }
                                                        , value = "{-| doc\n-}"
                                                        }
                                                , signature = Nothing
                                                , declaration =
                                                    { range = { start = { row = 13, column = 1 }, end = { row = 13, column = 6 } }
                                                    , value =
                                                        { name =
                                                            { range = { start = { row = 13, column = 1 }, end = { row = 13, column = 2 } }, value = "b" }
                                                        , parameters = []
                                                        , expression =
                                                            { range = { start = { row = 13, column = 5 }, end = { row = 13, column = 6 } }
                                                            , value = GrenSyntax.ExpressionInteger 3
                                                            }
                                                        }
                                                    }
                                                }
                                      }
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
                                    { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                                    , value =
                                        GrenSyntax.NormalModule
                                            { moduleName =
                                                { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 18 } }
                                                , value = [ "TestModule" ]
                                                }
                                            , exposingList =
                                                { range = { start = { row = 1, column = 19 }, end = { row = 1, column = 32 } }
                                                , value = GrenSyntax.All { start = { row = 1, column = 29 }, end = { row = 1, column = 31 } }
                                                }
                                            }
                                    }
                                , imports = []
                                , declarations =
                                    [ { range = { start = { row = 3, column = 1 }, end = { row = 4, column = 6 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { documentation = Nothing
                                                , signature = Nothing
                                                , declaration =
                                                    { range = { start = { row = 3, column = 1 }, end = { row = 4, column = 6 } }
                                                    , value =
                                                        { name = { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 2 } }, value = "a" }
                                                        , parameters = []
                                                        , expression =
                                                            { range = { start = { row = 4, column = 5 }, end = { row = 4, column = 6 } }
                                                            , value = GrenSyntax.ExpressionInteger 2
                                                            }
                                                        }
                                                    }
                                                }
                                      }
                                    , { range = { start = { row = 8, column = 1 }, end = { row = 10, column = 6 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { documentation =
                                                    Just
                                                        { range = { start = { row = 8, column = 1 }, end = { row = 9, column = 3 } }
                                                        , value = "{-| doc\n-}"
                                                        }
                                                , signature = Nothing
                                                , declaration =
                                                    { range = { start = { row = 10, column = 1 }, end = { row = 10, column = 6 } }
                                                    , value =
                                                        { name =
                                                            { range = { start = { row = 10, column = 1 }, end = { row = 10, column = 2 } }, value = "b" }
                                                        , parameters = []
                                                        , expression =
                                                            { range = { start = { row = 10, column = 5 }, end = { row = 10, column = 6 } }
                                                            , value = GrenSyntax.ExpressionInteger 3
                                                            }
                                                        }
                                                    }
                                                }
                                      }
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
                                    { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                                    , value =
                                        GrenSyntax.NormalModule
                                            { moduleName =
                                                { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 18 } }
                                                , value = [ "TestModule" ]
                                                }
                                            , exposingList =
                                                { range = { start = { row = 1, column = 19 }, end = { row = 1, column = 32 } }
                                                , value = GrenSyntax.All { start = { row = 1, column = 29 }, end = { row = 1, column = 31 } }
                                                }
                                            }
                                    }
                                , imports =
                                    [ { range = { start = { row = 2, column = 1 }, end = { row = 2, column = 9 } }
                                      , value =
                                            { moduleName =
                                                { range = { start = { row = 2, column = 8 }, end = { row = 2, column = 9 } }, value = [ "A" ] }
                                            , moduleAlias = Nothing
                                            , exposingList = Nothing
                                            }
                                      }
                                    , { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 9 } }
                                      , value =
                                            { moduleName =
                                                { range = { start = { row = 3, column = 8 }, end = { row = 3, column = 9 } }, value = [ "B" ] }
                                            , moduleAlias = Nothing
                                            , exposingList = Nothing
                                            }
                                      }
                                    ]
                                , declarations =
                                    [ { range = { start = { row = 5, column = 1 }, end = { row = 5, column = 6 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { documentation = Nothing
                                                , signature = Nothing
                                                , declaration =
                                                    { range = { start = { row = 5, column = 1 }, end = { row = 5, column = 6 } }
                                                    , value =
                                                        { name = { range = { start = { row = 5, column = 1 }, end = { row = 5, column = 2 } }, value = "a" }
                                                        , parameters = []
                                                        , expression =
                                                            { range = { start = { row = 5, column = 5 }, end = { row = 5, column = 6 } }
                                                            , value = GrenSyntax.ExpressionInteger 1
                                                            }
                                                        }
                                                    }
                                                }
                                      }
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
                                    { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                                    , value =
                                        GrenSyntax.NormalModule
                                            { moduleName =
                                                { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 18 } }
                                                , value = [ "TestModule" ]
                                                }
                                            , exposingList =
                                                { range = { start = { row = 1, column = 19 }, end = { row = 1, column = 32 } }
                                                , value = GrenSyntax.All { start = { row = 1, column = 29 }, end = { row = 1, column = 31 } }
                                                }
                                            }
                                    }
                                , imports = []
                                , declarations =
                                    [ { range = { start = { row = 2, column = 1 }, end = { row = 2, column = 15 } }
                                      , value =
                                            GrenSyntax.ChoiceTypeDeclaration
                                                { documentation = Nothing
                                                , name =
                                                    { range = { start = { row = 2, column = 6 }, end = { row = 2, column = 7 } }, value = "A" }
                                                , generics = []
                                                , constructors =
                                                    [ { range = { start = { row = 2, column = 10 }, end = { row = 2, column = 11 } }
                                                      , value =
                                                            { name =
                                                                { range = { start = { row = 2, column = 10 }, end = { row = 2, column = 11 } }, value = "B" }
                                                            , value = Nothing
                                                            }
                                                      }
                                                    , { range = { start = { row = 2, column = 14 }, end = { row = 2, column = 15 } }
                                                      , value =
                                                            { name =
                                                                { range = { start = { row = 2, column = 14 }, end = { row = 2, column = 15 } }, value = "C" }
                                                            , value = Nothing
                                                            }
                                                      }
                                                    ]
                                                }
                                      }
                                    , { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 6 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { documentation = Nothing
                                                , signature = Nothing
                                                , declaration =
                                                    { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 6 } }
                                                    , value =
                                                        { name = { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 2 } }, value = "a" }
                                                        , parameters = []
                                                        , expression =
                                                            { range = { start = { row = 3, column = 5 }, end = { row = 3, column = 6 } }
                                                            , value = GrenSyntax.ExpressionInteger 1
                                                            }
                                                        }
                                                    }
                                                }
                                      }
                                    , { range = { start = { row = 4, column = 1 }, end = { row = 4, column = 17 } }
                                      , value =
                                            GrenSyntax.AliasDeclaration
                                                { documentation = Nothing
                                                , name =
                                                    { range = { start = { row = 4, column = 12 }, end = { row = 4, column = 13 } }, value = "B" }
                                                , generics = []
                                                , typeAnnotation =
                                                    { range = { start = { row = 4, column = 16 }, end = { row = 4, column = 17 } }
                                                    , value =
                                                        GrenSyntax.TypeAnnotationConstruct
                                                            { reference =
                                                                { range = { start = { row = 4, column = 16 }, end = { row = 4, column = 17 } }
                                                                , value = { qualification = [], name = "A" }
                                                                }
                                                            , arguments = []
                                                            }
                                                    }
                                                }
                                      }
                                    , { range = { start = { row = 5, column = 1 }, end = { row = 6, column = 6 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { documentation = Nothing
                                                , signature =
                                                    Just
                                                        { range = { start = { row = 5, column = 1 }, end = { row = 5, column = 8 } }
                                                        , value =
                                                            { name = { range = { start = { row = 5, column = 1 }, end = { row = 5, column = 2 } }, value = "b" }
                                                            , typeAnnotation =
                                                                { range = { start = { row = 5, column = 5 }, end = { row = 5, column = 8 } }
                                                                , value =
                                                                    GrenSyntax.TypeAnnotationConstruct
                                                                        { reference =
                                                                            { range = { start = { row = 5, column = 5 }, end = { row = 5, column = 8 } }
                                                                            , value = { qualification = [], name = "Int" }
                                                                            }
                                                                        , arguments = []
                                                                        }
                                                                }
                                                            }
                                                        }
                                                , declaration =
                                                    { range = { start = { row = 6, column = 1 }, end = { row = 6, column = 6 } }
                                                    , value =
                                                        { name = { range = { start = { row = 6, column = 1 }, end = { row = 6, column = 2 } }, value = "b" }
                                                        , parameters = []
                                                        , expression =
                                                            { range = { start = { row = 6, column = 5 }, end = { row = 6, column = 6 } }
                                                            , value = GrenSyntax.ExpressionInteger 2
                                                            }
                                                        }
                                                    }
                                                }
                                      }
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
                                { moduleDefinition =
                                    { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 31 } }
                                    , value =
                                        GrenSyntax.NormalModule
                                            { moduleName =
                                                { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } }, value = [ "A" ] }
                                            , exposingList =
                                                { range = { start = { row = 1, column = 10 }, end = { row = 1, column = 31 } }
                                                , value =
                                                    GrenSyntax.Explicit
                                                        [ { range = { start = { row = 1, column = 20 }, end = { row = 1, column = 24 } }
                                                          , value = GrenSyntax.FunctionExpose "fun1"
                                                          }
                                                        , { range = { start = { row = 1, column = 26 }, end = { row = 1, column = 30 } }
                                                          , value = GrenSyntax.FunctionExpose "fun2"
                                                          }
                                                        ]
                                                }
                                            }
                                    }
                                , imports = []
                                , declarations =
                                    [ { range = { start = { row = 3, column = 1 }, end = { row = 5, column = 11 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { documentation = Nothing
                                                , signature = Nothing
                                                , declaration =
                                                    { range = { start = { row = 3, column = 1 }, end = { row = 5, column = 11 } }
                                                    , value =
                                                        { name =
                                                            { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } }, value = "fun1" }
                                                        , parameters =
                                                            [ { range = { start = { row = 3, column = 6 }, end = { row = 3, column = 7 } }
                                                              , value = GrenSyntax.PatternVariable "n"
                                                              }
                                                            ]
                                                        , expression =
                                                            { range = { start = { row = 4, column = 3 }, end = { row = 5, column = 11 } }
                                                            , value =
                                                                GrenSyntax.ExpressionInfixOperation
                                                                    { operator = "+"
                                                                    , left =
                                                                        { range = { start = { row = 4, column = 3 }, end = { row = 4, column = 9 } }
                                                                        , value =
                                                                            GrenSyntax.ExpressionCall
                                                                                [ { range = { start = { row = 4, column = 3 }, end = { row = 4, column = 7 } }
                                                                                  , value = GrenSyntax.ExpressionReference { qualification = [], name = "fun2" }
                                                                                  }
                                                                                , { range = { start = { row = 4, column = 8 }, end = { row = 4, column = 9 } }
                                                                                  , value = GrenSyntax.ExpressionReference { qualification = [], name = "n" }
                                                                                  }
                                                                                ]
                                                                        }
                                                                    , right =
                                                                        { range = { start = { row = 5, column = 5 }, end = { row = 5, column = 11 } }
                                                                        , value =
                                                                            GrenSyntax.ExpressionCall
                                                                                [ { range = { start = { row = 5, column = 5 }, end = { row = 5, column = 9 } }
                                                                                  , value = GrenSyntax.ExpressionReference { qualification = [], name = "fun2" }
                                                                                  }
                                                                                , { range = { start = { row = 5, column = 10 }, end = { row = 5, column = 11 } }
                                                                                  , value = GrenSyntax.ExpressionReference { qualification = [], name = "n" }
                                                                                  }
                                                                                ]
                                                                        }
                                                                    }
                                                            }
                                                        }
                                                    }
                                                }
                                      }
                                    , { range = { start = { row = 7, column = 1 }, end = { row = 8, column = 9 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { documentation = Nothing
                                                , signature = Nothing
                                                , declaration =
                                                    { range = { start = { row = 7, column = 1 }, end = { row = 8, column = 9 } }
                                                    , value =
                                                        { name =
                                                            { range = { start = { row = 7, column = 1 }, end = { row = 7, column = 5 } }, value = "fun2" }
                                                        , parameters =
                                                            [ { range = { start = { row = 7, column = 6 }, end = { row = 7, column = 7 } }
                                                              , value = GrenSyntax.PatternVariable "n"
                                                              }
                                                            ]
                                                        , expression =
                                                            { range = { start = { row = 8, column = 3 }, end = { row = 8, column = 9 } }
                                                            , value =
                                                                GrenSyntax.ExpressionCall
                                                                    [ { range = { start = { row = 8, column = 3 }, end = { row = 8, column = 7 } }
                                                                      , value = GrenSyntax.ExpressionReference { qualification = [], name = "fun1" }
                                                                      }
                                                                    , { range = { start = { row = 8, column = 8 }, end = { row = 8, column = 9 } }
                                                                      , value = GrenSyntax.ExpressionReference { qualification = [], name = "n" }
                                                                      }
                                                                    ]
                                                            }
                                                        }
                                                    }
                                                }
                                      }
                                    ]
                                , comments = [ { range = { start = { row = 5, column = 13 }, end = { row = 5, column = 17 } }, value = "-- a" }, { range = { start = { row = 8, column = 13 }, end = { row = 8, column = 17 } }, value = "-- b" } ]
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
                                { moduleDefinition =
                                    { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 31 } }
                                    , value =
                                        GrenSyntax.NormalModule
                                            { moduleName =
                                                { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } }, value = [ "A" ] }
                                            , exposingList =
                                                { range = { start = { row = 1, column = 10 }, end = { row = 1, column = 31 } }
                                                , value =
                                                    GrenSyntax.Explicit
                                                        [ { range = { start = { row = 1, column = 20 }, end = { row = 1, column = 24 } }
                                                          , value = GrenSyntax.FunctionExpose "fun1"
                                                          }
                                                        , { range = { start = { row = 1, column = 26 }, end = { row = 1, column = 30 } }
                                                          , value = GrenSyntax.FunctionExpose "fun2"
                                                          }
                                                        ]
                                                }
                                            }
                                    }
                                , imports =
                                    [ { range = { end = { column = 1, row = 3 }, start = { column = 1, row = 3 } }
                                      , value =
                                            { exposingList = Nothing
                                            , moduleAlias = Nothing
                                            , moduleName =
                                                { range = { end = { column = 9, row = 5 }, start = { column = 8, row = 5 } }, value = [ "B" ] }
                                            }
                                      }
                                    ]
                                , declarations =
                                    [ { range = { start = { row = 3, column = 1 }, end = { row = 4, column = 9 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { documentation = Nothing
                                                , signature = Nothing
                                                , declaration =
                                                    { range = { start = { row = 3, column = 1 }, end = { row = 4, column = 9 } }
                                                    , value =
                                                        { name =
                                                            { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } }, value = "fun1" }
                                                        , parameters =
                                                            [ { range = { start = { row = 3, column = 6 }, end = { row = 3, column = 7 } }
                                                              , value = GrenSyntax.PatternVariable "n"
                                                              }
                                                            ]
                                                        , expression =
                                                            { range = { start = { row = 4, column = 3 }, end = { row = 4, column = 9 } }
                                                            , value =
                                                                GrenSyntax.ExpressionCall
                                                                    [ { range = { start = { row = 4, column = 3 }, end = { row = 4, column = 7 } }
                                                                      , value = GrenSyntax.ExpressionReference { qualification = [], name = "fun2" }
                                                                      }
                                                                    , { range = { start = { row = 4, column = 8 }, end = { row = 4, column = 9 } }
                                                                      , value = GrenSyntax.ExpressionReference { qualification = [], name = "n" }
                                                                      }
                                                                    ]
                                                            }
                                                        }
                                                    }
                                                }
                                      }
                                    , { range = { start = { row = 7, column = 1 }, end = { row = 8, column = 9 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { documentation = Nothing
                                                , signature = Nothing
                                                , declaration =
                                                    { range = { start = { row = 7, column = 1 }, end = { row = 8, column = 9 } }
                                                    , value =
                                                        { name =
                                                            { range = { start = { row = 7, column = 1 }, end = { row = 7, column = 5 } }, value = "fun2" }
                                                        , parameters =
                                                            [ { range = { start = { row = 7, column = 6 }, end = { row = 7, column = 7 } }
                                                              , value = GrenSyntax.PatternVariable "n"
                                                              }
                                                            ]
                                                        , expression =
                                                            { range = { start = { row = 8, column = 3 }, end = { row = 8, column = 9 } }
                                                            , value =
                                                                GrenSyntax.ExpressionCall
                                                                    [ { range = { start = { row = 8, column = 3 }, end = { row = 8, column = 7 } }
                                                                      , value = GrenSyntax.ExpressionReference { qualification = [], name = "fun1" }
                                                                      }
                                                                    , { range = { start = { row = 8, column = 8 }, end = { row = 8, column = 9 } }
                                                                      , value = GrenSyntax.ExpressionReference { qualification = [], name = "n" }
                                                                      }
                                                                    ]
                                                            }
                                                        }
                                                    }
                                                }
                                      }
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
                                { moduleDefinition =
                                    { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 31 } }
                                    , value =
                                        GrenSyntax.NormalModule
                                            { moduleName =
                                                { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } }, value = [ "A" ] }
                                            , exposingList =
                                                { range = { start = { row = 1, column = 10 }, end = { row = 1, column = 31 } }
                                                , value =
                                                    GrenSyntax.Explicit
                                                        [ { range = { start = { row = 1, column = 20 }, end = { row = 1, column = 24 } }
                                                          , value = GrenSyntax.FunctionExpose "fun1"
                                                          }
                                                        , { range = { start = { row = 1, column = 26 }, end = { row = 1, column = 30 } }
                                                          , value = GrenSyntax.FunctionExpose "fun2"
                                                          }
                                                        ]
                                                }
                                            }
                                    }
                                , imports =
                                    [ { range = { end = { column = 1, row = 2 }, start = { column = 1, row = 2 } }
                                      , value =
                                            { exposingList = Nothing
                                            , moduleAlias = Nothing
                                            , moduleName =
                                                { range = { end = { column = 9, row = 5 }, start = { column = 8, row = 5 } }, value = [ "B" ] }
                                            }
                                      }
                                    , { range = { end = { column = 9, row = 2 }, start = { column = 1, row = 2 } }
                                      , value =
                                            { exposingList = Nothing
                                            , moduleAlias = Nothing
                                            , moduleName =
                                                { range = { end = { column = 9, row = 2 }, start = { column = 8, row = 2 } }, value = [ "C" ] }
                                            }
                                      }
                                    ]
                                , declarations =
                                    [ { range = { start = { row = 3, column = 1 }, end = { row = 4, column = 9 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { documentation = Nothing
                                                , signature = Nothing
                                                , declaration =
                                                    { range = { start = { row = 3, column = 1 }, end = { row = 4, column = 9 } }
                                                    , value =
                                                        { name =
                                                            { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } }, value = "fun1" }
                                                        , parameters =
                                                            [ { range = { start = { row = 3, column = 6 }, end = { row = 3, column = 7 } }
                                                              , value = GrenSyntax.PatternVariable "n"
                                                              }
                                                            ]
                                                        , expression =
                                                            { range = { start = { row = 4, column = 3 }, end = { row = 4, column = 9 } }
                                                            , value =
                                                                GrenSyntax.ExpressionCall
                                                                    [ { range = { start = { row = 4, column = 3 }, end = { row = 4, column = 7 } }
                                                                      , value = GrenSyntax.ExpressionReference { qualification = [], name = "fun2" }
                                                                      }
                                                                    , { range = { start = { row = 4, column = 8 }, end = { row = 4, column = 9 } }
                                                                      , value = GrenSyntax.ExpressionReference { qualification = [], name = "n" }
                                                                      }
                                                                    ]
                                                            }
                                                        }
                                                    }
                                                }
                                      }
                                    , { range = { start = { row = 7, column = 1 }, end = { row = 8, column = 9 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { documentation = Nothing
                                                , signature = Nothing
                                                , declaration =
                                                    { range = { start = { row = 7, column = 1 }, end = { row = 8, column = 9 } }
                                                    , value =
                                                        { name =
                                                            { range = { start = { row = 7, column = 1 }, end = { row = 7, column = 5 } }, value = "fun2" }
                                                        , parameters =
                                                            [ { range = { start = { row = 7, column = 6 }, end = { row = 7, column = 7 } }
                                                              , value = GrenSyntax.PatternVariable "n"
                                                              }
                                                            ]
                                                        , expression =
                                                            { range = { start = { row = 8, column = 3 }, end = { row = 8, column = 9 } }
                                                            , value =
                                                                GrenSyntax.ExpressionCall
                                                                    [ { range = { start = { row = 8, column = 3 }, end = { row = 8, column = 7 } }
                                                                      , value = GrenSyntax.ExpressionReference { qualification = [], name = "fun1" }
                                                                      }
                                                                    , { range = { start = { row = 8, column = 8 }, end = { row = 8, column = 9 } }
                                                                      , value = GrenSyntax.ExpressionReference { qualification = [], name = "n" }
                                                                      }
                                                                    ]
                                                            }
                                                        }
                                                    }
                                                }
                                      }
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
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 37 } }
                            , value =
                                { moduleName =
                                    { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }, value = [ "Foo" ] }
                                , moduleAlias = Nothing
                                , exposingList =
                                    Just
                                        { range = { start = { row = 1, column = 12 }, end = { row = 1, column = 37 } }
                                        , value =
                                            GrenSyntax.Explicit
                                                [ { range = { start = { row = 1, column = 22 }, end = { row = 1, column = 27 } }
                                                  , value = GrenSyntax.TypeOrAliasExpose "Model"
                                                  }
                                                , { range = { start = { row = 1, column = 29 }, end = { row = 1, column = 36 } }
                                                  , value =
                                                        GrenSyntax.TypeExpose
                                                            { name = "Msg"
                                                            , open = Just { start = { row = 1, column = 32 }, end = { row = 1, column = 36 } }
                                                            }
                                                  }
                                                ]
                                        }
                                }
                            }
                )
            , Test.test "import with explicits 2"
                (\() ->
                    "import Html exposing (text)"
                        |> expectSyntaxWithoutComments GrenParserLenient.import_
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 28 } }
                            , value =
                                { moduleName =
                                    { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 12 } }, value = [ "Html" ] }
                                , moduleAlias = Nothing
                                , exposingList =
                                    Just
                                        { range = { start = { row = 1, column = 13 }, end = { row = 1, column = 28 } }
                                        , value =
                                            GrenSyntax.Explicit
                                                [ { range = { start = { row = 1, column = 23 }, end = { row = 1, column = 27 } }
                                                  , value = GrenSyntax.FunctionExpose "text"
                                                  }
                                                ]
                                        }
                                }
                            }
                )
            , Test.test "import with exposing ()"
                (\() ->
                    "import Html exposing ()"
                        |> expectSyntaxWithoutComments GrenParserLenient.import_
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                            , value =
                                { moduleName =
                                    { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 12 } }, value = [ "Html" ] }
                                , moduleAlias = Nothing
                                , exposingList = Nothing
                                }
                            }
                )
            , Test.test "import with alias and exposing ()"
                (\() ->
                    "import Html as H exposing ()"
                        |> expectSyntaxWithoutComments GrenParserLenient.import_
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 17 } }
                            , value =
                                { moduleName =
                                    { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 12 } }, value = [ "Html" ] }
                                , moduleAlias =
                                    Just
                                        { range = { start = { row = 1, column = 16 }, end = { row = 1, column = 17 } }, value = [ "H" ] }
                                , exposingList = Nothing
                                }
                            }
                )
            , Test.test "import minimal"
                (\() ->
                    "import Foo"
                        |> expectSyntaxWithoutComments GrenParserLenient.import_
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                            , value =
                                { moduleName =
                                    { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }, value = [ "Foo" ] }
                                , moduleAlias = Nothing
                                , exposingList = Nothing
                                }
                            }
                )
            , Test.test "import with alias"
                (\() ->
                    "import Foo as Bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.import_
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 18 } }
                            , value =
                                { moduleName =
                                    { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }, value = [ "Foo" ] }
                                , moduleAlias =
                                    Just
                                        { range = { start = { row = 1, column = 15 }, end = { row = 1, column = 18 } }, value = [ "Bar" ] }
                                , exposingList = Nothing
                                }
                            }
                )
            , Test.test "import with alias and exposing .."
                (\() ->
                    "import Foo as Bar exposing (..)"
                        |> expectSyntaxWithoutComments GrenParserLenient.import_
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                            , value =
                                { moduleName =
                                    { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }, value = [ "Foo" ] }
                                , moduleAlias =
                                    Just
                                        { range = { start = { row = 1, column = 15 }, end = { row = 1, column = 18 } }, value = [ "Bar" ] }
                                , exposingList =
                                    Just
                                        { range = { start = { row = 1, column = 19 }, end = { row = 1, column = 32 } }
                                        , value = GrenSyntax.All { start = { row = 1, column = 29 }, end = { row = 1, column = 31 } }
                                        }
                                }
                            }
                )
            , Test.test "import with alias and exposing ..."
                (\() ->
                    "import Foo as Bar exposing (...)"
                        |> expectSyntaxWithoutComments GrenParserLenient.import_
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 33 } }
                            , value =
                                { moduleName =
                                    { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }, value = [ "Foo" ] }
                                , moduleAlias =
                                    Just
                                        { range = { start = { row = 1, column = 15 }, end = { row = 1, column = 18 } }, value = [ "Bar" ] }
                                , exposingList =
                                    Just
                                        { range = { start = { row = 1, column = 19 }, end = { row = 1, column = 33 } }
                                        , value = GrenSyntax.All { start = { row = 1, column = 29 }, end = { row = 1, column = 32 } }
                                        }
                                }
                            }
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
                            [ { range = { start = { row = 4, column = 1 }, end = { row = 5, column = 3 } }
                              , value = "{-| Module documentation\n-}"
                              }
                            , { range = { start = { row = 9, column = 1 }, end = { row = 9, column = 5 } }, value = "-- 1" }
                            , { range = { start = { row = 10, column = 1 }, end = { row = 10, column = 8 } }, value = "{- 2 -}" }
                            , { range = { start = { row = 11, column = 1 }, end = { row = 11, column = 5 } }, value = "-- 3" }
                            , { range = { start = { row = 16, column = 5 }, end = { row = 16, column = 9 } }, value = "-- 4" }
                            , { range = { start = { row = 19, column = 1 }, end = { row = 19, column = 5 } }, value = "-- 5" }
                            , { range = { start = { row = 20, column = 1 }, end = { row = 20, column = 8 } }, value = "{- 6 -}" }
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
                            [ { range = { start = { row = 7, column = 17 }, end = { row = 7, column = 21 } }, value = "-- 1" }
                            , { range = { start = { row = 13, column = 1 }, end = { row = 13, column = 5 } }, value = "-- 2" }
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
                                { range = { start = { row = 2, column = 1 }, end = { row = 2, column = 41 } }
                                , value =
                                    GrenSyntax.NormalModule
                                        { moduleName =
                                            { range = { start = { row = 2, column = 8 }, end = { row = 2, column = 27 } }
                                            , value = [ "Trailing", "Whitespace" ]
                                            }
                                        , exposingList =
                                            { range = { start = { row = 2, column = 28 }, end = { row = 2, column = 41 } }
                                            , value = GrenSyntax.All { start = { row = 2, column = 38 }, end = { row = 2, column = 40 } }
                                            }
                                        }
                                }
                            , imports = []
                            , declarations =
                                [ { range = { start = { row = 4, column = 1 }, end = { row = 11, column = 11 } }
                                  , value =
                                        GrenSyntax.ValueOrFunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                { range = { start = { row = 4, column = 1 }, end = { row = 11, column = 11 } }
                                                , value =
                                                    { name =
                                                        { range = { start = { row = 4, column = 1 }, end = { row = 4, column = 15 } }
                                                        , value = "caseWhitespace"
                                                        }
                                                    , parameters =
                                                        [ { range = { start = { row = 4, column = 16 }, end = { row = 4, column = 17 } }
                                                          , value = GrenSyntax.PatternVariable "f"
                                                          }
                                                        ]
                                                    , expression =
                                                        { range = { start = { row = 4, column = 20 }, end = { row = 11, column = 11 } }
                                                        , value =
                                                            GrenSyntax.ExpressionCaseOf
                                                                { expression =
                                                                    { range = { start = { row = 4, column = 25 }, end = { row = 4, column = 26 } }
                                                                    , value = GrenSyntax.ExpressionReference { qualification = [], name = "f" }
                                                                    }
                                                                , cases =
                                                                    [ { pattern =
                                                                            { range = { start = { row = 5, column = 3 }, end = { row = 5, column = 7 } }
                                                                            , value = GrenSyntax.PatternVariant { qualification = [], name = "True", value = Nothing }
                                                                            }
                                                                      , result =
                                                                            { range = { start = { row = 6, column = 5 }, end = { row = 6, column = 6 } }
                                                                            , value = GrenSyntax.ExpressionInteger 1
                                                                            }
                                                                      }
                                                                    , { pattern =
                                                                            { range = { start = { row = 7, column = 3 }, end = { row = 7, column = 8 } }
                                                                            , value = GrenSyntax.PatternVariant { qualification = [], name = "False", value = Nothing }
                                                                            }
                                                                      , result =
                                                                            { range = { start = { row = 11, column = 10 }, end = { row = 11, column = 11 } }
                                                                            , value = GrenSyntax.ExpressionInteger 2
                                                                            }
                                                                      }
                                                                    ]
                                                                }
                                                        }
                                                    }
                                                }
                                            }
                                  }
                                ]
                            , comments =
                                [ { range = { start = { row = 13, column = 4 }, end = { row = 13, column = 18 } }
                                  , value = "--some comment"
                                  }
                                ]
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
                                { range = { start = { row = 2, column = 1 }, end = { row = 2, column = 41 } }
                                , value =
                                    GrenSyntax.NormalModule
                                        { moduleName =
                                            { range = { start = { row = 2, column = 8 }, end = { row = 2, column = 27 } }
                                            , value = [ "Trailing", "Whitespace" ]
                                            }
                                        , exposingList =
                                            { range = { start = { row = 2, column = 28 }, end = { row = 2, column = 41 } }
                                            , value = GrenSyntax.All { start = { row = 2, column = 38 }, end = { row = 2, column = 40 } }
                                            }
                                        }
                                }
                            , imports = []
                            , declarations =
                                [ { range = { start = { row = 4, column = 1 }, end = { row = 8, column = 6 } }
                                  , value =
                                        GrenSyntax.ValueOrFunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                { range = { start = { row = 4, column = 1 }, end = { row = 8, column = 6 } }
                                                , value =
                                                    { name =
                                                        { range = { start = { row = 4, column = 1 }, end = { row = 4, column = 17 } }
                                                        , value = "lambdaWhitespace"
                                                        }
                                                    , parameters = []
                                                    , expression =
                                                        { range = { start = { row = 4, column = 22 }, end = { row = 8, column = 6 } }
                                                        , value =
                                                            GrenSyntax.ExpressionLambda
                                                                { parameters =
                                                                    [ { range = { start = { row = 4, column = 24 }, end = { row = 4, column = 25 } }
                                                                      , value = GrenSyntax.PatternVariable "a"
                                                                      }
                                                                    , { range = { start = { row = 4, column = 26 }, end = { row = 4, column = 27 } }
                                                                      , value = GrenSyntax.PatternVariable "b"
                                                                      }
                                                                    ]
                                                                , result =
                                                                    { range = { start = { row = 4, column = 34 }, end = { row = 8, column = 6 } }
                                                                    , value =
                                                                        GrenSyntax.ExpressionInfixOperation
                                                                            { operator = "+"
                                                                            , left =
                                                                                { range = { start = { row = 4, column = 34 }, end = { row = 4, column = 35 } }
                                                                                , value = GrenSyntax.ExpressionReference { qualification = [], name = "a" }
                                                                                }
                                                                            , right =
                                                                                { range = { start = { row = 8, column = 5 }, end = { row = 8, column = 6 } }
                                                                                , value = GrenSyntax.ExpressionReference { qualification = [], name = "b" }
                                                                                }
                                                                            }
                                                                    }
                                                                }
                                                        }
                                                    }
                                                }
                                            }
                                  }
                                ]
                            , comments =
                                [ { range = { start = { row = 11, column = 1 }, end = { row = 11, column = 15 } }
                                  , value = "--some comment"
                                  }
                                ]
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
                                { range = { start = { row = 2, column = 1 }, end = { row = 2, column = 41 } }
                                , value =
                                    GrenSyntax.NormalModule
                                        { moduleName =
                                            { range = { start = { row = 2, column = 8 }, end = { row = 2, column = 27 } }
                                            , value = [ "Trailing", "Whitespace" ]
                                            }
                                        , exposingList =
                                            { range = { start = { row = 2, column = 28 }, end = { row = 2, column = 41 } }
                                            , value = GrenSyntax.All { start = { row = 2, column = 38 }, end = { row = 2, column = 40 } }
                                            }
                                        }
                                }
                            , imports = []
                            , declarations =
                                [ { range = { start = { row = 4, column = 1 }, end = { row = 8, column = 3 } }
                                  , value =
                                        GrenSyntax.ValueOrFunctionDeclaration
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                { range = { start = { row = 4, column = 1 }, end = { row = 8, column = 3 } }
                                                , value =
                                                    { name =
                                                        { range = { start = { row = 4, column = 1 }, end = { row = 4, column = 14 } }
                                                        , value = "letWhitespace"
                                                        }
                                                    , parameters = []
                                                    , expression =
                                                        { range = { start = { row = 4, column = 17 }, end = { row = 8, column = 3 } }
                                                        , value =
                                                            GrenSyntax.ExpressionLetIn
                                                                { declarations =
                                                                    [ { range = { start = { row = 5, column = 19 }, end = { row = 5, column = 26 } }
                                                                      , value =
                                                                            GrenSyntax.LetFunction
                                                                                { documentation = Nothing
                                                                                , signature = Nothing
                                                                                , declaration =
                                                                                    { range = { start = { row = 5, column = 19 }, end = { row = 5, column = 26 } }
                                                                                    , value =
                                                                                        { name =
                                                                                            { range = { start = { row = 5, column = 19 }, end = { row = 5, column = 20 } }, value = "b" }
                                                                                        , parameters = []
                                                                                        , expression =
                                                                                            { range = { start = { row = 5, column = 25 }, end = { row = 5, column = 26 } }
                                                                                            , value = GrenSyntax.ExpressionInteger 1
                                                                                            }
                                                                                        }
                                                                                    }
                                                                                }
                                                                      }
                                                                    ]
                                                                , result =
                                                                    { range = { start = { row = 8, column = 2 }, end = { row = 8, column = 3 } }
                                                                    , value = GrenSyntax.ExpressionReference { qualification = [], name = "b" }
                                                                    }
                                                                }
                                                        }
                                                    }
                                                }
                                            }
                                  }
                                ]
                            , comments =
                                [ { range = { start = { row = 11, column = 1 }, end = { row = 11, column = 15 } }
                                  , value = "--some comment"
                                  }
                                ]
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
                                { range = { start = { row = 2, column = 1 }, end = { row = 2, column = 25 } }
                                , value =
                                    GrenSyntax.NormalModule
                                        { moduleName =
                                            { range = { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } }, value = [ "Foo" ] }
                                        , exposingList =
                                            { range = { start = { row = 2, column = 12 }, end = { row = 2, column = 25 } }
                                            , value = GrenSyntax.All { start = { row = 2, column = 22 }, end = { row = 2, column = 24 } }
                                            }
                                        }
                                }
                            , imports =
                                [ { range = { start = { row = 4, column = 1 }, end = { row = 4, column = 12 } }
                                  , value =
                                        { moduleName =
                                            { range = { start = { row = 4, column = 8 }, end = { row = 4, column = 12 } }, value = [ "Dict" ] }
                                        , moduleAlias = Nothing
                                        , exposingList = Nothing
                                        }
                                  }
                                ]
                            , declarations =
                                [ { range = { start = { row = 6, column = 1 }, end = { row = 9, column = 20 } }
                                  , value =
                                        GrenSyntax.ChoiceTypeDeclaration
                                            { documentation =
                                                Just
                                                    { range = { start = { row = 6, column = 1 }, end = { row = 7, column = 3 } }
                                                    , value = "{-| Config goes here\n-}"
                                                    }
                                            , name =
                                                { range = { start = { row = 8, column = 6 }, end = { row = 8, column = 19 } }
                                                , value = "Configuration"
                                                }
                                            , generics = []
                                            , constructors =
                                                [ { range = { start = { row = 9, column = 7 }, end = { row = 9, column = 20 } }
                                                  , value =
                                                        { name =
                                                            { range = { start = { row = 9, column = 7 }, end = { row = 9, column = 20 } }
                                                            , value = "Configuration"
                                                            }
                                                        , value = Nothing
                                                        }
                                                  }
                                                ]
                                            }
                                  }
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
                                { range = { start = { row = 2, column = 1 }, end = { row = 2, column = 25 } }
                                , value =
                                    GrenSyntax.NormalModule
                                        { moduleName =
                                            { range = { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } }, value = [ "Foo" ] }
                                        , exposingList =
                                            { range = { start = { row = 2, column = 12 }, end = { row = 2, column = 25 } }
                                            , value = GrenSyntax.All { start = { row = 2, column = 22 }, end = { row = 2, column = 24 } }
                                            }
                                        }
                                }
                            , imports = []
                            , declarations =
                                [ { range = { start = { row = 6, column = 1 }, end = { row = 7, column = 20 } }
                                  , value =
                                        GrenSyntax.ChoiceTypeDeclaration
                                            { documentation = Nothing
                                            , name =
                                                { range = { start = { row = 6, column = 6 }, end = { row = 6, column = 19 } }
                                                , value = "Configuration"
                                                }
                                            , generics = []
                                            , constructors =
                                                [ { range = { start = { row = 7, column = 7 }, end = { row = 7, column = 20 } }
                                                  , value =
                                                        { name =
                                                            { range = { start = { row = 7, column = 7 }, end = { row = 7, column = 20 } }
                                                            , value = "Configuration"
                                                            }
                                                        , value = Nothing
                                                        }
                                                  }
                                                ]
                                            }
                                  }
                                ]
                            , comments =
                                [ { range = { start = { row = 4, column = 1 }, end = { row = 5, column = 3 } }
                                  , value = "{-| actually module doc\n-}"
                                  }
                                ]
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
                                { range = { start = { row = 2, column = 1 }, end = { row = 2, column = 30 } }
                                , value =
                                    GrenSyntax.PortModule
                                        { moduleName =
                                            { range = { start = { row = 2, column = 13 }, end = { row = 2, column = 16 } }, value = [ "Foo" ] }
                                        , exposingList =
                                            { range = { start = { row = 2, column = 17 }, end = { row = 2, column = 30 } }
                                            , value = GrenSyntax.All { start = { row = 2, column = 27 }, end = { row = 2, column = 29 } }
                                            }
                                        }
                                }
                            , imports =
                                [ { range = { start = { row = 4, column = 1 }, end = { row = 4, column = 14 } }
                                  , value =
                                        { moduleName =
                                            { range = { start = { row = 4, column = 8 }, end = { row = 4, column = 14 } }
                                            , value = [ "String" ]
                                            }
                                        , moduleAlias = Nothing
                                        , exposingList = Nothing
                                        }
                                  }
                                ]
                            , declarations =
                                [ { range = { start = { row = 8, column = 1 }, end = { row = 8, column = 38 } }
                                  , value =
                                        GrenSyntax.PortDeclaration
                                            { name =
                                                { range = { start = { row = 8, column = 6 }, end = { row = 8, column = 18 } }
                                                , value = "sendResponse"
                                                }
                                            , typeAnnotation =
                                                { range = { start = { row = 8, column = 21 }, end = { row = 8, column = 38 } }
                                                , value =
                                                    GrenSyntax.TypeAnnotationFunction
                                                        { input =
                                                            { range = { start = { row = 8, column = 21 }, end = { row = 8, column = 27 } }
                                                            , value =
                                                                GrenSyntax.TypeAnnotationConstruct
                                                                    { reference =
                                                                        { range = { start = { row = 8, column = 21 }, end = { row = 8, column = 27 } }
                                                                        , value = { qualification = [], name = "String" }
                                                                        }
                                                                    , arguments = []
                                                                    }
                                                            }
                                                        , output =
                                                            { range = { start = { row = 8, column = 31 }, end = { row = 8, column = 38 } }
                                                            , value =
                                                                GrenSyntax.TypeAnnotationConstruct
                                                                    { reference =
                                                                        { range = { start = { row = 8, column = 31 }, end = { row = 8, column = 34 } }
                                                                        , value = { qualification = [], name = "Cmd" }
                                                                        }
                                                                    , arguments =
                                                                        [ { range = { start = { row = 8, column = 35 }, end = { row = 8, column = 38 } }
                                                                          , value = GrenSyntax.TypeAnnotationVariable "msg"
                                                                          }
                                                                        ]
                                                                    }
                                                            }
                                                        }
                                                }
                                            }
                                  }
                                ]
                            , comments =
                                [ { range = { start = { row = 6, column = 1 }, end = { row = 7, column = 3 } }
                                  , value = "{-| foo\n-}"
                                  }
                                ]
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
                                [ { range = { end = { column = 13, row = 5 }, start = { column = 1, row = 4 } }
                                  , value =
                                        GrenSyntax.ValueOrFunctionDeclaration
                                            { declaration =
                                                { range = { end = { column = 13, row = 5 }, start = { column = 1, row = 4 } }
                                                , value =
                                                    { parameters = []
                                                    , expression =
                                                        { range = { end = { column = 13, row = 5 }, start = { column = 5, row = 5 } }
                                                        , value = GrenSyntax.ExpressionReference { qualification = [ "Cmd" ], name = "none" }
                                                        }
                                                    , name =
                                                        { range = { end = { column = 13, row = 4 }, start = { column = 1, row = 4 } }
                                                        , value = "sendResponse"
                                                        }
                                                    }
                                                }
                                            , documentation = Nothing
                                            , signature = Nothing
                                            }
                                  }
                                ]
                            , imports = []
                            , moduleDefinition =
                                { range = { end = { column = 30, row = 2 }, start = { column = 1, row = 2 } }
                                , value =
                                    GrenSyntax.NormalModule
                                        { exposingList =
                                            { range = { end = { column = 30, row = 2 }, start = { column = 17, row = 2 } }
                                            , value = GrenSyntax.All { end = { column = 29, row = 2 }, start = { column = 27, row = 2 } }
                                            }
                                        , moduleName =
                                            { range = { end = { column = 16, row = 2 }, start = { column = 13, row = 2 } }, value = [ "Foo" ] }
                                        }
                                }
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
                                [ { range = { end = { column = 38, row = 4 }, start = { column = 1, row = 4 } }
                                  , value =
                                        GrenSyntax.PortDeclaration
                                            { name =
                                                { range = { end = { column = 18, row = 4 }, start = { column = 6, row = 4 } }
                                                , value = "sendResponse"
                                                }
                                            , typeAnnotation =
                                                { range = { end = { column = 38, row = 4 }, start = { column = 21, row = 4 } }
                                                , value =
                                                    GrenSyntax.TypeAnnotationFunction
                                                        { input =
                                                            { range = { end = { column = 27, row = 4 }, start = { column = 21, row = 4 } }
                                                            , value =
                                                                GrenSyntax.TypeAnnotationConstruct
                                                                    { reference =
                                                                        { range = { end = { column = 27, row = 4 }, start = { column = 21, row = 4 } }
                                                                        , value = { qualification = [], name = "String" }
                                                                        }
                                                                    , arguments = []
                                                                    }
                                                            }
                                                        , output =
                                                            { range = { end = { column = 38, row = 4 }, start = { column = 31, row = 4 } }
                                                            , value =
                                                                GrenSyntax.TypeAnnotationConstruct
                                                                    { reference =
                                                                        { range = { end = { column = 34, row = 4 }, start = { column = 31, row = 4 } }
                                                                        , value = { qualification = [], name = "Cmd" }
                                                                        }
                                                                    , arguments =
                                                                        [ { range = { end = { column = 38, row = 4 }, start = { column = 35, row = 4 } }
                                                                          , value = GrenSyntax.TypeAnnotationVariable "msg"
                                                                          }
                                                                        ]
                                                                    }
                                                            }
                                                        }
                                                }
                                            }
                                  }
                                ]
                            , imports = []
                            , moduleDefinition =
                                { range = { end = { column = 25, row = 2 }, start = { column = 1, row = 2 } }
                                , value =
                                    GrenSyntax.PortModule
                                        { exposingList =
                                            { range = { end = { column = 25, row = 2 }, start = { column = 12, row = 2 } }
                                            , value = GrenSyntax.All { end = { column = 24, row = 2 }, start = { column = 22, row = 2 } }
                                            }
                                        , moduleName =
                                            { range = { end = { column = 11, row = 2 }, start = { column = 8, row = 2 } }, value = [ "Foo" ] }
                                        }
                                }
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
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                            , value =
                                GrenSyntax.ValueOrFunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                                        , value =
                                            { name =
                                                { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }, value = "foo" }
                                            , parameters = []
                                            , expression =
                                                { range = { start = { row = 1, column = 7 }, end = { row = 1, column = 10 } }
                                                , value = GrenSyntax.ExpressionReference { qualification = [], name = "bar" }
                                                }
                                            }
                                        }
                                    }
                            }
                )
            , Test.test "function declaration with documentation"
                (\() ->
                    """{-| Foo does bar -}
foo = bar"""
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            { range = { start = { row = 1, column = 1 }, end = { row = 2, column = 10 } }
                            , value =
                                GrenSyntax.ValueOrFunctionDeclaration
                                    { documentation =
                                        Just
                                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 20 } }
                                            , value = "{-| Foo does bar -}"
                                            }
                                    , signature = Nothing
                                    , declaration =
                                        { range = { start = { row = 2, column = 1 }, end = { row = 2, column = 10 } }
                                        , value =
                                            { name =
                                                { range = { start = { row = 2, column = 1 }, end = { row = 2, column = 4 } }, value = "foo" }
                                            , parameters = []
                                            , expression =
                                                { range = { start = { row = 2, column = 7 }, end = { row = 2, column = 10 } }
                                                , value = GrenSyntax.ExpressionReference { qualification = [], name = "bar" }
                                                }
                                            }
                                        }
                                    }
                            }
                )
            , Test.test "function declaration with empty record"
                (\() ->
                    "foo = {}"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 9 } }
                            , value =
                                GrenSyntax.ValueOrFunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 9 } }
                                        , value =
                                            { name =
                                                { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }, value = "foo" }
                                            , parameters = []
                                            , expression =
                                                { range = { start = { row = 1, column = 7 }, end = { row = 1, column = 9 } }
                                                , value = GrenSyntax.ExpressionRecord []
                                                }
                                            }
                                        }
                                    }
                            }
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
                            { range = { start = { row = 1, column = 1 }, end = { row = 7, column = 7 } }
                            , value =
                                GrenSyntax.ValueOrFunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 7, column = 7 } }
                                        , value =
                                            { name =
                                                { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }, value = "inc" }
                                            , parameters =
                                                [ { range = { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } }
                                                  , value = GrenSyntax.PatternVariable "x"
                                                  }
                                                ]
                                            , expression =
                                                { range = { start = { row = 2, column = 3 }, end = { row = 7, column = 7 } }
                                                , value =
                                                    GrenSyntax.ExpressionLetIn
                                                        { declarations =
                                                            [ { range = { start = { row = 3, column = 5 }, end = { row = 5, column = 18 } }
                                                              , value =
                                                                    GrenSyntax.LetFunction
                                                                        { documentation = Nothing
                                                                        , signature = Nothing
                                                                        , declaration =
                                                                            { range = { start = { row = 3, column = 5 }, end = { row = 5, column = 18 } }
                                                                            , value =
                                                                                { name = { range = { start = { row = 3, column = 5 }, end = { row = 3, column = 6 } }, value = "y" }
                                                                                , parameters = []
                                                                                , expression =
                                                                                    { range = { start = { row = 4, column = 7 }, end = { row = 5, column = 18 } }
                                                                                    , value =
                                                                                        GrenSyntax.ExpressionCaseOf
                                                                                            { expression =
                                                                                                { range = { start = { row = 4, column = 12 }, end = { row = 4, column = 13 } }
                                                                                                , value = GrenSyntax.ExpressionReference { qualification = [], name = "x" }
                                                                                                }
                                                                                            , cases =
                                                                                                [ { pattern =
                                                                                                        { range = { start = { row = 5, column = 9 }, end = { row = 5, column = 13 } }
                                                                                                        , value = GrenSyntax.PatternVariant { qualification = [], name = "True", value = Nothing }
                                                                                                        }
                                                                                                  , result =
                                                                                                        { range = { start = { row = 5, column = 17 }, end = { row = 5, column = 18 } }
                                                                                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "z" }
                                                                                                        }
                                                                                                  }
                                                                                                ]
                                                                                            }
                                                                                    }
                                                                                }
                                                                            }
                                                                        }
                                                              }
                                                            , { range = { start = { row = 6, column = 5 }, end = { row = 6, column = 10 } }
                                                              , value =
                                                                    GrenSyntax.LetFunction
                                                                        { documentation = Nothing
                                                                        , signature = Nothing
                                                                        , declaration =
                                                                            { range = { start = { row = 6, column = 5 }, end = { row = 6, column = 10 } }
                                                                            , value =
                                                                                { name = { range = { start = { row = 6, column = 5 }, end = { row = 6, column = 6 } }, value = "a" }
                                                                                , parameters = []
                                                                                , expression =
                                                                                    { range = { start = { row = 6, column = 9 }, end = { row = 6, column = 10 } }
                                                                                    , value = GrenSyntax.ExpressionReference { qualification = [], name = "b" }
                                                                                    }
                                                                                }
                                                                            }
                                                                        }
                                                              }
                                                            ]
                                                        , result =
                                                            { range = { start = { row = 7, column = 6 }, end = { row = 7, column = 7 } }
                                                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "a" }
                                                            }
                                                        }
                                                }
                                            }
                                        }
                                    }
                            }
                )
            , Test.test "function declaration with args"
                (\() ->
                    "inc x = x + 1"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 14 } }
                            , value =
                                GrenSyntax.ValueOrFunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 14 } }
                                        , value =
                                            { name =
                                                { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }, value = "inc" }
                                            , parameters =
                                                [ { range = { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } }
                                                  , value = GrenSyntax.PatternVariable "x"
                                                  }
                                                ]
                                            , expression =
                                                { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 14 } }
                                                , value =
                                                    GrenSyntax.ExpressionInfixOperation
                                                        { operator = "+"
                                                        , left =
                                                            { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } }
                                                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "x" }
                                                            }
                                                        , right =
                                                            { range = { start = { row = 1, column = 13 }, end = { row = 1, column = 14 } }
                                                            , value = GrenSyntax.ExpressionInteger 1
                                                            }
                                                        }
                                                }
                                            }
                                        }
                                    }
                            }
                )
            , Test.test "function declaration with let"
                (\() ->
                    """foo =
 let
  b = 1
 in
  b"""
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            { range = { start = { row = 1, column = 1 }, end = { row = 5, column = 4 } }
                            , value =
                                GrenSyntax.ValueOrFunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 5, column = 4 } }
                                        , value =
                                            { name =
                                                { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }, value = "foo" }
                                            , parameters = []
                                            , expression =
                                                { range = { start = { row = 2, column = 2 }, end = { row = 5, column = 4 } }
                                                , value =
                                                    GrenSyntax.ExpressionLetIn
                                                        { declarations =
                                                            [ { range = { start = { row = 3, column = 3 }, end = { row = 3, column = 8 } }
                                                              , value =
                                                                    GrenSyntax.LetFunction
                                                                        { documentation = Nothing
                                                                        , signature = Nothing
                                                                        , declaration =
                                                                            { range = { start = { row = 3, column = 3 }, end = { row = 3, column = 8 } }
                                                                            , value =
                                                                                { name = { range = { start = { row = 3, column = 3 }, end = { row = 3, column = 4 } }, value = "b" }
                                                                                , parameters = []
                                                                                , expression =
                                                                                    { range = { start = { row = 3, column = 7 }, end = { row = 3, column = 8 } }
                                                                                    , value = GrenSyntax.ExpressionInteger 1
                                                                                    }
                                                                                }
                                                                            }
                                                                        }
                                                              }
                                                            ]
                                                        , result =
                                                            { range = { start = { row = 5, column = 3 }, end = { row = 5, column = 4 } }
                                                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "b" }
                                                            }
                                                        }
                                                }
                                            }
                                        }
                                    }
                            }
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
  (b   )=(1   )
 in
  b"""
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            { range = { start = { row = 1, column = 1 }, end = { row = 5, column = 4 } }
                            , value =
                                GrenSyntax.ValueOrFunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 5, column = 4 } }
                                        , value =
                                            { name =
                                                { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }, value = "foo" }
                                            , parameters = []
                                            , expression =
                                                { range = { start = { row = 2, column = 2 }, end = { row = 5, column = 4 } }
                                                , value =
                                                    GrenSyntax.ExpressionLetIn
                                                        { declarations =
                                                            [ { range = { start = { row = 3, column = 3 }, end = { row = 3, column = 16 } }
                                                              , value =
                                                                    GrenSyntax.LetDestructuring
                                                                        { pattern =
                                                                            { range = { start = { row = 3, column = 3 }, end = { row = 3, column = 9 } }
                                                                            , value =
                                                                                GrenSyntax.PatternParenthesized
                                                                                    { range = { start = { row = 3, column = 4 }, end = { row = 3, column = 5 } }
                                                                                    , value = GrenSyntax.PatternVariable "b"
                                                                                    }
                                                                            }
                                                                        , expression =
                                                                            { range = { start = { row = 3, column = 10 }, end = { row = 3, column = 16 } }
                                                                            , value =
                                                                                GrenSyntax.ExpressionParenthesized
                                                                                    { range = { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } }
                                                                                    , value = GrenSyntax.ExpressionInteger 1
                                                                                    }
                                                                            }
                                                                        }
                                                              }
                                                            ]
                                                        , result =
                                                            { range = { start = { row = 5, column = 3 }, end = { row = 5, column = 4 } }
                                                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "b" }
                                                            }
                                                        }
                                                }
                                            }
                                        }
                                    }
                            }
                )
            , Test.test "declaration with record"
                (\() ->
                    """main =
  beginnerProgram { model = 0, view = view, update = update }"""
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            { range = { start = { row = 1, column = 1 }, end = { row = 2, column = 62 } }
                            , value =
                                GrenSyntax.ValueOrFunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 2, column = 62 } }
                                        , value =
                                            { name =
                                                { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }, value = "main" }
                                            , parameters = []
                                            , expression =
                                                { range = { start = { row = 2, column = 3 }, end = { row = 2, column = 62 } }
                                                , value =
                                                    GrenSyntax.ExpressionCall
                                                        [ { range = { start = { row = 2, column = 3 }, end = { row = 2, column = 18 } }
                                                          , value = GrenSyntax.ExpressionReference { qualification = [], name = "beginnerProgram" }
                                                          }
                                                        , { range = { start = { row = 2, column = 19 }, end = { row = 2, column = 62 } }
                                                          , value =
                                                                GrenSyntax.ExpressionRecord
                                                                    [ { range = { start = { row = 2, column = 21 }, end = { row = 2, column = 30 } }
                                                                      , value =
                                                                            { name =
                                                                                { range = { start = { row = 2, column = 21 }, end = { row = 2, column = 26 } }, value = "model" }
                                                                            , value =
                                                                                { range = { start = { row = 2, column = 29 }, end = { row = 2, column = 30 } }
                                                                                , value = GrenSyntax.ExpressionInteger 0
                                                                                }
                                                                            }
                                                                      }
                                                                    , { range = { start = { row = 2, column = 32 }, end = { row = 2, column = 43 } }
                                                                      , value =
                                                                            { name =
                                                                                { range = { start = { row = 2, column = 32 }, end = { row = 2, column = 36 } }, value = "view" }
                                                                            , value =
                                                                                { range = { start = { row = 2, column = 39 }, end = { row = 2, column = 43 } }
                                                                                , value = GrenSyntax.ExpressionReference { qualification = [], name = "view" }
                                                                                }
                                                                            }
                                                                      }
                                                                    , { range = { start = { row = 2, column = 45 }, end = { row = 2, column = 61 } }
                                                                      , value =
                                                                            { name =
                                                                                { range = { start = { row = 2, column = 45 }, end = { row = 2, column = 51 } }, value = "update" }
                                                                            , value =
                                                                                { range = { start = { row = 2, column = 54 }, end = { row = 2, column = 60 } }
                                                                                , value = GrenSyntax.ExpressionReference { qualification = [], name = "update" }
                                                                                }
                                                                            }
                                                                      }
                                                                    ]
                                                          }
                                                        ]
                                                }
                                            }
                                        }
                                    }
                            }
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
                            { range = { start = { row = 1, column = 1 }, end = { row = 7, column = 16 } }
                            , value =
                                GrenSyntax.ValueOrFunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 7, column = 16 } }
                                        , value =
                                            { name =
                                                { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }, value = "update" }
                                            , parameters =
                                                [ { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }
                                                  , value = GrenSyntax.PatternVariable "msg"
                                                  }
                                                , { range = { start = { row = 1, column = 12 }, end = { row = 1, column = 17 } }
                                                  , value = GrenSyntax.PatternVariable "model"
                                                  }
                                                ]
                                            , expression =
                                                { range = { start = { row = 2, column = 3 }, end = { row = 7, column = 16 } }
                                                , value =
                                                    GrenSyntax.ExpressionCaseOf
                                                        { expression =
                                                            { range = { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } }
                                                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "msg" }
                                                            }
                                                        , cases =
                                                            [ { pattern =
                                                                    { range = { start = { row = 3, column = 5 }, end = { row = 3, column = 14 } }
                                                                    , value = GrenSyntax.PatternVariant { qualification = [], name = "Increment", value = Nothing }
                                                                    }
                                                              , result =
                                                                    { range = { start = { row = 4, column = 7 }, end = { row = 4, column = 16 } }
                                                                    , value =
                                                                        GrenSyntax.ExpressionInfixOperation
                                                                            { operator = "+"
                                                                            , left =
                                                                                { range = { start = { row = 4, column = 7 }, end = { row = 4, column = 12 } }
                                                                                , value = GrenSyntax.ExpressionReference { qualification = [], name = "model" }
                                                                                }
                                                                            , right =
                                                                                { range = { start = { row = 4, column = 15 }, end = { row = 4, column = 16 } }
                                                                                , value = GrenSyntax.ExpressionInteger 1
                                                                                }
                                                                            }
                                                                    }
                                                              }
                                                            , { pattern =
                                                                    { range = { start = { row = 6, column = 5 }, end = { row = 6, column = 14 } }
                                                                    , value = GrenSyntax.PatternVariant { qualification = [], name = "Decrement", value = Nothing }
                                                                    }
                                                              , result =
                                                                    { range = { start = { row = 7, column = 7 }, end = { row = 7, column = 16 } }
                                                                    , value =
                                                                        GrenSyntax.ExpressionInfixOperation
                                                                            { operator = "-"
                                                                            , left =
                                                                                { range = { start = { row = 7, column = 7 }, end = { row = 7, column = 12 } }
                                                                                , value = GrenSyntax.ExpressionReference { qualification = [], name = "model" }
                                                                                }
                                                                            , right =
                                                                                { range = { start = { row = 7, column = 15 }, end = { row = 7, column = 16 } }
                                                                                , value = GrenSyntax.ExpressionInteger 1
                                                                                }
                                                                            }
                                                                    }
                                                              }
                                                            ]
                                                        }
                                                }
                                            }
                                        }
                                    }
                            }
                )
            , Test.test "port declaration for command"
                (\() ->
                    "port parseResponse : ( String         ) -> Cmd msg"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            { range = { end = { column = 51, row = 1 }, start = { column = 1, row = 1 } }
                            , value =
                                GrenSyntax.PortDeclaration
                                    { name =
                                        { range = { end = { column = 19, row = 1 }, start = { column = 6, row = 1 } }
                                        , value = "parseResponse"
                                        }
                                    , typeAnnotation =
                                        { range = { end = { column = 51, row = 1 }, start = { column = 22, row = 1 } }
                                        , value =
                                            GrenSyntax.TypeAnnotationFunction
                                                { input =
                                                    { range = { end = { column = 40, row = 1 }, start = { column = 22, row = 1 } }
                                                    , value =
                                                        GrenSyntax.TypeAnnotationParenthesized
                                                            { range = { end = { column = 30, row = 1 }, start = { column = 24, row = 1 } }
                                                            , value =
                                                                GrenSyntax.TypeAnnotationConstruct
                                                                    { reference =
                                                                        { range = { end = { column = 30, row = 1 }, start = { column = 24, row = 1 } }
                                                                        , value = { qualification = [], name = "String" }
                                                                        }
                                                                    , arguments = []
                                                                    }
                                                            }
                                                    }
                                                , output =
                                                    { range = { end = { column = 51, row = 1 }, start = { column = 44, row = 1 } }
                                                    , value =
                                                        GrenSyntax.TypeAnnotationConstruct
                                                            { reference =
                                                                { range = { end = { column = 47, row = 1 }, start = { column = 44, row = 1 } }
                                                                , value = { qualification = [], name = "Cmd" }
                                                                }
                                                            , arguments =
                                                                [ { range = { end = { column = 51, row = 1 }, start = { column = 48, row = 1 } }
                                                                  , value = GrenSyntax.TypeAnnotationVariable "msg"
                                                                  }
                                                                ]
                                                            }
                                                    }
                                                }
                                        }
                                    }
                            }
                )
            , Test.test "port declaration for subscription"
                (\() ->
                    "port scroll : (Move -> msg) -> Sub msg"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            { range = { end = { column = 39, row = 1 }, start = { column = 1, row = 1 } }
                            , value =
                                GrenSyntax.PortDeclaration
                                    { name =
                                        { range = { end = { column = 12, row = 1 }, start = { column = 6, row = 1 } }, value = "scroll" }
                                    , typeAnnotation =
                                        { range = { end = { column = 39, row = 1 }, start = { column = 15, row = 1 } }
                                        , value =
                                            GrenSyntax.TypeAnnotationFunction
                                                { input =
                                                    { range = { end = { column = 28, row = 1 }, start = { column = 15, row = 1 } }
                                                    , value =
                                                        GrenSyntax.TypeAnnotationParenthesized
                                                            { range = { end = { column = 27, row = 1 }, start = { column = 16, row = 1 } }
                                                            , value =
                                                                GrenSyntax.TypeAnnotationFunction
                                                                    { input =
                                                                        { range = { end = { column = 20, row = 1 }, start = { column = 16, row = 1 } }
                                                                        , value =
                                                                            GrenSyntax.TypeAnnotationConstruct
                                                                                { reference =
                                                                                    { range = { end = { column = 20, row = 1 }, start = { column = 16, row = 1 } }
                                                                                    , value = { qualification = [], name = "Move" }
                                                                                    }
                                                                                , arguments = []
                                                                                }
                                                                        }
                                                                    , output =
                                                                        { range = { end = { column = 27, row = 1 }, start = { column = 24, row = 1 } }
                                                                        , value = GrenSyntax.TypeAnnotationVariable "msg"
                                                                        }
                                                                    }
                                                            }
                                                    }
                                                , output =
                                                    { range = { end = { column = 39, row = 1 }, start = { column = 32, row = 1 } }
                                                    , value =
                                                        GrenSyntax.TypeAnnotationConstruct
                                                            { reference =
                                                                { range = { end = { column = 35, row = 1 }, start = { column = 32, row = 1 } }
                                                                , value = { qualification = [], name = "Sub" }
                                                                }
                                                            , arguments =
                                                                [ { range = { end = { column = 39, row = 1 }, start = { column = 36, row = 1 } }
                                                                  , value = GrenSyntax.TypeAnnotationVariable "msg"
                                                                  }
                                                                ]
                                                            }
                                                    }
                                                }
                                        }
                                    }
                            }
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
                            { range = { start = { row = 1, column = 1 }, end = { row = 2, column = 23 } }
                            , value =
                                GrenSyntax.ValueOrFunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 2, column = 23 } }
                                        , value =
                                            { name =
                                                { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }, value = "main" }
                                            , parameters = []
                                            , expression =
                                                { range = { start = { row = 2, column = 3 }, end = { row = 2, column = 23 } }
                                                , value =
                                                    GrenSyntax.ExpressionCall
                                                        [ { range = { start = { row = 2, column = 3 }, end = { row = 2, column = 7 } }
                                                          , value = GrenSyntax.ExpressionReference { qualification = [], name = "text" }
                                                          }
                                                        , { range = { start = { row = 2, column = 8 }, end = { row = 2, column = 23 } }
                                                          , value =
                                                                GrenSyntax.ExpressionString
                                                                    { quotingStyle = GrenSyntax.StringSingleQuoted, content = "Hello, World!" }
                                                          }
                                                        ]
                                                }
                                            }
                                        }
                                    }
                            }
                )
            , Test.test "value/function declaration with signature"
                (\() ->
                    """main : Html msg
main =
  text "Hello, World!\""""
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            { range = { end = { column = 23, row = 3 }, start = { column = 1, row = 1 } }
                            , value =
                                GrenSyntax.ValueOrFunctionDeclaration
                                    { declaration =
                                        { range = { end = { column = 23, row = 3 }, start = { column = 1, row = 2 } }
                                        , value =
                                            { parameters = []
                                            , expression =
                                                { range = { end = { column = 23, row = 3 }, start = { column = 3, row = 3 } }
                                                , value =
                                                    GrenSyntax.ExpressionCall
                                                        [ { range = { end = { column = 7, row = 3 }, start = { column = 3, row = 3 } }
                                                          , value = GrenSyntax.ExpressionReference { qualification = [], name = "text" }
                                                          }
                                                        , { range = { end = { column = 23, row = 3 }, start = { column = 8, row = 3 } }
                                                          , value =
                                                                GrenSyntax.ExpressionString
                                                                    { quotingStyle = GrenSyntax.StringSingleQuoted, content = "Hello, World!" }
                                                          }
                                                        ]
                                                }
                                            , name =
                                                { range = { end = { column = 5, row = 2 }, start = { column = 1, row = 2 } }, value = "main" }
                                            }
                                        }
                                    , documentation = Nothing
                                    , signature =
                                        Just
                                            { range = { end = { column = 16, row = 1 }, start = { column = 1, row = 1 } }
                                            , value =
                                                { name =
                                                    { range = { end = { column = 5, row = 1 }, start = { column = 1, row = 1 } }, value = "main" }
                                                , typeAnnotation =
                                                    { range = { end = { column = 16, row = 1 }, start = { column = 8, row = 1 } }
                                                    , value =
                                                        GrenSyntax.TypeAnnotationConstruct
                                                            { reference =
                                                                { range = { end = { column = 12, row = 1 }, start = { column = 8, row = 1 } }
                                                                , value = { qualification = [], name = "Html" }
                                                                }
                                                            , arguments =
                                                                [ { range = { end = { column = 16, row = 1 }, start = { column = 13, row = 1 } }
                                                                  , value = GrenSyntax.TypeAnnotationVariable "msg"
                                                                  }
                                                                ]
                                                            }
                                                    }
                                                }
                                            }
                                    }
                            }
                )
            , Test.test "value/function declaration with signature omitting start name"
                (\() ->
                    """: Html msg
main =
  text "Hello, World!\""""
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            { range = { end = { column = 23, row = 3 }, start = { column = 1, row = 1 } }
                            , value =
                                GrenSyntax.ValueOrFunctionDeclaration
                                    { declaration =
                                        { range = { end = { column = 23, row = 3 }, start = { column = 1, row = 2 } }
                                        , value =
                                            { parameters = []
                                            , expression =
                                                { range = { end = { column = 23, row = 3 }, start = { column = 3, row = 3 } }
                                                , value =
                                                    GrenSyntax.ExpressionCall
                                                        [ { range = { end = { column = 7, row = 3 }, start = { column = 3, row = 3 } }
                                                          , value = GrenSyntax.ExpressionReference { qualification = [], name = "text" }
                                                          }
                                                        , { range = { end = { column = 23, row = 3 }, start = { column = 8, row = 3 } }
                                                          , value =
                                                                GrenSyntax.ExpressionString
                                                                    { quotingStyle = GrenSyntax.StringSingleQuoted, content = "Hello, World!" }
                                                          }
                                                        ]
                                                }
                                            , name =
                                                { range = { end = { column = 5, row = 2 }, start = { column = 1, row = 2 } }, value = "main" }
                                            }
                                        }
                                    , documentation = Nothing
                                    , signature =
                                        Just
                                            { range = { end = { column = 11, row = 1 }, start = { column = 1, row = 1 } }
                                            , value =
                                                { name =
                                                    { range = { end = { column = 1, row = 1 }, start = { column = 1, row = 1 } }, value = "main" }
                                                , typeAnnotation =
                                                    { range = { end = { column = 11, row = 1 }, start = { column = 3, row = 1 } }
                                                    , value =
                                                        GrenSyntax.TypeAnnotationConstruct
                                                            { reference =
                                                                { range = { end = { column = 7, row = 1 }, start = { column = 3, row = 1 } }
                                                                , value = { qualification = [], name = "Html" }
                                                                }
                                                            , arguments =
                                                                [ { range = { end = { column = 11, row = 1 }, start = { column = 8, row = 1 } }
                                                                  , value = GrenSyntax.TypeAnnotationVariable "msg"
                                                                  }
                                                                ]
                                                            }
                                                    }
                                                }
                                            }
                                    }
                            }
                )
            , Test.test "function with -> instead of ="
                (\() ->
                    """main ->
  text "Hello, World!\""""
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            { range = { start = { row = 1, column = 1 }, end = { row = 2, column = 23 } }
                            , value =
                                GrenSyntax.ValueOrFunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 2, column = 23 } }
                                        , value =
                                            { name =
                                                { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }, value = "main" }
                                            , parameters = []
                                            , expression =
                                                { range = { start = { row = 2, column = 3 }, end = { row = 2, column = 23 } }
                                                , value =
                                                    GrenSyntax.ExpressionCall
                                                        [ { range = { start = { row = 2, column = 3 }, end = { row = 2, column = 7 } }
                                                          , value = GrenSyntax.ExpressionReference { qualification = [], name = "text" }
                                                          }
                                                        , { range = { start = { row = 2, column = 8 }, end = { row = 2, column = 23 } }
                                                          , value =
                                                                GrenSyntax.ExpressionString
                                                                    { quotingStyle = GrenSyntax.StringSingleQuoted, content = "Hello, World!" }
                                                          }
                                                        ]
                                                }
                                            }
                                        }
                                    }
                            }
                )
            , Test.test "function starting with multi line comment"
                (\() ->
                    """main =
  {- y -} x"""
                        |> expectSyntaxWithComments GrenParserLenient.declaration
                            { syntax =
                                { range = { start = { row = 1, column = 1 }, end = { row = 2, column = 12 } }
                                , value =
                                    GrenSyntax.ValueOrFunctionDeclaration
                                        { documentation = Nothing
                                        , signature = Nothing
                                        , declaration =
                                            { range = { start = { row = 1, column = 1 }, end = { row = 2, column = 12 } }
                                            , value =
                                                { name =
                                                    { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }, value = "main" }
                                                , parameters = []
                                                , expression =
                                                    { range = { start = { row = 2, column = 11 }, end = { row = 2, column = 12 } }
                                                    , value = GrenSyntax.ExpressionReference { qualification = [], name = "x" }
                                                    }
                                                }
                                            }
                                        }
                                }
                            , comments = [ { range = { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } }, value = "{- y -}" } ]
                            }
                )
            , Test.test "function with a lot of symbols"
                (\() ->
                    "updateState update sendPort = curry <| (uncurry update) >> batchStateCmds sendPort"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 83 } }
                            , value =
                                GrenSyntax.ValueOrFunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 83 } }
                                        , value =
                                            { name =
                                                { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                                , value = "updateState"
                                                }
                                            , parameters =
                                                [ { range = { start = { row = 1, column = 13 }, end = { row = 1, column = 19 } }
                                                  , value = GrenSyntax.PatternVariable "update"
                                                  }
                                                , { range = { start = { row = 1, column = 20 }, end = { row = 1, column = 28 } }
                                                  , value = GrenSyntax.PatternVariable "sendPort"
                                                  }
                                                ]
                                            , expression =
                                                { range = { start = { row = 1, column = 31 }, end = { row = 1, column = 83 } }
                                                , value =
                                                    GrenSyntax.ExpressionInfixOperation
                                                        { operator = "<|"
                                                        , left =
                                                            { range = { start = { row = 1, column = 31 }, end = { row = 1, column = 36 } }
                                                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "curry" }
                                                            }
                                                        , right =
                                                            { range = { start = { row = 1, column = 40 }, end = { row = 1, column = 83 } }
                                                            , value =
                                                                GrenSyntax.ExpressionInfixOperation
                                                                    { operator = ">>"
                                                                    , left =
                                                                        { range = { start = { row = 1, column = 40 }, end = { row = 1, column = 56 } }
                                                                        , value =
                                                                            GrenSyntax.ExpressionParenthesized
                                                                                { range = { start = { row = 1, column = 41 }, end = { row = 1, column = 55 } }
                                                                                , value =
                                                                                    GrenSyntax.ExpressionCall
                                                                                        [ { range = { start = { row = 1, column = 41 }, end = { row = 1, column = 48 } }
                                                                                          , value = GrenSyntax.ExpressionReference { qualification = [], name = "uncurry" }
                                                                                          }
                                                                                        , { range = { start = { row = 1, column = 49 }, end = { row = 1, column = 55 } }
                                                                                          , value = GrenSyntax.ExpressionReference { qualification = [], name = "update" }
                                                                                          }
                                                                                        ]
                                                                                }
                                                                        }
                                                                    , right =
                                                                        { range = { start = { row = 1, column = 60 }, end = { row = 1, column = 83 } }
                                                                        , value =
                                                                            GrenSyntax.ExpressionCall
                                                                                [ { range = { start = { row = 1, column = 60 }, end = { row = 1, column = 74 } }
                                                                                  , value = GrenSyntax.ExpressionReference { qualification = [], name = "batchStateCmds" }
                                                                                  }
                                                                                , { range = { start = { row = 1, column = 75 }, end = { row = 1, column = 83 } }
                                                                                  , value = GrenSyntax.ExpressionReference { qualification = [], name = "sendPort" }
                                                                                  }
                                                                                ]
                                                                        }
                                                                    }
                                                            }
                                                        }
                                                }
                                            }
                                        }
                                    }
                            }
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
                            { range = { start = { row = 1, column = 1 }, end = { row = 7, column = 16 } }
                            , value =
                                GrenSyntax.ValueOrFunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 7, column = 16 } }
                                        , value =
                                            { name =
                                                { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }, value = "update" }
                                            , parameters =
                                                [ { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }
                                                  , value = GrenSyntax.PatternVariable "msg"
                                                  }
                                                , { range = { start = { row = 1, column = 12 }, end = { row = 1, column = 17 } }
                                                  , value = GrenSyntax.PatternVariable "model"
                                                  }
                                                ]
                                            , expression =
                                                { range = { start = { row = 2, column = 3 }, end = { row = 7, column = 16 } }
                                                , value =
                                                    GrenSyntax.ExpressionCaseOf
                                                        { expression =
                                                            { range = { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } }
                                                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "msg" }
                                                            }
                                                        , cases =
                                                            [ { pattern =
                                                                    { range = { start = { row = 3, column = 5 }, end = { row = 3, column = 14 } }
                                                                    , value = GrenSyntax.PatternVariant { qualification = [], name = "Increment", value = Nothing }
                                                                    }
                                                              , result =
                                                                    { range = { start = { row = 4, column = 7 }, end = { row = 4, column = 16 } }
                                                                    , value =
                                                                        GrenSyntax.ExpressionInfixOperation
                                                                            { operator = "+"
                                                                            , left =
                                                                                { range = { start = { row = 4, column = 7 }, end = { row = 4, column = 12 } }
                                                                                , value = GrenSyntax.ExpressionReference { qualification = [], name = "model" }
                                                                                }
                                                                            , right =
                                                                                { range = { start = { row = 4, column = 15 }, end = { row = 4, column = 16 } }
                                                                                , value = GrenSyntax.ExpressionInteger 1
                                                                                }
                                                                            }
                                                                    }
                                                              }
                                                            , { pattern =
                                                                    { range = { start = { row = 6, column = 5 }, end = { row = 6, column = 14 } }
                                                                    , value = GrenSyntax.PatternVariant { qualification = [], name = "Decrement", value = Nothing }
                                                                    }
                                                              , result =
                                                                    { range = { start = { row = 7, column = 7 }, end = { row = 7, column = 16 } }
                                                                    , value =
                                                                        GrenSyntax.ExpressionInfixOperation
                                                                            { operator = "-"
                                                                            , left =
                                                                                { range = { start = { row = 7, column = 7 }, end = { row = 7, column = 12 } }
                                                                                , value = GrenSyntax.ExpressionReference { qualification = [], name = "model" }
                                                                                }
                                                                            , right =
                                                                                { range = { start = { row = 7, column = 15 }, end = { row = 7, column = 16 } }
                                                                                , value = GrenSyntax.ExpressionInteger 1
                                                                                }
                                                                            }
                                                                    }
                                                              }
                                                            ]
                                                        }
                                                }
                                            }
                                        }
                                    }
                            }
                )
            , Test.test "some other function"
                (\() ->
                    """update : Model
update msg model =
    msg"""
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            { range = { start = { row = 1, column = 1 }, end = { row = 3, column = 8 } }
                            , value =
                                GrenSyntax.ValueOrFunctionDeclaration
                                    { documentation = Nothing
                                    , signature =
                                        Just
                                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                                            , value =
                                                { name =
                                                    { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }, value = "update" }
                                                , typeAnnotation =
                                                    { range = { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } }
                                                    , value =
                                                        GrenSyntax.TypeAnnotationConstruct
                                                            { reference =
                                                                { range = { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } }
                                                                , value = { qualification = [], name = "Model" }
                                                                }
                                                            , arguments = []
                                                            }
                                                    }
                                                }
                                            }
                                    , declaration =
                                        { range = { start = { row = 2, column = 1 }, end = { row = 3, column = 8 } }
                                        , value =
                                            { name =
                                                { range = { start = { row = 2, column = 1 }, end = { row = 2, column = 7 } }, value = "update" }
                                            , parameters =
                                                [ { range = { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } }
                                                  , value = GrenSyntax.PatternVariable "msg"
                                                  }
                                                , { range = { start = { row = 2, column = 12 }, end = { row = 2, column = 17 } }
                                                  , value = GrenSyntax.PatternVariable "model"
                                                  }
                                                ]
                                            , expression =
                                                { range = { start = { row = 3, column = 5 }, end = { row = 3, column = 8 } }
                                                , value = GrenSyntax.ExpressionReference { qualification = [], name = "msg" }
                                                }
                                            }
                                        }
                                    }
                            }
                )
            , Test.test "type alias"
                (\() ->
                    "type alias Foo = {color: String }"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 34 } }
                            , value =
                                GrenSyntax.AliasDeclaration
                                    { documentation = Nothing
                                    , name =
                                        { range = { start = { row = 1, column = 12 }, end = { row = 1, column = 15 } }, value = "Foo" }
                                    , generics = []
                                    , typeAnnotation =
                                        { range = { start = { row = 1, column = 18 }, end = { row = 1, column = 34 } }
                                        , value =
                                            GrenSyntax.TypeAnnotationRecord
                                                [ { range = { start = { row = 1, column = 19 }, end = { row = 1, column = 32 } }
                                                  , value =
                                                        { name =
                                                            { range = { start = { row = 1, column = 19 }, end = { row = 1, column = 24 } }, value = "color" }
                                                        , value =
                                                            { range = { start = { row = 1, column = 26 }, end = { row = 1, column = 32 } }
                                                            , value =
                                                                GrenSyntax.TypeAnnotationConstruct
                                                                    { reference =
                                                                        { range = { start = { row = 1, column = 26 }, end = { row = 1, column = 32 } }
                                                                        , value = { qualification = [], name = "String" }
                                                                        }
                                                                    , arguments = []
                                                                    }
                                                            }
                                                        }
                                                  }
                                                ]
                                        }
                                    }
                            }
                )
            , Test.test "type alias with documentation"
                (\() ->
                    """{-| Foo is colorful -}
type alias Foo = {color: String }"""
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            { range = { start = { row = 1, column = 1 }, end = { row = 2, column = 34 } }
                            , value =
                                GrenSyntax.AliasDeclaration
                                    { documentation =
                                        Just
                                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 23 } }
                                            , value = "{-| Foo is colorful -}"
                                            }
                                    , name =
                                        { range = { start = { row = 2, column = 12 }, end = { row = 2, column = 15 } }, value = "Foo" }
                                    , generics = []
                                    , typeAnnotation =
                                        { range = { start = { row = 2, column = 18 }, end = { row = 2, column = 34 } }
                                        , value =
                                            GrenSyntax.TypeAnnotationRecord
                                                [ { range = { start = { row = 2, column = 19 }, end = { row = 2, column = 32 } }
                                                  , value =
                                                        { name =
                                                            { range = { start = { row = 2, column = 19 }, end = { row = 2, column = 24 } }, value = "color" }
                                                        , value =
                                                            { range = { start = { row = 2, column = 26 }, end = { row = 2, column = 32 } }
                                                            , value =
                                                                GrenSyntax.TypeAnnotationConstruct
                                                                    { reference =
                                                                        { range = { start = { row = 2, column = 26 }, end = { row = 2, column = 32 } }
                                                                        , value = { qualification = [], name = "String" }
                                                                        }
                                                                    , arguments = []
                                                                    }
                                                            }
                                                        }
                                                  }
                                                ]
                                        }
                                    }
                            }
                )
            , Test.test "type alias without spacings around '='"
                (\() ->
                    "type alias Foo={color: String }"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                            , value =
                                GrenSyntax.AliasDeclaration
                                    { documentation = Nothing
                                    , name =
                                        { range = { start = { row = 1, column = 12 }, end = { row = 1, column = 15 } }, value = "Foo" }
                                    , generics = []
                                    , typeAnnotation =
                                        { range = { start = { row = 1, column = 16 }, end = { row = 1, column = 32 } }
                                        , value =
                                            GrenSyntax.TypeAnnotationRecord
                                                [ { range = { start = { row = 1, column = 17 }, end = { row = 1, column = 30 } }
                                                  , value =
                                                        { name =
                                                            { range = { start = { row = 1, column = 17 }, end = { row = 1, column = 22 } }, value = "color" }
                                                        , value =
                                                            { range = { start = { row = 1, column = 24 }, end = { row = 1, column = 30 } }
                                                            , value =
                                                                GrenSyntax.TypeAnnotationConstruct
                                                                    { reference =
                                                                        { range = { start = { row = 1, column = 24 }, end = { row = 1, column = 30 } }
                                                                        , value = { qualification = [], name = "String" }
                                                                        }
                                                                    , arguments = []
                                                                    }
                                                            }
                                                        }
                                                  }
                                                ]
                                        }
                                    }
                            }
                )
            , Test.test "type alias with GenericType "
                (\() ->
                    "type alias Foo a = {some : a }"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 31 } }
                            , value =
                                GrenSyntax.AliasDeclaration
                                    { documentation = Nothing
                                    , name =
                                        { range = { start = { row = 1, column = 12 }, end = { row = 1, column = 15 } }, value = "Foo" }
                                    , generics =
                                        [ { range = { start = { row = 1, column = 16 }, end = { row = 1, column = 17 } }, value = "a" }
                                        ]
                                    , typeAnnotation =
                                        { range = { start = { row = 1, column = 20 }, end = { row = 1, column = 31 } }
                                        , value =
                                            GrenSyntax.TypeAnnotationRecord
                                                [ { range = { start = { row = 1, column = 21 }, end = { row = 1, column = 29 } }
                                                  , value =
                                                        { name =
                                                            { range = { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } }, value = "some" }
                                                        , value =
                                                            { range = { start = { row = 1, column = 28 }, end = { row = 1, column = 29 } }
                                                            , value = GrenSyntax.TypeAnnotationVariable "a"
                                                            }
                                                        }
                                                  }
                                                ]
                                        }
                                    }
                            }
                )
            , Test.test "type"
                (\() ->
                    "type Color = Blue String | Red | Green"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 39 } }
                            , value =
                                GrenSyntax.ChoiceTypeDeclaration
                                    { documentation = Nothing
                                    , name =
                                        { range = { start = { row = 1, column = 6 }, end = { row = 1, column = 11 } }, value = "Color" }
                                    , generics = []
                                    , constructors =
                                        [ { range = { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } }
                                          , value =
                                                { name =
                                                    { range = { start = { row = 1, column = 14 }, end = { row = 1, column = 18 } }, value = "Blue" }
                                                , value =
                                                    Just
                                                        { range = { start = { row = 1, column = 19 }, end = { row = 1, column = 25 } }
                                                        , value =
                                                            GrenSyntax.TypeAnnotationConstruct
                                                                { reference =
                                                                    { range = { start = { row = 1, column = 19 }, end = { row = 1, column = 25 } }
                                                                    , value = { qualification = [], name = "String" }
                                                                    }
                                                                , arguments = []
                                                                }
                                                        }
                                                }
                                          }
                                        , { range = { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } }
                                          , value =
                                                { name =
                                                    { range = { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } }, value = "Red" }
                                                , value = Nothing
                                                }
                                          }
                                        , { range = { start = { row = 1, column = 34 }, end = { row = 1, column = 39 } }
                                          , value =
                                                { name =
                                                    { range = { start = { row = 1, column = 34 }, end = { row = 1, column = 39 } }, value = "Green" }
                                                , value = Nothing
                                                }
                                          }
                                        ]
                                    }
                            }
                )
            , Test.test "type with leading | before first variant"
                (\() ->
                    "type Color=| Blue String | Red | Green"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 39 } }
                            , value =
                                GrenSyntax.ChoiceTypeDeclaration
                                    { documentation = Nothing
                                    , name =
                                        { range = { start = { row = 1, column = 6 }, end = { row = 1, column = 11 } }, value = "Color" }
                                    , generics = []
                                    , constructors =
                                        [ { range = { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } }
                                          , value =
                                                { name =
                                                    { range = { start = { row = 1, column = 14 }, end = { row = 1, column = 18 } }, value = "Blue" }
                                                , value =
                                                    Just
                                                        { range = { start = { row = 1, column = 19 }, end = { row = 1, column = 25 } }
                                                        , value =
                                                            GrenSyntax.TypeAnnotationConstruct
                                                                { reference =
                                                                    { range = { start = { row = 1, column = 19 }, end = { row = 1, column = 25 } }
                                                                    , value = { qualification = [], name = "String" }
                                                                    }
                                                                , arguments = []
                                                                }
                                                        }
                                                }
                                          }
                                        , { range = { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } }
                                          , value =
                                                { name =
                                                    { range = { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } }, value = "Red" }
                                                , value = Nothing
                                                }
                                          }
                                        , { range = { start = { row = 1, column = 34 }, end = { row = 1, column = 39 } }
                                          , value =
                                                { name =
                                                    { range = { start = { row = 1, column = 34 }, end = { row = 1, column = 39 } }, value = "Green" }
                                                , value = Nothing
                                                }
                                          }
                                        ]
                                    }
                            }
                )
            , Test.test "type with extra | between variants"
                (\() ->
                    "type Color = Blue String ||Red | Green"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 39 } }
                            , value =
                                GrenSyntax.ChoiceTypeDeclaration
                                    { documentation = Nothing
                                    , name =
                                        { range = { start = { row = 1, column = 6 }, end = { row = 1, column = 11 } }, value = "Color" }
                                    , generics = []
                                    , constructors =
                                        [ { range = { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } }
                                          , value =
                                                { name =
                                                    { range = { start = { row = 1, column = 14 }, end = { row = 1, column = 18 } }, value = "Blue" }
                                                , value =
                                                    Just
                                                        { range = { start = { row = 1, column = 19 }, end = { row = 1, column = 25 } }
                                                        , value =
                                                            GrenSyntax.TypeAnnotationConstruct
                                                                { reference =
                                                                    { range = { start = { row = 1, column = 19 }, end = { row = 1, column = 25 } }
                                                                    , value = { qualification = [], name = "String" }
                                                                    }
                                                                , arguments = []
                                                                }
                                                        }
                                                }
                                          }
                                        , { range = { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } }
                                          , value =
                                                { name =
                                                    { range = { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } }, value = "Red" }
                                                , value = Nothing
                                                }
                                          }
                                        , { range = { start = { row = 1, column = 34 }, end = { row = 1, column = 39 } }
                                          , value =
                                                { name =
                                                    { range = { start = { row = 1, column = 34 }, end = { row = 1, column = 39 } }, value = "Green" }
                                                , value = Nothing
                                                }
                                          }
                                        ]
                                    }
                            }
                )
            , Test.test "type with documentation"
                (\() ->
                    """{-| Classic RGB -}
type Color = Blue String | Red | Green"""
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            { range = { start = { row = 1, column = 1 }, end = { row = 2, column = 39 } }
                            , value =
                                GrenSyntax.ChoiceTypeDeclaration
                                    { documentation =
                                        Just
                                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 19 } }
                                            , value = "{-| Classic RGB -}"
                                            }
                                    , name =
                                        { range = { start = { row = 2, column = 6 }, end = { row = 2, column = 11 } }, value = "Color" }
                                    , generics = []
                                    , constructors =
                                        [ { range = { start = { row = 2, column = 14 }, end = { row = 2, column = 25 } }
                                          , value =
                                                { name =
                                                    { range = { start = { row = 2, column = 14 }, end = { row = 2, column = 18 } }, value = "Blue" }
                                                , value =
                                                    Just
                                                        { range = { start = { row = 2, column = 19 }, end = { row = 2, column = 25 } }
                                                        , value =
                                                            GrenSyntax.TypeAnnotationConstruct
                                                                { reference =
                                                                    { range = { start = { row = 2, column = 19 }, end = { row = 2, column = 25 } }
                                                                    , value = { qualification = [], name = "String" }
                                                                    }
                                                                , arguments = []
                                                                }
                                                        }
                                                }
                                          }
                                        , { range = { start = { row = 2, column = 28 }, end = { row = 2, column = 31 } }
                                          , value =
                                                { name =
                                                    { range = { start = { row = 2, column = 28 }, end = { row = 2, column = 31 } }, value = "Red" }
                                                , value = Nothing
                                                }
                                          }
                                        , { range = { start = { row = 2, column = 34 }, end = { row = 2, column = 39 } }
                                          , value =
                                                { name =
                                                    { range = { start = { row = 2, column = 34 }, end = { row = 2, column = 39 } }, value = "Green" }
                                                , value = Nothing
                                                }
                                          }
                                        ]
                                    }
                            }
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
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                            , value =
                                GrenSyntax.ChoiceTypeDeclaration
                                    { documentation = Nothing
                                    , name =
                                        { range = { start = { row = 1, column = 6 }, end = { row = 1, column = 11 } }, value = "Maybe" }
                                    , generics =
                                        [ { range = { start = { row = 1, column = 12 }, end = { row = 1, column = 13 } }, value = "a" }
                                        ]
                                    , constructors =
                                        [ { range = { start = { row = 1, column = 16 }, end = { row = 1, column = 22 } }
                                          , value =
                                                { name =
                                                    { range = { start = { row = 1, column = 16 }, end = { row = 1, column = 20 } }, value = "Just" }
                                                , value =
                                                    Just
                                                        { range = { start = { row = 1, column = 21 }, end = { row = 1, column = 22 } }
                                                        , value = GrenSyntax.TypeAnnotationVariable "a"
                                                        }
                                                }
                                          }
                                        , { range = { start = { row = 1, column = 25 }, end = { row = 1, column = 32 } }
                                          , value =
                                                { name =
                                                    { range = { start = { row = 1, column = 25 }, end = { row = 1, column = 32 } }, value = "Nothing" }
                                                , value = Nothing
                                                }
                                          }
                                        ]
                                    }
                            }
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
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 28 } }
                            , value =
                                GrenSyntax.InfixDeclaration
                                    { direction =
                                        { range = { start = { row = 1, column = 7 }, end = { row = 1, column = 12 } }
                                        , value = GrenSyntax.Right
                                        }
                                    , precedence =
                                        { range = { start = { row = 1, column = 13 }, end = { row = 1, column = 14 } }, value = 7 }
                                    , operator =
                                        { range = { start = { row = 1, column = 15 }, end = { row = 1, column = 20 } }, value = "</>" }
                                    , function =
                                        { range = { start = { row = 1, column = 23 }, end = { row = 1, column = 28 } }, value = "slash" }
                                    }
                            }
                )
            , Test.test "left infix"
                (\() ->
                    "infix left  8 (<?>) = questionMark"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 35 } }
                            , value =
                                GrenSyntax.InfixDeclaration
                                    { direction =
                                        { range = { start = { row = 1, column = 7 }, end = { row = 1, column = 11 } }
                                        , value = GrenSyntax.Left
                                        }
                                    , precedence =
                                        { range = { start = { row = 1, column = 13 }, end = { row = 1, column = 14 } }, value = 8 }
                                    , operator =
                                        { range = { start = { row = 1, column = 15 }, end = { row = 1, column = 20 } }, value = "<?>" }
                                    , function =
                                        { range = { start = { row = 1, column = 23 }, end = { row = 1, column = 35 } }
                                        , value = "questionMark"
                                        }
                                    }
                            }
                )
            , Test.test "non infix"
                (\() ->
                    "infix non   4 (==) = eq"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 24 } }
                            , value =
                                GrenSyntax.InfixDeclaration
                                    { direction =
                                        { range = { start = { row = 1, column = 7 }, end = { row = 1, column = 10 } }
                                        , value = GrenSyntax.Non
                                        }
                                    , precedence =
                                        { range = { start = { row = 1, column = 13 }, end = { row = 1, column = 14 } }, value = 4 }
                                    , operator =
                                        { range = { start = { row = 1, column = 15 }, end = { row = 1, column = 19 } }, value = "==" }
                                    , function =
                                        { range = { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }, value = "eq" }
                                    }
                            }
                )
            ]
        , Test.describe "type"
            [ Test.test "unitTypeReference"
                (\() ->
                    "()"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } }
                            , value = GrenSyntax.TypeAnnotationUnit
                            }
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
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                            , value =
                                GrenSyntax.TypeAnnotationConstruct
                                    { reference =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                                        , value = { qualification = [ "Foo" ], name = "Bar" }
                                        }
                                    , arguments = []
                                    }
                            }
                )
            , Test.test "typeAnnotationNoFn"
                (\() ->
                    "Bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                            , value =
                                GrenSyntax.TypeAnnotationConstruct
                                    { reference =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                        , value = { qualification = [], name = "Bar" }
                                        }
                                    , arguments = []
                                    }
                            }
                )
            , Test.test "typedTypeReference 1"
                (\() ->
                    "Foo () a Bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                            , value =
                                GrenSyntax.TypeAnnotationConstruct
                                    { reference =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                        , value = { qualification = [], name = "Foo" }
                                        }
                                    , arguments =
                                        [ { range = { start = { row = 1, column = 5 }, end = { row = 1, column = 7 } }
                                          , value = GrenSyntax.TypeAnnotationUnit
                                          }
                                        , { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } }
                                          , value = GrenSyntax.TypeAnnotationVariable "a"
                                          }
                                        , { range = { start = { row = 1, column = 10 }, end = { row = 1, column = 13 } }
                                          , value =
                                                GrenSyntax.TypeAnnotationConstruct
                                                    { reference =
                                                        { range = { start = { row = 1, column = 10 }, end = { row = 1, column = 13 } }
                                                        , value = { qualification = [], name = "Bar" }
                                                        }
                                                    , arguments = []
                                                    }
                                          }
                                        ]
                                    }
                            }
                )
            , Test.test "typedTypeReference 2"
                (\() ->
                    "Foo () a Bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                            , value =
                                GrenSyntax.TypeAnnotationConstruct
                                    { reference =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                        , value = { qualification = [], name = "Foo" }
                                        }
                                    , arguments =
                                        [ { range = { start = { row = 1, column = 5 }, end = { row = 1, column = 7 } }
                                          , value = GrenSyntax.TypeAnnotationUnit
                                          }
                                        , { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } }
                                          , value = GrenSyntax.TypeAnnotationVariable "a"
                                          }
                                        , { range = { start = { row = 1, column = 10 }, end = { row = 1, column = 13 } }
                                          , value =
                                                GrenSyntax.TypeAnnotationConstruct
                                                    { reference =
                                                        { range = { start = { row = 1, column = 10 }, end = { row = 1, column = 13 } }
                                                        , value = { qualification = [], name = "Bar" }
                                                        }
                                                    , arguments = []
                                                    }
                                          }
                                        ]
                                    }
                            }
                )
            , Test.test "recordTypeReference empty"
                (\() ->
                    "{}"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } }
                            , value = GrenSyntax.TypeAnnotationRecord []
                            }
                )
            , Test.test "recordTypeReference one field"
                (\() ->
                    "{color: String }"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 17 } }
                            , value =
                                GrenSyntax.TypeAnnotationRecord
                                    [ { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 15 } }
                                      , value =
                                            { name =
                                                { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } }, value = "color" }
                                            , value =
                                                { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 15 } }
                                                , value =
                                                    GrenSyntax.TypeAnnotationConstruct
                                                        { reference =
                                                            { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 15 } }
                                                            , value = { qualification = [], name = "String" }
                                                            }
                                                        , arguments = []
                                                        }
                                                }
                                            }
                                      }
                                    ]
                            }
                )
            , Test.test "recordTypeReference one field with name-value separator ="
                (\() ->
                    "{color= String }"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 17 } }
                            , value =
                                GrenSyntax.TypeAnnotationRecord
                                    [ { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 15 } }
                                      , value =
                                            { name =
                                                { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } }, value = "color" }
                                            , value =
                                                { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 15 } }
                                                , value =
                                                    GrenSyntax.TypeAnnotationConstruct
                                                        { reference =
                                                            { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 15 } }
                                                            , value = { qualification = [], name = "String" }
                                                            }
                                                        , arguments = []
                                                        }
                                                }
                                            }
                                      }
                                    ]
                            }
                )
            , Test.test "recordTypeReference one field with name-value separator empty"
                (\() ->
                    "{color  String }"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 17 } }
                            , value =
                                GrenSyntax.TypeAnnotationRecord
                                    [ { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 15 } }
                                      , value =
                                            { name =
                                                { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } }, value = "color" }
                                            , value =
                                                { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 15 } }
                                                , value =
                                                    GrenSyntax.TypeAnnotationConstruct
                                                        { reference =
                                                            { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 15 } }
                                                            , value = { qualification = [], name = "String" }
                                                            }
                                                        , arguments = []
                                                        }
                                                }
                                            }
                                      }
                                    ]
                            }
                )
            , Test.test "type record with field name colliding with keyword"
                (\() ->
                    "{  as : Int, b : Int }"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            { range = { end = { column = 23, row = 1 }, start = { column = 1, row = 1 } }
                            , value =
                                GrenSyntax.TypeAnnotationRecord
                                    [ { range = { end = { column = 12, row = 1 }, start = { column = 4, row = 1 } }
                                      , value =
                                            { name =
                                                { range = { end = { column = 6, row = 1 }, start = { column = 4, row = 1 } }, value = "as_" }
                                            , value =
                                                { range = { end = { column = 12, row = 1 }, start = { column = 9, row = 1 } }
                                                , value =
                                                    GrenSyntax.TypeAnnotationConstruct
                                                        { reference =
                                                            { range = { end = { column = 12, row = 1 }, start = { column = 9, row = 1 } }
                                                            , value = { qualification = [], name = "Int" }
                                                            }
                                                        , arguments = []
                                                        }
                                                }
                                            }
                                      }
                                    , { range = { end = { column = 22, row = 1 }, start = { column = 14, row = 1 } }
                                      , value =
                                            { name =
                                                { range = { end = { column = 15, row = 1 }, start = { column = 14, row = 1 } }, value = "b" }
                                            , value =
                                                { range = { end = { column = 21, row = 1 }, start = { column = 18, row = 1 } }
                                                , value =
                                                    GrenSyntax.TypeAnnotationConstruct
                                                        { reference =
                                                            { range = { end = { column = 21, row = 1 }, start = { column = 18, row = 1 } }
                                                            , value = { qualification = [], name = "Int" }
                                                            }
                                                        , arguments = []
                                                        }
                                                }
                                            }
                                      }
                                    ]
                            }
                )
            , Test.test "type record with prefixed comma"
                (\() ->
                    "{ , a : Int, b : Int }"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            { range = { end = { column = 23, row = 1 }, start = { column = 1, row = 1 } }
                            , value =
                                GrenSyntax.TypeAnnotationRecord
                                    [ { range = { end = { column = 12, row = 1 }, start = { column = 5, row = 1 } }
                                      , value =
                                            { name = { range = { end = { column = 6, row = 1 }, start = { column = 5, row = 1 } }, value = "a" }
                                            , value =
                                                { range = { end = { column = 12, row = 1 }, start = { column = 9, row = 1 } }
                                                , value =
                                                    GrenSyntax.TypeAnnotationConstruct
                                                        { reference =
                                                            { range = { end = { column = 12, row = 1 }, start = { column = 9, row = 1 } }
                                                            , value = { qualification = [], name = "Int" }
                                                            }
                                                        , arguments = []
                                                        }
                                                }
                                            }
                                      }
                                    , { range = { end = { column = 22, row = 1 }, start = { column = 14, row = 1 } }
                                      , value =
                                            { name =
                                                { range = { end = { column = 15, row = 1 }, start = { column = 14, row = 1 } }, value = "b" }
                                            , value =
                                                { range = { end = { column = 21, row = 1 }, start = { column = 18, row = 1 } }
                                                , value =
                                                    GrenSyntax.TypeAnnotationConstruct
                                                        { reference =
                                                            { range = { end = { column = 21, row = 1 }, start = { column = 18, row = 1 } }
                                                            , value = { qualification = [], name = "Int" }
                                                            }
                                                        , arguments = []
                                                        }
                                                }
                                            }
                                      }
                                    ]
                            }
                )
            , Test.test "type record with extra comma between fields"
                (\() ->
                    "{   a : Int,,b : Int }"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            { range = { end = { column = 23, row = 1 }, start = { column = 1, row = 1 } }
                            , value =
                                GrenSyntax.TypeAnnotationRecord
                                    [ { range = { end = { column = 12, row = 1 }, start = { column = 5, row = 1 } }
                                      , value =
                                            { name = { range = { end = { column = 6, row = 1 }, start = { column = 5, row = 1 } }, value = "a" }
                                            , value =
                                                { range = { end = { column = 12, row = 1 }, start = { column = 9, row = 1 } }
                                                , value =
                                                    GrenSyntax.TypeAnnotationConstruct
                                                        { reference =
                                                            { range = { end = { column = 12, row = 1 }, start = { column = 9, row = 1 } }
                                                            , value = { qualification = [], name = "Int" }
                                                            }
                                                        , arguments = []
                                                        }
                                                }
                                            }
                                      }
                                    , { range = { end = { column = 22, row = 1 }, start = { column = 14, row = 1 } }
                                      , value =
                                            { name =
                                                { range = { end = { column = 15, row = 1 }, start = { column = 14, row = 1 } }, value = "b" }
                                            , value =
                                                { range = { end = { column = 21, row = 1 }, start = { column = 18, row = 1 } }
                                                , value =
                                                    GrenSyntax.TypeAnnotationConstruct
                                                        { reference =
                                                            { range = { end = { column = 21, row = 1 }, start = { column = 18, row = 1 } }
                                                            , value = { qualification = [], name = "Int" }
                                                            }
                                                        , arguments = []
                                                        }
                                                }
                                            }
                                      }
                                    ]
                            }
                )
            , Test.test "record with generic"
                (\() ->
                    "{ attr | position : Vec2, texture : Vec2 }"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 43 } }
                            , value =
                                GrenSyntax.TypeAnnotationRecordExtension
                                    { recordVariable = { range = { start = { row = 1, column = 3 }, end = { row = 1, column = 7 } }, value = "attr" }
                                    , fields =
                                        { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 42 } }
                                        , value =
                                            [ { range = { start = { row = 1, column = 10 }, end = { row = 1, column = 25 } }
                                              , value =
                                                    { name =
                                                        { range = { start = { row = 1, column = 10 }, end = { row = 1, column = 18 } }, value = "position" }
                                                    , value =
                                                        { range = { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } }
                                                        , value =
                                                            GrenSyntax.TypeAnnotationConstruct
                                                                { reference =
                                                                    { range = { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } }
                                                                    , value = { qualification = [], name = "Vec2" }
                                                                    }
                                                                , arguments = []
                                                                }
                                                        }
                                                    }
                                              }
                                            , { range = { start = { row = 1, column = 27 }, end = { row = 1, column = 42 } }
                                              , value =
                                                    { name =
                                                        { range = { start = { row = 1, column = 27 }, end = { row = 1, column = 34 } }, value = "texture" }
                                                    , value =
                                                        { range = { start = { row = 1, column = 37 }, end = { row = 1, column = 41 } }
                                                        , value =
                                                            GrenSyntax.TypeAnnotationConstruct
                                                                { reference =
                                                                    { range = { start = { row = 1, column = 37 }, end = { row = 1, column = 41 } }
                                                                    , value = { qualification = [], name = "Vec2" }
                                                                    }
                                                                , arguments = []
                                                                }
                                                        }
                                                    }
                                              }
                                            ]
                                        }
                                    }
                            }
                )
            , Test.test "record with generic with extra comma between fields"
                (\() ->
                    "{ attr | position : Vec2,,texture : Vec2 }"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 43 } }
                            , value =
                                GrenSyntax.TypeAnnotationRecordExtension
                                    { recordVariable = { range = { start = { row = 1, column = 3 }, end = { row = 1, column = 7 } }, value = "attr" }
                                    , fields =
                                        { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 42 } }
                                        , value =
                                            [ { range = { start = { row = 1, column = 10 }, end = { row = 1, column = 25 } }
                                              , value =
                                                    { name =
                                                        { range = { start = { row = 1, column = 10 }, end = { row = 1, column = 18 } }, value = "position" }
                                                    , value =
                                                        { range = { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } }
                                                        , value =
                                                            GrenSyntax.TypeAnnotationConstruct
                                                                { reference =
                                                                    { range = { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } }
                                                                    , value = { qualification = [], name = "Vec2" }
                                                                    }
                                                                , arguments = []
                                                                }
                                                        }
                                                    }
                                              }
                                            , { range = { start = { row = 1, column = 27 }, end = { row = 1, column = 42 } }
                                              , value =
                                                    { name =
                                                        { range = { start = { row = 1, column = 27 }, end = { row = 1, column = 34 } }, value = "texture" }
                                                    , value =
                                                        { range = { start = { row = 1, column = 37 }, end = { row = 1, column = 41 } }
                                                        , value =
                                                            GrenSyntax.TypeAnnotationConstruct
                                                                { reference =
                                                                    { range = { start = { row = 1, column = 37 }, end = { row = 1, column = 41 } }
                                                                    , value = { qualification = [], name = "Vec2" }
                                                                    }
                                                                , arguments = []
                                                                }
                                                        }
                                                    }
                                              }
                                            ]
                                        }
                                    }
                            }
                )
            , Test.test "record with generic, with name-value separator = and name-value separator empty"
                (\() ->
                    "{ attr | position   Vec2, texture = Vec2 }"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 43 } }
                            , value =
                                GrenSyntax.TypeAnnotationRecordExtension
                                    { recordVariable = { range = { start = { row = 1, column = 3 }, end = { row = 1, column = 7 } }, value = "attr" }
                                    , fields =
                                        { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 42 } }
                                        , value =
                                            [ { range = { start = { row = 1, column = 10 }, end = { row = 1, column = 25 } }
                                              , value =
                                                    { name =
                                                        { range = { start = { row = 1, column = 10 }, end = { row = 1, column = 18 } }, value = "position" }
                                                    , value =
                                                        { range = { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } }
                                                        , value =
                                                            GrenSyntax.TypeAnnotationConstruct
                                                                { reference =
                                                                    { range = { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } }
                                                                    , value = { qualification = [], name = "Vec2" }
                                                                    }
                                                                , arguments = []
                                                                }
                                                        }
                                                    }
                                              }
                                            , { range = { start = { row = 1, column = 27 }, end = { row = 1, column = 42 } }
                                              , value =
                                                    { name =
                                                        { range = { start = { row = 1, column = 27 }, end = { row = 1, column = 34 } }, value = "texture" }
                                                    , value =
                                                        { range = { start = { row = 1, column = 37 }, end = { row = 1, column = 41 } }
                                                        , value =
                                                            GrenSyntax.TypeAnnotationConstruct
                                                                { reference =
                                                                    { range = { start = { row = 1, column = 37 }, end = { row = 1, column = 41 } }
                                                                    , value = { qualification = [], name = "Vec2" }
                                                                    }
                                                                , arguments = []
                                                                }
                                                        }
                                                    }
                                              }
                                            ]
                                        }
                                    }
                            }
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
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 37 } }
                            , value =
                                GrenSyntax.TypeAnnotationRecord
                                    [ { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 35 } }
                                      , value =
                                            { name =
                                                { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } }, value = "color" }
                                            , value =
                                                { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 35 } }
                                                , value =
                                                    GrenSyntax.TypeAnnotationRecord
                                                        [ { range = { start = { row = 1, column = 10 }, end = { row = 1, column = 17 } }
                                                          , value =
                                                                { name =
                                                                    { range = { start = { row = 1, column = 10 }, end = { row = 1, column = 11 } }, value = "r" }
                                                                , value =
                                                                    { range = { start = { row = 1, column = 14 }, end = { row = 1, column = 17 } }
                                                                    , value =
                                                                        GrenSyntax.TypeAnnotationConstruct
                                                                            { reference =
                                                                                { range = { start = { row = 1, column = 14 }, end = { row = 1, column = 17 } }
                                                                                , value = { qualification = [], name = "Int" }
                                                                                }
                                                                            , arguments = []
                                                                            }
                                                                    }
                                                                }
                                                          }
                                                        , { range = { start = { row = 1, column = 19 }, end = { row = 1, column = 25 } }
                                                          , value =
                                                                { name =
                                                                    { range = { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } }, value = "g" }
                                                                , value =
                                                                    { range = { start = { row = 1, column = 22 }, end = { row = 1, column = 25 } }
                                                                    , value =
                                                                        GrenSyntax.TypeAnnotationConstruct
                                                                            { reference =
                                                                                { range = { start = { row = 1, column = 22 }, end = { row = 1, column = 25 } }
                                                                                , value = { qualification = [], name = "Int" }
                                                                                }
                                                                            , arguments = []
                                                                            }
                                                                    }
                                                                }
                                                          }
                                                        , { range = { start = { row = 1, column = 27 }, end = { row = 1, column = 34 } }
                                                          , value =
                                                                { name =
                                                                    { range = { start = { row = 1, column = 27 }, end = { row = 1, column = 28 } }, value = "b" }
                                                                , value =
                                                                    { range = { start = { row = 1, column = 30 }, end = { row = 1, column = 33 } }
                                                                    , value =
                                                                        GrenSyntax.TypeAnnotationConstruct
                                                                            { reference =
                                                                                { range = { start = { row = 1, column = 30 }, end = { row = 1, column = 33 } }
                                                                                , value = { qualification = [], name = "Int" }
                                                                                }
                                                                            , arguments = []
                                                                            }
                                                                    }
                                                                }
                                                          }
                                                        ]
                                                }
                                            }
                                      }
                                    ]
                            }
                )
            , Test.test "record field ranges"
                (\() ->
                    "{ foo : Int, bar : Int, baz : Int }"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 36 } }
                            , value =
                                GrenSyntax.TypeAnnotationRecord
                                    [ { range = { start = { row = 1, column = 3 }, end = { row = 1, column = 12 } }
                                      , value =
                                            { name =
                                                { range = { start = { row = 1, column = 3 }, end = { row = 1, column = 6 } }, value = "foo" }
                                            , value =
                                                { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } }
                                                , value =
                                                    GrenSyntax.TypeAnnotationConstruct
                                                        { reference =
                                                            { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } }
                                                            , value = { qualification = [], name = "Int" }
                                                            }
                                                        , arguments = []
                                                        }
                                                }
                                            }
                                      }
                                    , { range = { start = { row = 1, column = 14 }, end = { row = 1, column = 23 } }
                                      , value =
                                            { name =
                                                { range = { start = { row = 1, column = 14 }, end = { row = 1, column = 17 } }, value = "bar" }
                                            , value =
                                                { range = { start = { row = 1, column = 20 }, end = { row = 1, column = 23 } }
                                                , value =
                                                    GrenSyntax.TypeAnnotationConstruct
                                                        { reference =
                                                            { range = { start = { row = 1, column = 20 }, end = { row = 1, column = 23 } }
                                                            , value = { qualification = [], name = "Int" }
                                                            }
                                                        , arguments = []
                                                        }
                                                }
                                            }
                                      }
                                    , { range = { start = { row = 1, column = 25 }, end = { row = 1, column = 35 } }
                                      , value =
                                            { name =
                                                { range = { start = { row = 1, column = 25 }, end = { row = 1, column = 28 } }, value = "baz" }
                                            , value =
                                                { range = { start = { row = 1, column = 31 }, end = { row = 1, column = 34 } }
                                                , value =
                                                    GrenSyntax.TypeAnnotationConstruct
                                                        { reference =
                                                            { range = { start = { row = 1, column = 31 }, end = { row = 1, column = 34 } }
                                                            , value = { qualification = [], name = "Int" }
                                                            }
                                                        , arguments = []
                                                        }
                                                }
                                            }
                                      }
                                    ]
                            }
                )
            , Test.test "recordTypeReference with generic"
                (\() ->
                    "{color: s }"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                            , value =
                                GrenSyntax.TypeAnnotationRecord
                                    [ { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 10 } }
                                      , value =
                                            { name =
                                                { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } }, value = "color" }
                                            , value =
                                                { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } }
                                                , value = GrenSyntax.TypeAnnotationVariable "s"
                                                }
                                            }
                                      }
                                    ]
                            }
                )
            , Test.test "function type reference"
                (\() ->
                    "Foo -> Bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                            , value =
                                GrenSyntax.TypeAnnotationFunction
                                    { input =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                        , value =
                                            GrenSyntax.TypeAnnotationConstruct
                                                { reference =
                                                    { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                                    , value = { qualification = [], name = "Foo" }
                                                    }
                                                , arguments = []
                                                }
                                        }
                                    , output =
                                        { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }
                                        , value =
                                            GrenSyntax.TypeAnnotationConstruct
                                                { reference =
                                                    { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }
                                                    , value = { qualification = [], name = "Bar" }
                                                    }
                                                , arguments = []
                                                }
                                        }
                                    }
                            }
                )
            , Test.test "function type reference multiple"
                (\() ->
                    "Foo -> Bar -> baz"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 18 } }
                            , value =
                                GrenSyntax.TypeAnnotationFunction
                                    { input =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                        , value =
                                            GrenSyntax.TypeAnnotationConstruct
                                                { reference =
                                                    { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                                    , value = { qualification = [], name = "Foo" }
                                                    }
                                                , arguments = []
                                                }
                                        }
                                    , output =
                                        { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 18 } }
                                        , value =
                                            GrenSyntax.TypeAnnotationFunction
                                                { input =
                                                    { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }
                                                    , value =
                                                        GrenSyntax.TypeAnnotationConstruct
                                                            { reference =
                                                                { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }
                                                                , value = { qualification = [], name = "Bar" }
                                                                }
                                                            , arguments = []
                                                            }
                                                    }
                                                , output =
                                                    { range = { start = { row = 1, column = 15 }, end = { row = 1, column = 18 } }
                                                    , value = GrenSyntax.TypeAnnotationVariable "baz"
                                                    }
                                                }
                                        }
                                    }
                            }
                )
            , Test.test "function type reference multiple with consecutive ->"
                (\() ->
                    "Foo->->Bar -> baz"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 18 } }
                            , value =
                                GrenSyntax.TypeAnnotationFunction
                                    { input =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                        , value =
                                            GrenSyntax.TypeAnnotationConstruct
                                                { reference =
                                                    { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                                    , value = { qualification = [], name = "Foo" }
                                                    }
                                                , arguments = []
                                                }
                                        }
                                    , output =
                                        { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 18 } }
                                        , value =
                                            GrenSyntax.TypeAnnotationFunction
                                                { input =
                                                    { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }
                                                    , value =
                                                        GrenSyntax.TypeAnnotationConstruct
                                                            { reference =
                                                                { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }
                                                                , value = { qualification = [], name = "Bar" }
                                                                }
                                                            , arguments = []
                                                            }
                                                    }
                                                , output =
                                                    { range = { start = { row = 1, column = 15 }, end = { row = 1, column = 18 } }
                                                    , value = GrenSyntax.TypeAnnotationVariable "baz"
                                                    }
                                                }
                                        }
                                    }
                            }
                )
            , Test.test "function type reference generics"
                (\() ->
                    "cMsg -> cModel -> a"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 20 } }
                            , value =
                                GrenSyntax.TypeAnnotationFunction
                                    { input =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }
                                        , value = GrenSyntax.TypeAnnotationVariable "cMsg"
                                        }
                                    , output =
                                        { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 20 } }
                                        , value =
                                            GrenSyntax.TypeAnnotationFunction
                                                { input =
                                                    { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 15 } }
                                                    , value = GrenSyntax.TypeAnnotationVariable "cModel"
                                                    }
                                                , output =
                                                    { range = { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } }
                                                    , value = GrenSyntax.TypeAnnotationVariable "a"
                                                    }
                                                }
                                        }
                                    }
                            }
                )
            , Test.test "annotation with parens"
                (\() ->
                    "Msg -> Model -> (Model->Cmd Msg)"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            { range = { end = { column = 33, row = 1 }, start = { column = 1, row = 1 } }
                            , value =
                                GrenSyntax.TypeAnnotationFunction
                                    { input =
                                        { range = { end = { column = 4, row = 1 }, start = { column = 1, row = 1 } }
                                        , value =
                                            GrenSyntax.TypeAnnotationConstruct
                                                { reference =
                                                    { range = { end = { column = 4, row = 1 }, start = { column = 1, row = 1 } }
                                                    , value = { qualification = [], name = "Msg" }
                                                    }
                                                , arguments = []
                                                }
                                        }
                                    , output =
                                        { range = { end = { column = 33, row = 1 }, start = { column = 8, row = 1 } }
                                        , value =
                                            GrenSyntax.TypeAnnotationFunction
                                                { input =
                                                    { range = { end = { column = 13, row = 1 }, start = { column = 8, row = 1 } }
                                                    , value =
                                                        GrenSyntax.TypeAnnotationConstruct
                                                            { reference =
                                                                { range = { end = { column = 13, row = 1 }, start = { column = 8, row = 1 } }
                                                                , value = { qualification = [], name = "Model" }
                                                                }
                                                            , arguments = []
                                                            }
                                                    }
                                                , output =
                                                    { range = { end = { column = 33, row = 1 }, start = { column = 17, row = 1 } }
                                                    , value =
                                                        GrenSyntax.TypeAnnotationParenthesized
                                                            { range = { end = { column = 32, row = 1 }, start = { column = 18, row = 1 } }
                                                            , value =
                                                                GrenSyntax.TypeAnnotationFunction
                                                                    { input =
                                                                        { range = { end = { column = 23, row = 1 }, start = { column = 18, row = 1 } }
                                                                        , value =
                                                                            GrenSyntax.TypeAnnotationConstruct
                                                                                { reference =
                                                                                    { range = { end = { column = 23, row = 1 }, start = { column = 18, row = 1 } }
                                                                                    , value = { qualification = [], name = "Model" }
                                                                                    }
                                                                                , arguments = []
                                                                                }
                                                                        }
                                                                    , output =
                                                                        { range = { end = { column = 32, row = 1 }, start = { column = 25, row = 1 } }
                                                                        , value =
                                                                            GrenSyntax.TypeAnnotationConstruct
                                                                                { reference =
                                                                                    { range = { end = { column = 28, row = 1 }, start = { column = 25, row = 1 } }
                                                                                    , value = { qualification = [], name = "Cmd" }
                                                                                    }
                                                                                , arguments =
                                                                                    [ { range = { end = { column = 32, row = 1 }, start = { column = 29, row = 1 } }
                                                                                      , value =
                                                                                            GrenSyntax.TypeAnnotationConstruct
                                                                                                { reference =
                                                                                                    { range = { end = { column = 32, row = 1 }, start = { column = 29, row = 1 } }
                                                                                                    , value = { qualification = [], name = "Msg" }
                                                                                                    }
                                                                                                , arguments = []
                                                                                                }
                                                                                      }
                                                                                    ]
                                                                                }
                                                                        }
                                                                    }
                                                            }
                                                    }
                                                }
                                        }
                                    }
                            }
                )
            , Test.test "function as argument"
                (\() ->
                    "( cMsg -> cModel -> a ) -> b"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            { range = { end = { column = 29, row = 1 }, start = { column = 1, row = 1 } }
                            , value =
                                GrenSyntax.TypeAnnotationFunction
                                    { input =
                                        { range = { end = { column = 24, row = 1 }, start = { column = 1, row = 1 } }
                                        , value =
                                            GrenSyntax.TypeAnnotationParenthesized
                                                { range = { end = { column = 22, row = 1 }, start = { column = 3, row = 1 } }
                                                , value =
                                                    GrenSyntax.TypeAnnotationFunction
                                                        { input =
                                                            { range = { end = { column = 7, row = 1 }, start = { column = 3, row = 1 } }
                                                            , value = GrenSyntax.TypeAnnotationVariable "cMsg"
                                                            }
                                                        , output =
                                                            { range = { end = { column = 22, row = 1 }, start = { column = 11, row = 1 } }
                                                            , value =
                                                                GrenSyntax.TypeAnnotationFunction
                                                                    { input =
                                                                        { range = { end = { column = 17, row = 1 }, start = { column = 11, row = 1 } }
                                                                        , value = GrenSyntax.TypeAnnotationVariable "cModel"
                                                                        }
                                                                    , output =
                                                                        { range = { end = { column = 22, row = 1 }, start = { column = 21, row = 1 } }
                                                                        , value = GrenSyntax.TypeAnnotationVariable "a"
                                                                        }
                                                                    }
                                                            }
                                                        }
                                                }
                                        }
                                    , output =
                                        { range = { end = { column = 29, row = 1 }, start = { column = 28, row = 1 } }
                                        , value = GrenSyntax.TypeAnnotationVariable "b"
                                        }
                                    }
                            }
                )
            , Test.test "type with params"
                (\() ->
                    "(Foo -> Bar)"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            { range = { end = { column = 13, row = 1 }, start = { column = 1, row = 1 } }
                            , value =
                                GrenSyntax.TypeAnnotationParenthesized
                                    { range = { end = { column = 12, row = 1 }, start = { column = 2, row = 1 } }
                                    , value =
                                        GrenSyntax.TypeAnnotationFunction
                                            { input =
                                                { range = { end = { column = 5, row = 1 }, start = { column = 2, row = 1 } }
                                                , value =
                                                    GrenSyntax.TypeAnnotationConstruct
                                                        { reference =
                                                            { range = { end = { column = 5, row = 1 }, start = { column = 2, row = 1 } }
                                                            , value = { qualification = [], name = "Foo" }
                                                            }
                                                        , arguments = []
                                                        }
                                                }
                                            , output =
                                                { range = { end = { column = 12, row = 1 }, start = { column = 9, row = 1 } }
                                                , value =
                                                    GrenSyntax.TypeAnnotationConstruct
                                                        { reference =
                                                            { range = { end = { column = 12, row = 1 }, start = { column = 9, row = 1 } }
                                                            , value = { qualification = [], name = "Bar" }
                                                            }
                                                        , arguments = []
                                                        }
                                                }
                                            }
                                    }
                            }
                )
            , Test.test "function type reference multiple and parens"
                (\() ->
                    "(Foo -> Bar) -> baz"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            { range = { end = { column = 20, row = 1 }, start = { column = 1, row = 1 } }
                            , value =
                                GrenSyntax.TypeAnnotationFunction
                                    { input =
                                        { range = { end = { column = 13, row = 1 }, start = { column = 1, row = 1 } }
                                        , value =
                                            GrenSyntax.TypeAnnotationParenthesized
                                                { range = { end = { column = 12, row = 1 }, start = { column = 2, row = 1 } }
                                                , value =
                                                    GrenSyntax.TypeAnnotationFunction
                                                        { input =
                                                            { range = { end = { column = 5, row = 1 }, start = { column = 2, row = 1 } }
                                                            , value =
                                                                GrenSyntax.TypeAnnotationConstruct
                                                                    { reference =
                                                                        { range = { end = { column = 5, row = 1 }, start = { column = 2, row = 1 } }
                                                                        , value = { qualification = [], name = "Foo" }
                                                                        }
                                                                    , arguments = []
                                                                    }
                                                            }
                                                        , output =
                                                            { range = { end = { column = 12, row = 1 }, start = { column = 9, row = 1 } }
                                                            , value =
                                                                GrenSyntax.TypeAnnotationConstruct
                                                                    { reference =
                                                                        { range = { end = { column = 12, row = 1 }, start = { column = 9, row = 1 } }
                                                                        , value = { qualification = [], name = "Bar" }
                                                                        }
                                                                    , arguments = []
                                                                    }
                                                            }
                                                        }
                                                }
                                        }
                                    , output =
                                        { range = { end = { column = 20, row = 1 }, start = { column = 17, row = 1 } }
                                        , value = GrenSyntax.TypeAnnotationVariable "baz"
                                        }
                                    }
                            }
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
                            { range = { start = { row = 1, column = 1 }, end = { row = 2, column = 3 } }
                            , value =
                                GrenSyntax.TypeAnnotationConstruct
                                    { reference =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                                        , value = { qualification = [], name = "Maybe" }
                                        }
                                    , arguments =
                                        [ { range = { start = { row = 2, column = 2 }, end = { row = 2, column = 3 } }
                                          , value = GrenSyntax.TypeAnnotationVariable "a"
                                          }
                                        ]
                                    }
                            }
                )
            , Test.test "issue #5 - no spaces between type and generic with parens"
                (\() ->
                    "List(String)"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            { range = { end = { column = 13, row = 1 }, start = { column = 1, row = 1 } }
                            , value =
                                GrenSyntax.TypeAnnotationConstruct
                                    { reference =
                                        { range = { end = { column = 5, row = 1 }, start = { column = 1, row = 1 } }
                                        , value = { qualification = [], name = "List" }
                                        }
                                    , arguments =
                                        [ { range = { end = { column = 13, row = 1 }, start = { column = 5, row = 1 } }
                                          , value =
                                                GrenSyntax.TypeAnnotationParenthesized
                                                    { range = { end = { column = 12, row = 1 }, start = { column = 6, row = 1 } }
                                                    , value =
                                                        GrenSyntax.TypeAnnotationConstruct
                                                            { reference =
                                                                { range = { end = { column = 12, row = 1 }, start = { column = 6, row = 1 } }
                                                                , value = { qualification = [], name = "String" }
                                                                }
                                                            , arguments = []
                                                            }
                                                    }
                                          }
                                        ]
                                    }
                            }
                )
            , Test.test "parse type with multiple params"
                (\() ->
                    "Dict String Int"
                        |> expectSyntaxWithoutComments GrenParserLenient.type_
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 16 } }
                            , value =
                                GrenSyntax.TypeAnnotationConstruct
                                    { reference =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }
                                        , value = { qualification = [], name = "Dict" }
                                        }
                                    , arguments =
                                        [ { range = { start = { row = 1, column = 6 }, end = { row = 1, column = 12 } }
                                          , value =
                                                GrenSyntax.TypeAnnotationConstruct
                                                    { reference =
                                                        { range = { start = { row = 1, column = 6 }, end = { row = 1, column = 12 } }
                                                        , value = { qualification = [], name = "String" }
                                                        }
                                                    , arguments = []
                                                    }
                                          }
                                        , { range = { start = { row = 1, column = 13 }, end = { row = 1, column = 16 } }
                                          , value =
                                                GrenSyntax.TypeAnnotationConstruct
                                                    { reference =
                                                        { range = { start = { row = 1, column = 13 }, end = { row = 1, column = 16 } }
                                                        , value = { qualification = [], name = "Int" }
                                                        }
                                                    , arguments = []
                                                    }
                                          }
                                        ]
                                    }
                            }
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
                                (Just (GrenSyntax.ExpressionHex 67108863))
                    )
                , Test.test "hex FF"
                    (\() ->
                        "0xFF"
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                            |> Expect.equal
                                (Just (GrenSyntax.ExpressionHex 255))
                    )
                , Test.test "hex"
                    (\() ->
                        "0x2A"
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                            |> Expect.equal
                                (Just (GrenSyntax.ExpressionHex 42))
                    )
                , Test.test "Hex integer literal"
                    (\() ->
                        "0x56"
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }
                                , value = GrenSyntax.ExpressionHex 86
                                }
                    )
                , Test.test "Integer literal"
                    (\() ->
                        "101"
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                , value = GrenSyntax.ExpressionInteger 101
                                }
                    )
                , Test.test "float"
                    (\() ->
                        "2.0"
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                            |> Expect.equal
                                (Just (GrenSyntax.ExpressionFloat 2.0))
                    )
                , Test.test "integer with negative exponent"
                    (\() ->
                        "2e-2"
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                            |> Expect.equal
                                (Just (GrenSyntax.ExpressionFloat 0.02))
                    )
                , Test.test "literal e is not a number"
                    (\() ->
                        "e"
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                            |> Expect.equal
                                (Just (GrenSyntax.ExpressionReference { qualification = [], name = "e" }))
                    )
                , Test.test "integer with negative exponent (uppercase E)"
                    (\() ->
                        "2E-2"
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                            |> Expect.equal
                                (Just (GrenSyntax.ExpressionFloat 0.02))
                    )
                , Test.test "integer with positive exponent"
                    (\() ->
                        "2e+2"
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                            |> Expect.equal
                                (Just (GrenSyntax.ExpressionFloat 200.0))
                    )
                , Test.test "float with negative exponent"
                    (\() ->
                        "2.0e-2"
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                            |> Expect.equal
                                (Just (GrenSyntax.ExpressionFloat 0.02))
                    )
                , Test.test "float with negative exponent (uppercase E)"
                    (\() ->
                        "2.0E-2"
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                            |> Expect.equal
                                (Just (GrenSyntax.ExpressionFloat 0.02))
                    )
                , Test.test "float with positive exponent"
                    (\() ->
                        "2.0e+2"
                            |> GrenParserLenient.run GrenParserLenient.expression
                            |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                            |> Expect.equal
                                (Just (GrenSyntax.ExpressionFloat 200.0))
                    )
                ]
            , Test.test "String literal"
                (\() ->
                    "\"Bar\""
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                            , value =
                                GrenSyntax.ExpressionString { quotingStyle = GrenSyntax.StringSingleQuoted, content = "Bar" }
                            }
                )
            , Test.test "multiline string"
                (\() ->
                    "\"\"\"Bar foo \n a\"\"\""
                        |> GrenParserLenient.run GrenParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                        |> Expect.equal (Just (GrenSyntax.ExpressionString { quotingStyle = GrenSyntax.StringTripleQuoted, content = "Bar foo \n a" }))
                )
            , Test.test "multiline string escape"
                (\() ->
                    """\"\"\" \\\"\"\" \"\"\""""
                        |> GrenParserLenient.run GrenParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                        |> Expect.equal (Just (GrenSyntax.ExpressionString { quotingStyle = GrenSyntax.StringTripleQuoted, content = """ \"\"\" """ }))
                )
            , Test.test "character escaped"
                (\() ->
                    "'\\''"
                        |> GrenParserLenient.run GrenParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                        |> Expect.equal (Just (GrenSyntax.ExpressionChar '\''))
                )
            , Test.test "character escaped - 2"
                (\() ->
                    "'\\r'"
                        |> GrenParserLenient.run GrenParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                        |> Expect.equal (Just (GrenSyntax.ExpressionChar '\u{000D}'))
                )
            , Test.test "unicode char"
                (\() ->
                    "'\\u{000D}'"
                        |> GrenParserLenient.run GrenParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                        |> Expect.equal (Just (GrenSyntax.ExpressionChar '\u{000D}'))
                )
            , Test.test "unicode char with lowercase hex"
                (\() ->
                    "'\\u{000d}'"
                        |> GrenParserLenient.run GrenParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                        |> Expect.equal (Just (GrenSyntax.ExpressionChar '\u{000D}'))
                )
            , Test.test "string escaped 3"
                (\() ->
                    "\"\\\"\""
                        |> GrenParserLenient.run GrenParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                        |> Expect.equal (Just (GrenSyntax.ExpressionString { quotingStyle = GrenSyntax.StringSingleQuoted, content = "\"" }))
                )
            , Test.test "string escaped"
                (\() ->
                    "\"foo\\\\\""
                        |> GrenParserLenient.run GrenParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                        |> Expect.equal (Just (GrenSyntax.ExpressionString { quotingStyle = GrenSyntax.StringSingleQuoted, content = "foo\\" }))
                )
            , Test.test "character escaped 3"
                (\() ->
                    "'\\n'"
                        |> GrenParserLenient.run GrenParserLenient.expression
                        |> Maybe.map (\result -> result.syntax |> GrenSyntax.nodeValue)
                        |> Expect.equal (Just (GrenSyntax.ExpressionChar '\n'))
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
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                            , value = GrenSyntax.ExpressionChar 'c'
                            }
                )
            , Test.test "String literal multiline"
                (\() ->
                    "\"\"\"Bar foo \n a\"\"\""
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } }
                            , value =
                                GrenSyntax.ExpressionString
                                    { quotingStyle = GrenSyntax.StringTripleQuoted, content = "Bar foo \n a" }
                            }
                )
            , Test.test "Regression test for multiline strings with backslashes"
                (\() ->
                    "a = \"\"\"\\{\\}\"\"\""
                        |> expectFailsToParse GrenParserLenient.declaration
                )
            , Test.test "Regression test 2 for multiline strings with backslashes"
                (\() ->
                    "\"\"\"\\\\{\\\\}\"\"\""
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                            , value =
                                GrenSyntax.ExpressionString { quotingStyle = GrenSyntax.StringTripleQuoted, content = "\\{\\}" }
                            }
                )
            , Test.test "Regression test 3 for multiline strings with backslashes"
                (\() ->
                    "\"\"\"\\\\a-blablabla-\\\\b\"\"\""
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 24 } }
                            , value =
                                GrenSyntax.ExpressionString
                                    { quotingStyle = GrenSyntax.StringTripleQuoted, content = "\\a-blablabla-\\b" }
                            }
                )
            , Test.test "Type expression for upper case"
                (\() ->
                    "Bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "Bar" }
                            }
                )
            , Test.test "Type expression for lower case"
                (\() ->
                    "bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "bar" }
                            }
                )
            , Test.test "Type expression for lower case but qualified"
                (\() ->
                    "Bar.foo"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                            , value = GrenSyntax.ExpressionReference { qualification = [ "Bar" ], name = "foo" }
                            }
                )
            , Test.test "parenthesizedExpression"
                (\() ->
                    "(bar)"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                            , value =
                                GrenSyntax.ExpressionParenthesized
                                    { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 5 } }
                                    , value = GrenSyntax.ExpressionReference { qualification = [], name = "bar" }
                                    }
                            }
                )
            , Test.test "parenthesized expression starting with a negation"
                (\() ->
                    "(-1 * sign)"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                            , value =
                                GrenSyntax.ExpressionParenthesized
                                    { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 11 } }
                                    , value =
                                        GrenSyntax.ExpressionInfixOperation
                                            { operator = "*"
                                            , left =
                                                { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } }
                                                , value =
                                                    GrenSyntax.ExpressionNegation
                                                        { range = { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } }
                                                        , value = GrenSyntax.ExpressionInteger 1
                                                        }
                                                }
                                            , right =
                                                { range = { start = { row = 1, column = 7 }, end = { row = 1, column = 11 } }
                                                , value = GrenSyntax.ExpressionReference { qualification = [], name = "sign" }
                                                }
                                            }
                                    }
                            }
                )
            , Test.test "application expression"
                (\() ->
                    "List.concat []"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                            , value =
                                GrenSyntax.ExpressionCall
                                    [ { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                      , value = GrenSyntax.ExpressionReference { qualification = [ "List" ], name = "concat" }
                                      }
                                    , { range = { start = { row = 1, column = 13 }, end = { row = 1, column = 15 } }
                                      , value = GrenSyntax.ExpressionArray []
                                      }
                                    ]
                            }
                )
            , Test.test "Binary operation"
                (\() ->
                    "model + 1"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                            , value =
                                GrenSyntax.ExpressionInfixOperation
                                    { operator = "+"
                                    , left =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "model" }
                                        }
                                    , right =
                                        { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } }
                                        , value = GrenSyntax.ExpressionInteger 1
                                        }
                                    }
                            }
                )
            , Test.test "Nested binary operations (+ and ==)"
                (\() ->
                    "count + 1 == 1"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                            , value =
                                GrenSyntax.ExpressionInfixOperation
                                    { operator = "=="
                                    , left =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                                        , value =
                                            GrenSyntax.ExpressionInfixOperation
                                                { operator = "+"
                                                , left =
                                                    { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                                                    , value = GrenSyntax.ExpressionReference { qualification = [], name = "count" }
                                                    }
                                                , right =
                                                    { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } }
                                                    , value = GrenSyntax.ExpressionInteger 1
                                                    }
                                                }
                                        }
                                    , right =
                                        { range = { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } }
                                        , value = GrenSyntax.ExpressionInteger 1
                                        }
                                    }
                            }
                )
            , Test.test "Nested binary operations (+ and !=)"
                (\() ->
                    "count + 1 != 1"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                            , value =
                                GrenSyntax.ExpressionInfixOperation
                                    { operator = "!="
                                    , left =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                                        , value =
                                            GrenSyntax.ExpressionInfixOperation
                                                { operator = "+"
                                                , left =
                                                    { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                                                    , value = GrenSyntax.ExpressionReference { qualification = [], name = "count" }
                                                    }
                                                , right =
                                                    { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } }
                                                    , value = GrenSyntax.ExpressionInteger 1
                                                    }
                                                }
                                        }
                                    , right =
                                        { range = { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } }
                                        , value = GrenSyntax.ExpressionInteger 1
                                        }
                                    }
                            }
                )
            , Test.test "Nested binary operations (+ and //)"
                (\() ->
                    "count + 1 // 2"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                            , value =
                                GrenSyntax.ExpressionInfixOperation
                                    { operator = "+"
                                    , left =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "count" }
                                        }
                                    , right =
                                        { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 15 } }
                                        , value =
                                            GrenSyntax.ExpressionInfixOperation
                                                { operator = "//"
                                                , left =
                                                    { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } }
                                                    , value = GrenSyntax.ExpressionInteger 1
                                                    }
                                                , right =
                                                    { range = { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } }
                                                    , value = GrenSyntax.ExpressionInteger 2
                                                    }
                                                }
                                        }
                                    }
                            }
                )
            , Test.test "Nested binary operations (&& and <|)"
                (\() ->
                    "condition && condition <| f"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 28 } }
                            , value =
                                GrenSyntax.ExpressionInfixOperation
                                    { operator = "<|"
                                    , left =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 23 } }
                                        , value =
                                            GrenSyntax.ExpressionInfixOperation
                                                { operator = "&&"
                                                , left =
                                                    { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                                                    , value = GrenSyntax.ExpressionReference { qualification = [], name = "condition" }
                                                    }
                                                , right =
                                                    { range = { start = { row = 1, column = 14 }, end = { row = 1, column = 23 } }
                                                    , value = GrenSyntax.ExpressionReference { qualification = [], name = "condition" }
                                                    }
                                                }
                                        }
                                    , right =
                                        { range = { start = { row = 1, column = 27 }, end = { row = 1, column = 28 } }
                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "f" }
                                        }
                                    }
                            }
                )
            , Test.test "application expression 2"
                (\() ->
                    "(    always (List.concat [ [ fileName ], [] ]))"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 48 } }
                            , value =
                                GrenSyntax.ExpressionParenthesized
                                    { range = { start = { row = 1, column = 6 }, end = { row = 1, column = 47 } }
                                    , value =
                                        GrenSyntax.ExpressionCall
                                            [ { range = { start = { row = 1, column = 6 }, end = { row = 1, column = 12 } }
                                              , value = GrenSyntax.ExpressionReference { qualification = [], name = "always" }
                                              }
                                            , { range = { start = { row = 1, column = 13 }, end = { row = 1, column = 47 } }
                                              , value =
                                                    GrenSyntax.ExpressionParenthesized
                                                        { range = { start = { row = 1, column = 14 }, end = { row = 1, column = 46 } }
                                                        , value =
                                                            GrenSyntax.ExpressionCall
                                                                [ { range = { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } }
                                                                  , value = GrenSyntax.ExpressionReference { qualification = [ "List" ], name = "concat" }
                                                                  }
                                                                , { range = { start = { row = 1, column = 26 }, end = { row = 1, column = 46 } }
                                                                  , value =
                                                                        GrenSyntax.ExpressionArray
                                                                            [ { range = { start = { row = 1, column = 28 }, end = { row = 1, column = 40 } }
                                                                              , value =
                                                                                    GrenSyntax.ExpressionArray
                                                                                        [ { range = { start = { row = 1, column = 30 }, end = { row = 1, column = 38 } }
                                                                                          , value = GrenSyntax.ExpressionReference { qualification = [], name = "fileName" }
                                                                                          }
                                                                                        ]
                                                                              }
                                                                            , { range = { start = { row = 1, column = 42 }, end = { row = 1, column = 44 } }
                                                                              , value = GrenSyntax.ExpressionArray []
                                                                              }
                                                                            ]
                                                                  }
                                                                ]
                                                        }
                                              }
                                            ]
                                    }
                            }
                )
            , Test.test "expressionNotApplication simple"
                (\() ->
                    "foo"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "foo" }
                            }
                )
            , Test.test "unit application"
                (\() ->
                    "Task.succeed ()"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 16 } }
                            , value =
                                GrenSyntax.ExpressionCall
                                    [ { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                                      , value = GrenSyntax.ExpressionReference { qualification = [ "Task" ], name = "succeed" }
                                      }
                                    , { range = { start = { row = 1, column = 14 }, end = { row = 1, column = 16 } }
                                      , value = GrenSyntax.ExpressionUnit
                                      }
                                    ]
                            }
                )
            , Test.test "Function call"
                (\() ->
                    "foo bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                            , value =
                                GrenSyntax.ExpressionCall
                                    [ { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                      , value = GrenSyntax.ExpressionReference { qualification = [], name = "foo" }
                                      }
                                    , { range = { start = { row = 1, column = 5 }, end = { row = 1, column = 8 } }
                                      , value = GrenSyntax.ExpressionReference { qualification = [], name = "bar" }
                                      }
                                    ]
                            }
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
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 26 } }
                            , value =
                                GrenSyntax.ExpressionIfThenElse
                                    { condition =
                                        { range = { start = { row = 1, column = 4 }, end = { row = 1, column = 8 } }
                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "True" }
                                        }
                                    , onTrue =
                                        { range = { start = { row = 1, column = 14 }, end = { row = 1, column = 17 } }
                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "foo" }
                                        }
                                    , onFalse =
                                        { range = { start = { row = 1, column = 23 }, end = { row = 1, column = 26 } }
                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "bar" }
                                        }
                                    }
                            }
                )
            , Test.test "if-then-else with -> instead of then"
                (\() ->
                    "if True ->   foo else bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 26 } }
                            , value =
                                GrenSyntax.ExpressionIfThenElse
                                    { condition =
                                        { range = { start = { row = 1, column = 4 }, end = { row = 1, column = 8 } }
                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "True" }
                                        }
                                    , onTrue =
                                        { range = { start = { row = 1, column = 14 }, end = { row = 1, column = 17 } }
                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "foo" }
                                        }
                                    , onFalse =
                                        { range = { start = { row = 1, column = 23 }, end = { row = 1, column = 26 } }
                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "bar" }
                                        }
                                    }
                            }
                )
            , Test.test "nestedIfExpression"
                (\() ->
                    "if True then if False then foo else baz else bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 49 } }
                            , value =
                                GrenSyntax.ExpressionIfThenElse
                                    { condition =
                                        { range = { start = { row = 1, column = 4 }, end = { row = 1, column = 8 } }
                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "True" }
                                        }
                                    , onTrue =
                                        { range = { start = { row = 1, column = 14 }, end = { row = 1, column = 40 } }
                                        , value =
                                            GrenSyntax.ExpressionIfThenElse
                                                { condition =
                                                    { range = { start = { row = 1, column = 17 }, end = { row = 1, column = 22 } }
                                                    , value = GrenSyntax.ExpressionReference { qualification = [], name = "False" }
                                                    }
                                                , onTrue =
                                                    { range = { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } }
                                                    , value = GrenSyntax.ExpressionReference { qualification = [], name = "foo" }
                                                    }
                                                , onFalse =
                                                    { range = { start = { row = 1, column = 37 }, end = { row = 1, column = 40 } }
                                                    , value = GrenSyntax.ExpressionReference { qualification = [], name = "baz" }
                                                    }
                                                }
                                        }
                                    , onFalse =
                                        { range = { start = { row = 1, column = 46 }, end = { row = 1, column = 49 } }
                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "bar" }
                                        }
                                    }
                            }
                )
            , Test.test "recordExpression"
                (\() ->
                    "{ model = 0, view = view, update = update }"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 44 } }
                            , value =
                                GrenSyntax.ExpressionRecord
                                    [ { range = { start = { row = 1, column = 3 }, end = { row = 1, column = 12 } }
                                      , value =
                                            { name =
                                                { range = { start = { row = 1, column = 3 }, end = { row = 1, column = 8 } }, value = "model" }
                                            , value =
                                                { range = { start = { row = 1, column = 11 }, end = { row = 1, column = 12 } }
                                                , value = GrenSyntax.ExpressionInteger 0
                                                }
                                            }
                                      }
                                    , { range = { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } }
                                      , value =
                                            { name =
                                                { range = { start = { row = 1, column = 14 }, end = { row = 1, column = 18 } }, value = "view" }
                                            , value =
                                                { range = { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } }
                                                , value = GrenSyntax.ExpressionReference { qualification = [], name = "view" }
                                                }
                                            }
                                      }
                                    , { range = { start = { row = 1, column = 27 }, end = { row = 1, column = 43 } }
                                      , value =
                                            { name =
                                                { range = { start = { row = 1, column = 27 }, end = { row = 1, column = 33 } }, value = "update" }
                                            , value =
                                                { range = { start = { row = 1, column = 36 }, end = { row = 1, column = 42 } }
                                                , value = GrenSyntax.ExpressionReference { qualification = [], name = "update" }
                                                }
                                            }
                                      }
                                    ]
                            }
                )
            , Test.test "recordExpression with comment"
                (\() ->
                    "{ foo = 1 -- bar\n , baz = 2 }"
                        |> expectSyntaxWithComments GrenParserLenient.expression
                            { syntax =
                                { range = { start = { row = 1, column = 1 }, end = { row = 2, column = 13 } }
                                , value =
                                    GrenSyntax.ExpressionRecord
                                        [ { range = { start = { row = 1, column = 3 }, end = { row = 1, column = 10 } }
                                          , value =
                                                { name =
                                                    { range = { start = { row = 1, column = 3 }, end = { row = 1, column = 6 } }, value = "foo" }
                                                , value =
                                                    { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } }
                                                    , value = GrenSyntax.ExpressionInteger 1
                                                    }
                                                }
                                          }
                                        , { range = { start = { row = 2, column = 4 }, end = { row = 2, column = 12 } }
                                          , value =
                                                { name =
                                                    { range = { start = { row = 2, column = 4 }, end = { row = 2, column = 7 } }, value = "baz" }
                                                , value =
                                                    { range = { start = { row = 2, column = 10 }, end = { row = 2, column = 11 } }
                                                    , value = GrenSyntax.ExpressionInteger 2
                                                    }
                                                }
                                          }
                                        ]
                                }
                            , comments = [ { range = { start = { row = 1, column = 11 }, end = { row = 1, column = 17 } }, value = "-- bar" } ]
                            }
                )
            , Test.test "listExpression"
                (\() ->
                    "[ class \"a\", text \"Foo\"]"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                            , value =
                                GrenSyntax.ExpressionArray
                                    [ { range = { start = { row = 1, column = 3 }, end = { row = 1, column = 12 } }
                                      , value =
                                            GrenSyntax.ExpressionCall
                                                [ { range = { start = { row = 1, column = 3 }, end = { row = 1, column = 8 } }
                                                  , value = GrenSyntax.ExpressionReference { qualification = [], name = "class" }
                                                  }
                                                , { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } }
                                                  , value = GrenSyntax.ExpressionString { quotingStyle = GrenSyntax.StringSingleQuoted, content = "a" }
                                                  }
                                                ]
                                      }
                                    , { range = { start = { row = 1, column = 14 }, end = { row = 1, column = 24 } }
                                      , value =
                                            GrenSyntax.ExpressionCall
                                                [ { range = { start = { row = 1, column = 14 }, end = { row = 1, column = 18 } }
                                                  , value = GrenSyntax.ExpressionReference { qualification = [], name = "text" }
                                                  }
                                                , { range = { start = { row = 1, column = 19 }, end = { row = 1, column = 24 } }
                                                  , value =
                                                        GrenSyntax.ExpressionString { quotingStyle = GrenSyntax.StringSingleQuoted, content = "Foo" }
                                                  }
                                                ]
                                      }
                                    ]
                            }
                )
            , Test.test "list with extra comma between elements"
                (\() ->
                    "[ class \"a\",,text \"Foo\"]"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                            , value =
                                GrenSyntax.ExpressionArray
                                    [ { range = { start = { row = 1, column = 3 }, end = { row = 1, column = 12 } }
                                      , value =
                                            GrenSyntax.ExpressionCall
                                                [ { range = { start = { row = 1, column = 3 }, end = { row = 1, column = 8 } }
                                                  , value = GrenSyntax.ExpressionReference { qualification = [], name = "class" }
                                                  }
                                                , { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } }
                                                  , value = GrenSyntax.ExpressionString { quotingStyle = GrenSyntax.StringSingleQuoted, content = "a" }
                                                  }
                                                ]
                                      }
                                    , { range = { start = { row = 1, column = 14 }, end = { row = 1, column = 24 } }
                                      , value =
                                            GrenSyntax.ExpressionCall
                                                [ { range = { start = { row = 1, column = 14 }, end = { row = 1, column = 18 } }
                                                  , value = GrenSyntax.ExpressionReference { qualification = [], name = "text" }
                                                  }
                                                , { range = { start = { row = 1, column = 19 }, end = { row = 1, column = 24 } }
                                                  , value =
                                                        GrenSyntax.ExpressionString { quotingStyle = GrenSyntax.StringSingleQuoted, content = "Foo" }
                                                  }
                                                ]
                                      }
                                    ]
                            }
                )
            , Test.test "listExpression singleton with comment"
                (\() ->
                    "[ 1 {- Foo-} ]"
                        |> expectSyntaxWithComments GrenParserLenient.expression
                            { syntax =
                                { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                                , value =
                                    GrenSyntax.ExpressionArray
                                        [ { range = { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } }
                                          , value = GrenSyntax.ExpressionInteger 1
                                          }
                                        ]
                                }
                            , comments = [ { range = { start = { row = 1, column = 5 }, end = { row = 1, column = 13 } }, value = "{- Foo-}" } ]
                            }
                )
            , Test.test "list with extra prefix comma and comment"
                (\() ->
                    "[,1 {- Foo-} ]"
                        |> expectSyntaxWithComments GrenParserLenient.expression
                            { syntax =
                                { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                                , value =
                                    GrenSyntax.ExpressionArray
                                        [ { range = { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } }
                                          , value = GrenSyntax.ExpressionInteger 1
                                          }
                                        ]
                                }
                            , comments = [ { range = { start = { row = 1, column = 5 }, end = { row = 1, column = 13 } }, value = "{- Foo-}" } ]
                            }
                )
            , Test.test "listExpression empty with comment"
                (\() ->
                    "[{- Foo -}]"
                        |> expectSyntaxWithComments GrenParserLenient.expression
                            { syntax =
                                { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                , value = GrenSyntax.ExpressionArray []
                                }
                            , comments = [ { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 11 } }, value = "{- Foo -}" } ]
                            }
                )
            , Test.test "qualified expression"
                (\() ->
                    "Html.text"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                            , value = GrenSyntax.ExpressionReference { qualification = [ "Html" ], name = "text" }
                            }
                )
            , Test.test "qualified expression with name colliding with keyword"
                (\() ->
                    "Html.Attributes.type"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 21 } }
                            , value = GrenSyntax.ExpressionReference { qualification = [ "Html", "Attributes" ], name = "type_" }
                            }
                )
            , Test.test "record access"
                (\() ->
                    "foo.bar"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                            , value =
                                GrenSyntax.ExpressionRecordAccess
                                    { record =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "foo" }
                                        }
                                    , field =
                                        { range = { start = { row = 1, column = 5 }, end = { row = 1, column = 8 } }
                                        , value = "bar"
                                        }
                                    }
                            }
                )
            , Test.test "record access with field name colliding with keyword"
                (\() ->
                    "foo.exposing"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                            , value =
                                GrenSyntax.ExpressionRecordAccess
                                    { record =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "foo" }
                                        }
                                    , field =
                                        { range = { start = { row = 1, column = 5 }, end = { row = 1, column = 13 } }
                                        , value = "exposing_"
                                        }
                                    }
                            }
                )
            , Test.test "multiple record access operations"
                (\() ->
                    "foo.bar.baz"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                            , value =
                                GrenSyntax.ExpressionRecordAccess
                                    { record =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                                        , value =
                                            GrenSyntax.ExpressionRecordAccess
                                                { record =
                                                    { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                                    , value = GrenSyntax.ExpressionReference { qualification = [], name = "foo" }
                                                    }
                                                , field =
                                                    { range = { start = { row = 1, column = 5 }, end = { row = 1, column = 8 } }
                                                    , value = "bar"
                                                    }
                                                }
                                        }
                                    , field =
                                        { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } }
                                        , value = "baz"
                                        }
                                    }
                            }
                )
            , Test.test "multiple record access operations with module name"
                (\() ->
                    "A.B.foo.bar.baz"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 16 } }
                            , value =
                                GrenSyntax.ExpressionRecordAccess
                                    { record =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                        , value =
                                            GrenSyntax.ExpressionRecordAccess
                                                { record =
                                                    { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                                                    , value = GrenSyntax.ExpressionReference { qualification = [ "A", "B" ], name = "foo" }
                                                    }
                                                , field =
                                                    { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } }
                                                    , value = "bar"
                                                    }
                                                }
                                        }
                                    , field =
                                        { range = { start = { row = 1, column = 13 }, end = { row = 1, column = 16 } }
                                        , value = "baz"
                                        }
                                    }
                            }
                )
            , Test.test "record with name-value separator : and name-value separator = and name-value separator empty and punned fields with each of those"
                (\() ->
                    "{ a 1, b = 2, c : 3, aPunned, bPunned =, cPunned : }"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { end = { column = 53, row = 1 }, start = { column = 1, row = 1 } }
                            , value =
                                GrenSyntax.ExpressionRecord
                                    [ { range = { end = { column = 6, row = 1 }, start = { column = 3, row = 1 } }
                                      , value =
                                            { name = { range = { end = { column = 4, row = 1 }, start = { column = 3, row = 1 } }, value = "a" }
                                            , value =
                                                { range = { end = { column = 6, row = 1 }, start = { column = 5, row = 1 } }
                                                , value = GrenSyntax.ExpressionInteger 1
                                                }
                                            }
                                      }
                                    , { range = { end = { column = 13, row = 1 }, start = { column = 8, row = 1 } }
                                      , value =
                                            { name = { range = { end = { column = 9, row = 1 }, start = { column = 8, row = 1 } }, value = "b" }
                                            , value =
                                                { range = { end = { column = 13, row = 1 }, start = { column = 12, row = 1 } }
                                                , value = GrenSyntax.ExpressionInteger 2
                                                }
                                            }
                                      }
                                    , { range = { end = { column = 20, row = 1 }, start = { column = 15, row = 1 } }
                                      , value =
                                            { name =
                                                { range = { end = { column = 16, row = 1 }, start = { column = 15, row = 1 } }, value = "c" }
                                            , value =
                                                { range = { end = { column = 20, row = 1 }, start = { column = 19, row = 1 } }
                                                , value = GrenSyntax.ExpressionInteger 3
                                                }
                                            }
                                      }
                                    , { range = { end = { column = 29, row = 1 }, start = { column = 22, row = 1 } }
                                      , value =
                                            { name =
                                                { range = { end = { column = 29, row = 1 }, start = { column = 22, row = 1 } }, value = "aPunned" }
                                            , value =
                                                { range = { end = { column = 29, row = 1 }, start = { column = 29, row = 1 } }
                                                , value = GrenSyntax.ExpressionReference { qualification = [], name = "aPunned" }
                                                }
                                            }
                                      }
                                    , { range = { end = { column = 40, row = 1 }, start = { column = 31, row = 1 } }
                                      , value =
                                            { name =
                                                { range = { end = { column = 38, row = 1 }, start = { column = 31, row = 1 } }, value = "bPunned" }
                                            , value =
                                                { range = { end = { column = 38, row = 1 }, start = { column = 38, row = 1 } }
                                                , value = GrenSyntax.ExpressionReference { qualification = [], name = "bPunned" }
                                                }
                                            }
                                      }
                                    , { range = { end = { column = 52, row = 1 }, start = { column = 42, row = 1 } }
                                      , value =
                                            { name =
                                                { range = { end = { column = 49, row = 1 }, start = { column = 42, row = 1 } }, value = "cPunned" }
                                            , value =
                                                { range = { end = { column = 49, row = 1 }, start = { column = 49, row = 1 } }
                                                , value = GrenSyntax.ExpressionReference { qualification = [], name = "cPunned" }
                                                }
                                            }
                                      }
                                    ]
                            }
                )
            , Test.test "record with prefix comma"
                (\() ->
                    "{ , a = 1, b = 2 }"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { end = { column = 19, row = 1 }, start = { column = 1, row = 1 } }
                            , value =
                                GrenSyntax.ExpressionRecord
                                    [ { range = { end = { column = 10, row = 1 }, start = { column = 5, row = 1 } }
                                      , value =
                                            { name = { range = { end = { column = 6, row = 1 }, start = { column = 5, row = 1 } }, value = "a" }
                                            , value =
                                                { range = { end = { column = 10, row = 1 }, start = { column = 9, row = 1 } }
                                                , value = GrenSyntax.ExpressionInteger 1
                                                }
                                            }
                                      }
                                    , { range = { end = { column = 18, row = 1 }, start = { column = 12, row = 1 } }
                                      , value =
                                            { name =
                                                { range = { end = { column = 13, row = 1 }, start = { column = 12, row = 1 } }, value = "b" }
                                            , value =
                                                { range = { end = { column = 17, row = 1 }, start = { column = 16, row = 1 } }
                                                , value = GrenSyntax.ExpressionInteger 2
                                                }
                                            }
                                      }
                                    ]
                            }
                )
            , Test.test "record with extra comma between fields"
                (\() ->
                    "{   a = 1,,b = 2 }"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { end = { column = 19, row = 1 }, start = { column = 1, row = 1 } }
                            , value =
                                GrenSyntax.ExpressionRecord
                                    [ { range = { end = { column = 10, row = 1 }, start = { column = 5, row = 1 } }
                                      , value =
                                            { name = { range = { end = { column = 6, row = 1 }, start = { column = 5, row = 1 } }, value = "a" }
                                            , value =
                                                { range = { end = { column = 10, row = 1 }, start = { column = 9, row = 1 } }
                                                , value = GrenSyntax.ExpressionInteger 1
                                                }
                                            }
                                      }
                                    , { range = { end = { column = 18, row = 1 }, start = { column = 12, row = 1 } }
                                      , value =
                                            { name =
                                                { range = { end = { column = 13, row = 1 }, start = { column = 12, row = 1 } }, value = "b" }
                                            , value =
                                                { range = { end = { column = 17, row = 1 }, start = { column = 16, row = 1 } }
                                                , value = GrenSyntax.ExpressionInteger 2
                                                }
                                            }
                                      }
                                    ]
                            }
                )
            , Test.test "record update"
                (\() ->
                    "{ model | count = 1, loading = True }"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 38 } }
                            , value =
                                GrenSyntax.ExpressionRecordUpdate
                                    { record =
                                        { range = { start = { row = 1, column = 3 }, end = { row = 1, column = 8 } }
                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "model" }
                                        }
                                    , fields =
                                        [ { range = { start = { row = 1, column = 11 }, end = { row = 1, column = 20 } }
                                          , value =
                                                { name =
                                                    { range = { start = { row = 1, column = 11 }, end = { row = 1, column = 16 } }, value = "count" }
                                                , value =
                                                    { range = { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } }
                                                    , value = GrenSyntax.ExpressionInteger 1
                                                    }
                                                }
                                          }
                                        , { range = { start = { row = 1, column = 22 }, end = { row = 1, column = 37 } }
                                          , value =
                                                { name =
                                                    { range = { start = { row = 1, column = 22 }, end = { row = 1, column = 29 } }, value = "loading" }
                                                , value =
                                                    { range = { start = { row = 1, column = 32 }, end = { row = 1, column = 36 } }
                                                    , value = GrenSyntax.ExpressionReference { qualification = [], name = "True" }
                                                    }
                                                }
                                          }
                                        ]
                                    }
                            }
                )
            , Test.test "record update, qualified updated reference"
                (\() ->
                    "{ Shared.model | count = 1, loading = True }"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { end = { column = 45, row = 1 }, start = { column = 1, row = 1 } }
                            , value =
                                GrenSyntax.ExpressionRecordUpdate
                                    { record =
                                        { range = { end = { column = 15, row = 1 }, start = { column = 3, row = 1 } }
                                        , value = GrenSyntax.ExpressionReference { qualification = [ "Shared" ], name = "model" }
                                        }
                                    , fields =
                                        [ { range = { end = { column = 27, row = 1 }, start = { column = 18, row = 1 } }
                                          , value =
                                                { name =
                                                    { range = { end = { column = 23, row = 1 }, start = { column = 18, row = 1 } }, value = "count" }
                                                , value =
                                                    { range = { end = { column = 27, row = 1 }, start = { column = 26, row = 1 } }
                                                    , value = GrenSyntax.ExpressionInteger 1
                                                    }
                                                }
                                          }
                                        , { range = { end = { column = 44, row = 1 }, start = { column = 29, row = 1 } }
                                          , value =
                                                { name =
                                                    { range = { end = { column = 36, row = 1 }, start = { column = 29, row = 1 } }, value = "loading" }
                                                , value =
                                                    { range = { end = { column = 43, row = 1 }, start = { column = 39, row = 1 } }
                                                    , value = GrenSyntax.ExpressionReference { qualification = [], name = "True" }
                                                    }
                                                }
                                          }
                                        ]
                                    }
                            }
                )
            , Test.test "record update with extra comma between fields"
                (\() ->
                    "{ model | count = 1,,loading = True }"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 38 } }
                            , value =
                                GrenSyntax.ExpressionRecordUpdate
                                    { record =
                                        { range = { start = { row = 1, column = 3 }, end = { row = 1, column = 8 } }
                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "model" }
                                        }
                                    , fields =
                                        [ { range = { start = { row = 1, column = 11 }, end = { row = 1, column = 20 } }
                                          , value =
                                                { name =
                                                    { range = { start = { row = 1, column = 11 }, end = { row = 1, column = 16 } }, value = "count" }
                                                , value =
                                                    { range = { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } }
                                                    , value = GrenSyntax.ExpressionInteger 1
                                                    }
                                                }
                                          }
                                        , { range = { start = { row = 1, column = 22 }, end = { row = 1, column = 37 } }
                                          , value =
                                                { name =
                                                    { range = { start = { row = 1, column = 22 }, end = { row = 1, column = 29 } }, value = "loading" }
                                                , value =
                                                    { range = { start = { row = 1, column = 32 }, end = { row = 1, column = 36 } }
                                                    , value = GrenSyntax.ExpressionReference { qualification = [], name = "True" }
                                                    }
                                                }
                                          }
                                        ]
                                    }
                            }
                )
            , Test.test "record update with name-value separator : and name-value separator empty"
                (\() ->
                    "{ model | count : 1, loading   True }"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 38 } }
                            , value =
                                GrenSyntax.ExpressionRecordUpdate
                                    { record =
                                        { range = { start = { row = 1, column = 3 }, end = { row = 1, column = 8 } }
                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "model" }
                                        }
                                    , fields =
                                        [ { range = { start = { row = 1, column = 11 }, end = { row = 1, column = 20 } }
                                          , value =
                                                { name =
                                                    { range = { start = { row = 1, column = 11 }, end = { row = 1, column = 16 } }, value = "count" }
                                                , value =
                                                    { range = { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } }
                                                    , value = GrenSyntax.ExpressionInteger 1
                                                    }
                                                }
                                          }
                                        , { range = { start = { row = 1, column = 22 }, end = { row = 1, column = 37 } }
                                          , value =
                                                { name =
                                                    { range = { start = { row = 1, column = 22 }, end = { row = 1, column = 29 } }, value = "loading" }
                                                , value =
                                                    { range = { start = { row = 1, column = 32 }, end = { row = 1, column = 36 } }
                                                    , value = GrenSyntax.ExpressionReference { qualification = [], name = "True" }
                                                    }
                                                }
                                          }
                                        ]
                                    }
                            }
                )
            , Test.test "record update no spacing"
                (\() ->
                    "{model| count = 1, loading = True }"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 36 } }
                            , value =
                                GrenSyntax.ExpressionRecordUpdate
                                    { record =
                                        { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } }
                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "model" }
                                        }
                                    , fields =
                                        [ { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 18 } }
                                          , value =
                                                { name =
                                                    { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 14 } }, value = "count" }
                                                , value =
                                                    { range = { start = { row = 1, column = 17 }, end = { row = 1, column = 18 } }
                                                    , value = GrenSyntax.ExpressionInteger 1
                                                    }
                                                }
                                          }
                                        , { range = { start = { row = 1, column = 20 }, end = { row = 1, column = 35 } }
                                          , value =
                                                { name =
                                                    { range = { start = { row = 1, column = 20 }, end = { row = 1, column = 27 } }, value = "loading" }
                                                , value =
                                                    { range = { start = { row = 1, column = 30 }, end = { row = 1, column = 34 } }
                                                    , value = GrenSyntax.ExpressionReference { qualification = [], name = "True" }
                                                    }
                                                }
                                          }
                                        ]
                                    }
                            }
                )
            , Test.test "record access as function"
                (\() ->
                    "List.map .name people"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 22 } }
                            , value =
                                GrenSyntax.ExpressionCall
                                    [ { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 9 } }
                                      , value = GrenSyntax.ExpressionReference { qualification = [ "List" ], name = "map" }
                                      }
                                    , { range = { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } }
                                      , value = GrenSyntax.ExpressionRecordAccessFunction ".name"
                                      }
                                    , { range = { start = { row = 1, column = 16 }, end = { row = 1, column = 22 } }
                                      , value = GrenSyntax.ExpressionReference { qualification = [], name = "people" }
                                      }
                                    ]
                            }
                )
            , Test.test "record access direct"
                (\() ->
                    "(.spaceEvenly Internal.Style.classes)"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 38 } }
                            , value =
                                GrenSyntax.ExpressionParenthesized
                                    { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 37 } }
                                    , value =
                                        GrenSyntax.ExpressionCall
                                            [ { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 14 } }
                                              , value = GrenSyntax.ExpressionRecordAccessFunction ".spaceEvenly"
                                              }
                                            , { range = { start = { row = 1, column = 15 }, end = { row = 1, column = 37 } }
                                              , value = GrenSyntax.ExpressionReference { qualification = [ "Internal", "Style" ], name = "classes" }
                                              }
                                            ]
                                    }
                            }
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
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                            , value =
                                GrenSyntax.ExpressionCall
                                    [ { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }
                                      , value = GrenSyntax.ExpressionOperatorFunction "::"
                                      }
                                    , { range = { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } }
                                      , value = GrenSyntax.ExpressionReference { qualification = [], name = "x" }
                                      }
                                    ]
                            }
                )
            , Test.test "subtraction without spaces"
                (\() ->
                    "2-1"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                            , value =
                                GrenSyntax.ExpressionInfixOperation
                                    { operator = "-"
                                    , left =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } }
                                        , value = GrenSyntax.ExpressionInteger 2
                                        }
                                    , right =
                                        { range = { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } }
                                        , value = GrenSyntax.ExpressionInteger 1
                                        }
                                    }
                            }
                )
            , Test.test "negated expression after comma"
                (\() ->
                    "a = [0,-x]"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                            , value =
                                GrenSyntax.ValueOrFunctionDeclaration
                                    { signature = Nothing
                                    , documentation = Nothing
                                    , declaration =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                                        , value =
                                            { name = { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } }, value = "a" }
                                            , parameters = []
                                            , expression =
                                                { range = { start = { row = 1, column = 5 }, end = { row = 1, column = 11 } }
                                                , value =
                                                    GrenSyntax.ExpressionArray
                                                        [ { range = { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } }
                                                          , value = GrenSyntax.ExpressionInteger 0
                                                          }
                                                        , { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 10 } }
                                                          , value =
                                                                GrenSyntax.ExpressionNegation
                                                                    { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } }
                                                                    , value = GrenSyntax.ExpressionReference { qualification = [], name = "x" }
                                                                    }
                                                          }
                                                        ]
                                                }
                                            }
                                        }
                                    }
                            }
                )
            , Test.test "negated expression after = in record"
                (\() ->
                    "{a=-x}"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                            , value =
                                GrenSyntax.ExpressionRecord
                                    [ { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 6 } }
                                      , value =
                                            { name = { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } }, value = "a" }
                                            , value =
                                                { range = { start = { row = 1, column = 4 }, end = { row = 1, column = 6 } }
                                                , value =
                                                    GrenSyntax.ExpressionNegation
                                                        { range = { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } }
                                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "x" }
                                                        }
                                                }
                                            }
                                      }
                                    ]
                            }
                )
            , Test.test "negated expression after list"
                (\() ->
                    "List.sum [a,b]-x"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 17 } }
                            , value =
                                GrenSyntax.ExpressionInfixOperation
                                    { operator = "-"
                                    , left =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                                        , value =
                                            GrenSyntax.ExpressionCall
                                                [ { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 9 } }
                                                  , value = GrenSyntax.ExpressionReference { qualification = [ "List" ], name = "sum" }
                                                  }
                                                , { range = { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } }
                                                  , value =
                                                        GrenSyntax.ExpressionArray
                                                            [ { range = { start = { row = 1, column = 11 }, end = { row = 1, column = 12 } }
                                                              , value = GrenSyntax.ExpressionReference { qualification = [], name = "a" }
                                                              }
                                                            , { range = { start = { row = 1, column = 13 }, end = { row = 1, column = 14 } }
                                                              , value = GrenSyntax.ExpressionReference { qualification = [], name = "b" }
                                                              }
                                                            ]
                                                  }
                                                ]
                                        }
                                    , right =
                                        { range = { start = { row = 1, column = 16 }, end = { row = 1, column = 17 } }
                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "x" }
                                        }
                                    }
                            }
                )
            , Test.test "negated expression after = in let declaration"
                (\() ->
                    "let a=-x in a"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 14 } }
                            , value =
                                GrenSyntax.ExpressionLetIn
                                    { declarations =
                                        [ { range = { start = { row = 1, column = 5 }, end = { row = 1, column = 9 } }
                                          , value =
                                                GrenSyntax.LetFunction
                                                    { documentation = Nothing
                                                    , signature = Nothing
                                                    , declaration =
                                                        { range = { start = { row = 1, column = 5 }, end = { row = 1, column = 9 } }
                                                        , value =
                                                            { name = { range = { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } }, value = "a" }
                                                            , parameters = []
                                                            , expression =
                                                                { range = { start = { row = 1, column = 7 }, end = { row = 1, column = 9 } }
                                                                , value =
                                                                    GrenSyntax.ExpressionNegation
                                                                        { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } }
                                                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "x" }
                                                                        }
                                                                }
                                                            }
                                                        }
                                                    }
                                          }
                                        ]
                                    , result =
                                        { range = { start = { row = 1, column = 13 }, end = { row = 1, column = 14 } }
                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "a" }
                                        }
                                    }
                            }
                )
            , Test.test "negated expression after -> in case branch"
                (\() ->
                    "case 1 of\n    _->-a"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 2, column = 10 } }
                            , value =
                                GrenSyntax.ExpressionCaseOf
                                    { expression =
                                        { range = { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } }
                                        , value = GrenSyntax.ExpressionInteger 1
                                        }
                                    , cases =
                                        [ { pattern =
                                                { range = { start = { row = 2, column = 5 }, end = { row = 2, column = 6 } }
                                                , value = GrenSyntax.PatternIgnored Nothing
                                                }
                                          , result =
                                                { range = { start = { row = 2, column = 8 }, end = { row = 2, column = 10 } }
                                                , value =
                                                    GrenSyntax.ExpressionNegation
                                                        { range = { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } }
                                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "a" }
                                                        }
                                                }
                                          }
                                        ]
                                    }
                            }
                )
            , Test.test "negated expression after `in` in let body"
                (\() ->
                    "let a=-x in-a"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { end = { column = 14, row = 1 }, start = { column = 1, row = 1 } }
                            , value =
                                GrenSyntax.ExpressionLetIn
                                    { declarations =
                                        [ { range = { end = { column = 9, row = 1 }, start = { column = 5, row = 1 } }
                                          , value =
                                                GrenSyntax.LetFunction
                                                    { declaration =
                                                        { range = { end = { column = 9, row = 1 }, start = { column = 5, row = 1 } }
                                                        , value =
                                                            { parameters = []
                                                            , expression =
                                                                { range = { end = { column = 9, row = 1 }, start = { column = 7, row = 1 } }
                                                                , value =
                                                                    GrenSyntax.ExpressionNegation
                                                                        { range = { end = { column = 9, row = 1 }, start = { column = 8, row = 1 } }
                                                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "x" }
                                                                        }
                                                                }
                                                            , name = { range = { end = { column = 6, row = 1 }, start = { column = 5, row = 1 } }, value = "a" }
                                                            }
                                                        }
                                                    , documentation = Nothing
                                                    , signature = Nothing
                                                    }
                                          }
                                        ]
                                    , result =
                                        { range = { end = { column = 14, row = 1 }, start = { column = 12, row = 1 } }
                                        , value =
                                            GrenSyntax.ExpressionNegation
                                                { range = { end = { column = 14, row = 1 }, start = { column = 13, row = 1 } }
                                                , value = GrenSyntax.ExpressionReference { qualification = [], name = "a" }
                                                }
                                        }
                                    }
                            }
                )
            , Test.test "negated expression for value"
                (\() ->
                    "a = -x"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                            , value =
                                GrenSyntax.ValueOrFunctionDeclaration
                                    { signature = Nothing
                                    , documentation = Nothing
                                    , declaration =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                                        , value =
                                            { name = { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } }, value = "a" }
                                            , parameters = []
                                            , expression =
                                                { range = { start = { row = 1, column = 5 }, end = { row = 1, column = 7 } }
                                                , value =
                                                    GrenSyntax.ExpressionNegation
                                                        { range = { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } }
                                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "x" }
                                                        }
                                                }
                                            }
                                        }
                                    }
                            }
                )
            , Test.test "function declaration with negation sign after ="
                (\() ->
                    "foo=-1"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                            , value =
                                GrenSyntax.ValueOrFunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                                        , value =
                                            { name =
                                                { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }, value = "foo" }
                                            , parameters = []
                                            , expression =
                                                { range = { start = { row = 1, column = 5 }, end = { row = 1, column = 7 } }
                                                , value =
                                                    GrenSyntax.ExpressionNegation
                                                        { range = { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } }
                                                        , value = GrenSyntax.ExpressionInteger 1
                                                        }
                                                }
                                            }
                                        }
                                    }
                            }
                )
            , Test.test "negated expression in application"
                (\() ->
                    "toFloat -5"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                            , value =
                                GrenSyntax.ExpressionCall
                                    [ { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                                      , value = GrenSyntax.ExpressionReference { qualification = [], name = "toFloat" }
                                      }
                                    , { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 11 } }
                                      , value =
                                            GrenSyntax.ExpressionNegation
                                                { range = { start = { row = 1, column = 10 }, end = { row = 1, column = 11 } }
                                                , value = GrenSyntax.ExpressionInteger 5
                                                }
                                      }
                                    ]
                            }
                )
            , Test.test "negated expression for parenthesized"
                (\() ->
                    "a = -(x - y)"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                            , value =
                                GrenSyntax.ValueOrFunctionDeclaration
                                    { signature = Nothing
                                    , documentation = Nothing
                                    , declaration =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                                        , value =
                                            { name = { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } }, value = "a" }
                                            , parameters = []
                                            , expression =
                                                { range = { start = { row = 1, column = 5 }, end = { row = 1, column = 13 } }
                                                , value =
                                                    GrenSyntax.ExpressionNegation
                                                        { range = { start = { row = 1, column = 6 }, end = { row = 1, column = 13 } }
                                                        , value =
                                                            GrenSyntax.ExpressionParenthesized
                                                                { range = { start = { row = 1, column = 7 }, end = { row = 1, column = 12 } }
                                                                , value =
                                                                    GrenSyntax.ExpressionInfixOperation
                                                                        { operator = "-"
                                                                        , left =
                                                                            { range = { start = { row = 1, column = 7 }, end = { row = 1, column = 8 } }
                                                                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "x" }
                                                                            }
                                                                        , right =
                                                                            { range = { start = { row = 1, column = 11 }, end = { row = 1, column = 12 } }
                                                                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "y" }
                                                                            }
                                                                        }
                                                                }
                                                        }
                                                }
                                            }
                                        }
                                    }
                            }
                )
            , Test.test "negated expression with other operations"
                (\() ->
                    "a = -1 + -10 * -100^2 == -100001"
                        |> expectSyntaxWithoutComments GrenParserLenient.declaration
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 33 } }
                            , value =
                                GrenSyntax.ValueOrFunctionDeclaration
                                    { signature = Nothing
                                    , documentation = Nothing
                                    , declaration =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 33 } }
                                        , value =
                                            { name = { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } }, value = "a" }
                                            , parameters = []
                                            , expression =
                                                { range = { start = { row = 1, column = 5 }, end = { row = 1, column = 33 } }
                                                , value =
                                                    GrenSyntax.ExpressionInfixOperation
                                                        { operator = "=="
                                                        , left =
                                                            { range = { start = { row = 1, column = 5 }, end = { row = 1, column = 22 } }
                                                            , value =
                                                                GrenSyntax.ExpressionInfixOperation
                                                                    { operator = "+"
                                                                    , left =
                                                                        { range = { start = { row = 1, column = 5 }, end = { row = 1, column = 7 } }
                                                                        , value =
                                                                            GrenSyntax.ExpressionNegation
                                                                                { range = { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } }
                                                                                , value = GrenSyntax.ExpressionInteger 1
                                                                                }
                                                                        }
                                                                    , right =
                                                                        { range = { start = { row = 1, column = 10 }, end = { row = 1, column = 22 } }
                                                                        , value =
                                                                            GrenSyntax.ExpressionInfixOperation
                                                                                { operator = "*"
                                                                                , left =
                                                                                    { range = { start = { row = 1, column = 10 }, end = { row = 1, column = 13 } }
                                                                                    , value =
                                                                                        GrenSyntax.ExpressionNegation
                                                                                            { range = { start = { row = 1, column = 11 }, end = { row = 1, column = 13 } }
                                                                                            , value = GrenSyntax.ExpressionInteger 10
                                                                                            }
                                                                                    }
                                                                                , right =
                                                                                    { range = { start = { row = 1, column = 16 }, end = { row = 1, column = 22 } }
                                                                                    , value =
                                                                                        GrenSyntax.ExpressionInfixOperation
                                                                                            { operator = "^"
                                                                                            , left =
                                                                                                { range = { start = { row = 1, column = 16 }, end = { row = 1, column = 20 } }
                                                                                                , value =
                                                                                                    GrenSyntax.ExpressionNegation
                                                                                                        { range = { start = { row = 1, column = 17 }, end = { row = 1, column = 20 } }
                                                                                                        , value = GrenSyntax.ExpressionInteger 100
                                                                                                        }
                                                                                                }
                                                                                            , right =
                                                                                                { range = { start = { row = 1, column = 21 }, end = { row = 1, column = 22 } }
                                                                                                , value = GrenSyntax.ExpressionInteger 2
                                                                                                }
                                                                                            }
                                                                                    }
                                                                                }
                                                                        }
                                                                    }
                                                            }
                                                        , right =
                                                            { range = { start = { row = 1, column = 26 }, end = { row = 1, column = 33 } }
                                                            , value =
                                                                GrenSyntax.ExpressionNegation
                                                                    { range = { start = { row = 1, column = 27 }, end = { row = 1, column = 33 } }
                                                                    , value = GrenSyntax.ExpressionInteger 100001
                                                                    }
                                                            }
                                                        }
                                                }
                                            }
                                        }
                                    }
                            }
                )
            , Test.test "plus and minus in the same expression"
                (\() ->
                    "1 + 2 - 3"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                            , value =
                                GrenSyntax.ExpressionInfixOperation
                                    { operator = "-"
                                    , left =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                                        , value =
                                            GrenSyntax.ExpressionInfixOperation
                                                { operator = "+"
                                                , left =
                                                    { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } }
                                                    , value = GrenSyntax.ExpressionInteger 1
                                                    }
                                                , right =
                                                    { range = { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } }
                                                    , value = GrenSyntax.ExpressionInteger 2
                                                    }
                                                }
                                        }
                                    , right =
                                        { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } }
                                        , value = GrenSyntax.ExpressionInteger 3
                                        }
                                    }
                            }
                )
            , Test.test "pipe operation"
                (\() ->
                    "a |> b"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                            , value =
                                GrenSyntax.ExpressionInfixOperation
                                    { operator = "|>"
                                    , left =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } }
                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "a" }
                                        }
                                    , right =
                                        { range = { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } }
                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "b" }
                                        }
                                    }
                            }
                )
            , Test.test "function with higher order"
                (\() ->
                    "chompWhile (\\c -> c == ' ' || c == '\\n' || c == '\\r')"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 54 } }
                            , value =
                                GrenSyntax.ExpressionCall
                                    [ { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                                      , value = GrenSyntax.ExpressionReference { qualification = [], name = "chompWhile" }
                                      }
                                    , { range = { start = { row = 1, column = 12 }, end = { row = 1, column = 54 } }
                                      , value =
                                            GrenSyntax.ExpressionParenthesized
                                                { range = { start = { row = 1, column = 13 }, end = { row = 1, column = 53 } }
                                                , value =
                                                    GrenSyntax.ExpressionLambda
                                                        { parameters =
                                                            [ { range = { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } }
                                                              , value = GrenSyntax.PatternVariable "c"
                                                              }
                                                            ]
                                                        , result =
                                                            { range = { start = { row = 1, column = 19 }, end = { row = 1, column = 53 } }
                                                            , value =
                                                                GrenSyntax.ExpressionInfixOperation
                                                                    { operator = "||"
                                                                    , left =
                                                                        { range = { start = { row = 1, column = 19 }, end = { row = 1, column = 27 } }
                                                                        , value =
                                                                            GrenSyntax.ExpressionInfixOperation
                                                                                { operator = "=="
                                                                                , left =
                                                                                    { range = { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } }
                                                                                    , value = GrenSyntax.ExpressionReference { qualification = [], name = "c" }
                                                                                    }
                                                                                , right =
                                                                                    { range = { start = { row = 1, column = 24 }, end = { row = 1, column = 27 } }
                                                                                    , value = GrenSyntax.ExpressionChar ' '
                                                                                    }
                                                                                }
                                                                        }
                                                                    , right =
                                                                        { range = { start = { row = 1, column = 31 }, end = { row = 1, column = 53 } }
                                                                        , value =
                                                                            GrenSyntax.ExpressionInfixOperation
                                                                                { operator = "||"
                                                                                , left =
                                                                                    { range = { start = { row = 1, column = 31 }, end = { row = 1, column = 40 } }
                                                                                    , value =
                                                                                        GrenSyntax.ExpressionInfixOperation
                                                                                            { operator = "=="
                                                                                            , left =
                                                                                                { range = { start = { row = 1, column = 31 }, end = { row = 1, column = 32 } }
                                                                                                , value = GrenSyntax.ExpressionReference { qualification = [], name = "c" }
                                                                                                }
                                                                                            , right =
                                                                                                { range = { start = { row = 1, column = 36 }, end = { row = 1, column = 40 } }
                                                                                                , value = GrenSyntax.ExpressionChar '\n'
                                                                                                }
                                                                                            }
                                                                                    }
                                                                                , right =
                                                                                    { range = { start = { row = 1, column = 44 }, end = { row = 1, column = 53 } }
                                                                                    , value =
                                                                                        GrenSyntax.ExpressionInfixOperation
                                                                                            { operator = "=="
                                                                                            , left =
                                                                                                { range = { start = { row = 1, column = 44 }, end = { row = 1, column = 45 } }
                                                                                                , value = GrenSyntax.ExpressionReference { qualification = [], name = "c" }
                                                                                                }
                                                                                            , right =
                                                                                                { range = { start = { row = 1, column = 49 }, end = { row = 1, column = 53 } }
                                                                                                , value = GrenSyntax.ExpressionChar '\u{000D}'
                                                                                                }
                                                                                            }
                                                                                    }
                                                                                }
                                                                        }
                                                                    }
                                                            }
                                                        }
                                                }
                                      }
                                    ]
                            }
                )
            , Test.test "application should be lower-priority than field access"
                (\() ->
                    "foo { d | b = f x y }.b"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 24 } }
                            , value =
                                GrenSyntax.ExpressionCall
                                    [ { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                      , value = GrenSyntax.ExpressionReference { qualification = [], name = "foo" }
                                      }
                                    , { range = { start = { row = 1, column = 5 }, end = { row = 1, column = 24 } }
                                      , value =
                                            GrenSyntax.ExpressionRecordAccess
                                                { record =
                                                    { range = { start = { row = 1, column = 5 }, end = { row = 1, column = 22 } }
                                                    , value =
                                                        GrenSyntax.ExpressionRecordUpdate
                                                            { record =
                                                                { range = { start = { row = 1, column = 7 }, end = { row = 1, column = 8 } }
                                                                , value = GrenSyntax.ExpressionReference { qualification = [], name = "d" }
                                                                }
                                                            , fields =
                                                                [ { range = { start = { row = 1, column = 11 }, end = { row = 1, column = 21 } }
                                                                  , value =
                                                                        { name =
                                                                            { range = { start = { row = 1, column = 11 }, end = { row = 1, column = 12 } }, value = "b" }
                                                                        , value =
                                                                            { range = { start = { row = 1, column = 15 }, end = { row = 1, column = 20 } }
                                                                            , value =
                                                                                GrenSyntax.ExpressionCall
                                                                                    [ { range = { start = { row = 1, column = 15 }, end = { row = 1, column = 16 } }
                                                                                      , value = GrenSyntax.ExpressionReference { qualification = [], name = "f" }
                                                                                      }
                                                                                    , { range = { start = { row = 1, column = 17 }, end = { row = 1, column = 18 } }
                                                                                      , value = GrenSyntax.ExpressionReference { qualification = [], name = "x" }
                                                                                      }
                                                                                    , { range = { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } }
                                                                                      , value = GrenSyntax.ExpressionReference { qualification = [], name = "y" }
                                                                                      }
                                                                                    ]
                                                                            }
                                                                        }
                                                                  }
                                                                ]
                                                            }
                                                    }
                                                , field =
                                                    { range = { start = { row = 1, column = 23 }, end = { row = 1, column = 24 } }
                                                    , value = "b"
                                                    }
                                                }
                                      }
                                    ]
                            }
                )
            , Test.test "should not consider a negative number parameter as the start of a new application"
                (\() ->
                    "Random.list -1 generator"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                            , value =
                                GrenSyntax.ExpressionCall
                                    [ { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                      , value = GrenSyntax.ExpressionReference { qualification = [ "Random" ], name = "list" }
                                      }
                                    , { range = { start = { row = 1, column = 13 }, end = { row = 1, column = 15 } }
                                      , value =
                                            GrenSyntax.ExpressionNegation
                                                { range = { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } }
                                                , value = GrenSyntax.ExpressionInteger 1
                                                }
                                      }
                                    , { range = { start = { row = 1, column = 16 }, end = { row = 1, column = 25 } }
                                      , value = GrenSyntax.ExpressionReference { qualification = [], name = "generator" }
                                      }
                                    ]
                            }
                )
            , Test.test "negation can be applied on record access"
                (\() ->
                    "1 + -{x = 10}.x"
                        |> expectSyntaxWithoutComments GrenParserLenient.expression
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 16 } }
                            , value =
                                GrenSyntax.ExpressionInfixOperation
                                    { operator = "+"
                                    , left =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } }
                                        , value = GrenSyntax.ExpressionInteger 1
                                        }
                                    , right =
                                        { range = { start = { row = 1, column = 5 }, end = { row = 1, column = 16 } }
                                        , value =
                                            GrenSyntax.ExpressionNegation
                                                { range = { start = { row = 1, column = 6 }, end = { row = 1, column = 16 } }
                                                , value =
                                                    GrenSyntax.ExpressionRecordAccess
                                                        { record =
                                                            { range = { start = { row = 1, column = 6 }, end = { row = 1, column = 14 } }
                                                            , value =
                                                                GrenSyntax.ExpressionRecord
                                                                    [ { range = { start = { row = 1, column = 7 }, end = { row = 1, column = 13 } }
                                                                      , value =
                                                                            { name = { range = { start = { row = 1, column = 7 }, end = { row = 1, column = 8 } }, value = "x" }
                                                                            , value =
                                                                                { range = { start = { row = 1, column = 11 }, end = { row = 1, column = 13 } }
                                                                                , value = GrenSyntax.ExpressionInteger 10
                                                                                }
                                                                            }
                                                                      }
                                                                    ]
                                                            }
                                                        , field =
                                                            { range = { start = { row = 1, column = 15 }, end = { row = 1, column = 16 } }
                                                            , value = "x"
                                                            }
                                                        }
                                                }
                                        }
                                    }
                            }
                )
            , Test.describe "lambda"
                [ Test.test "unit lambda"
                    (\() ->
                        "\\() -> foo"
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                                , value =
                                    GrenSyntax.ExpressionLambda
                                        { parameters =
                                            [ { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } }
                                              , value = GrenSyntax.PatternUnit
                                              }
                                            ]
                                        , result =
                                            { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }
                                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "foo" }
                                            }
                                        }
                                }
                    )
                , Test.test "record lambda"
                    (\() ->
                        "\\{foo} -> foo"
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 14 } }
                                , value =
                                    GrenSyntax.ExpressionLambda
                                        { parameters =
                                            [ { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } }
                                              , value =
                                                    GrenSyntax.PatternRecord
                                                        [ { value = Nothing
                                                          , name =
                                                                { range = { start = { row = 1, column = 3 }, end = { row = 1, column = 6 } }, value = "foo" }
                                                          }
                                                        ]
                                              }
                                            ]
                                        , result =
                                            { range = { start = { row = 1, column = 11 }, end = { row = 1, column = 14 } }
                                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "foo" }
                                            }
                                        }
                                }
                    )
                , Test.test "empty record lambda"
                    (\() ->
                        "\\{} -> foo"
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                                , value =
                                    GrenSyntax.ExpressionLambda
                                        { parameters =
                                            [ { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } }
                                              , value = GrenSyntax.PatternRecord []
                                              }
                                            ]
                                        , result =
                                            { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }
                                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "foo" }
                                            }
                                        }
                                }
                    )
                , Test.test "args lambda"
                    (\() ->
                        "\\a b -> a + b"
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 14 } }
                                , value =
                                    GrenSyntax.ExpressionLambda
                                        { parameters =
                                            [ { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } }
                                              , value = GrenSyntax.PatternVariable "a"
                                              }
                                            , { range = { start = { row = 1, column = 4 }, end = { row = 1, column = 5 } }
                                              , value = GrenSyntax.PatternVariable "b"
                                              }
                                            ]
                                        , result =
                                            { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 14 } }
                                            , value =
                                                GrenSyntax.ExpressionInfixOperation
                                                    { operator = "+"
                                                    , left =
                                                        { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } }
                                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "a" }
                                                        }
                                                    , right =
                                                        { range = { start = { row = 1, column = 13 }, end = { row = 1, column = 14 } }
                                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "b" }
                                                        }
                                                    }
                                            }
                                        }
                                }
                    )
                , Test.test "lambda with => instead of ->"
                    (\() ->
                        "\\() => foo"
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                                , value =
                                    GrenSyntax.ExpressionLambda
                                        { parameters =
                                            [ { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } }
                                              , value = GrenSyntax.PatternUnit
                                              }
                                            ]
                                        , result =
                                            { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }
                                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "foo" }
                                            }
                                        }
                                }
                    )
                , Test.test "lambda with . instead of ->"
                    (\() ->
                        "\\().   foo"
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                                , value =
                                    GrenSyntax.ExpressionLambda
                                        { parameters =
                                            [ { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } }
                                              , value = GrenSyntax.PatternUnit
                                              }
                                            ]
                                        , result =
                                            { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }
                                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "foo" }
                                            }
                                        }
                                }
                    )
                ]
            , Test.describe "let-in"
                [ Test.test "let expression with multiple declarations"
                    (\() ->
                        """let
  foo = bar

  john n = n in 1"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                { range = { start = { row = 1, column = 1 }, end = { row = 4, column = 18 } }
                                , value =
                                    GrenSyntax.ExpressionLetIn
                                        { declarations =
                                            [ { range = { start = { row = 2, column = 3 }, end = { row = 2, column = 12 } }
                                              , value =
                                                    GrenSyntax.LetFunction
                                                        { documentation = Nothing
                                                        , signature = Nothing
                                                        , declaration =
                                                            { range = { start = { row = 2, column = 3 }, end = { row = 2, column = 12 } }
                                                            , value =
                                                                { name =
                                                                    { range = { start = { row = 2, column = 3 }, end = { row = 2, column = 6 } }, value = "foo" }
                                                                , parameters = []
                                                                , expression =
                                                                    { range = { start = { row = 2, column = 9 }, end = { row = 2, column = 12 } }
                                                                    , value = GrenSyntax.ExpressionReference { qualification = [], name = "bar" }
                                                                    }
                                                                }
                                                            }
                                                        }
                                              }
                                            , { range = { start = { row = 4, column = 3 }, end = { row = 4, column = 13 } }
                                              , value =
                                                    GrenSyntax.LetFunction
                                                        { documentation = Nothing
                                                        , signature = Nothing
                                                        , declaration =
                                                            { range = { start = { row = 4, column = 3 }, end = { row = 4, column = 13 } }
                                                            , value =
                                                                { name =
                                                                    { range = { start = { row = 4, column = 3 }, end = { row = 4, column = 7 } }, value = "john" }
                                                                , parameters =
                                                                    [ { range = { start = { row = 4, column = 8 }, end = { row = 4, column = 9 } }
                                                                      , value = GrenSyntax.PatternVariable "n"
                                                                      }
                                                                    ]
                                                                , expression =
                                                                    { range = { start = { row = 4, column = 12 }, end = { row = 4, column = 13 } }
                                                                    , value = GrenSyntax.ExpressionReference { qualification = [], name = "n" }
                                                                    }
                                                                }
                                                            }
                                                        }
                                              }
                                            ]
                                        , result =
                                            { range = { start = { row = 4, column = 17 }, end = { row = 4, column = 18 } }
                                            , value = GrenSyntax.ExpressionInteger 1
                                            }
                                        }
                                }
                    )
                , Test.test "Let with `in` indented more than the body and let declarations"
                    (\() ->
                        """let
  bar = 1
            in
  bar"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                { range = { start = { row = 1, column = 1 }, end = { row = 4, column = 6 } }
                                , value =
                                    GrenSyntax.ExpressionLetIn
                                        { declarations =
                                            [ { range = { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } }
                                              , value =
                                                    GrenSyntax.LetFunction
                                                        { documentation = Nothing
                                                        , signature = Nothing
                                                        , declaration =
                                                            { range = { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } }
                                                            , value =
                                                                { name =
                                                                    { range = { start = { row = 2, column = 3 }, end = { row = 2, column = 6 } }, value = "bar" }
                                                                , parameters = []
                                                                , expression =
                                                                    { range = { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } }
                                                                    , value = GrenSyntax.ExpressionInteger 1
                                                                    }
                                                                }
                                                            }
                                                        }
                                              }
                                            ]
                                        , result =
                                            { range = { start = { row = 4, column = 3 }, end = { row = 4, column = 6 } }
                                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "bar" }
                                            }
                                        }
                                }
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
                                { range = { start = { row = 1, column = 1 }, end = { row = 4, column = 7 } }
                                , value =
                                    GrenSyntax.ExpressionLetIn
                                        { declarations =
                                            [ { range = { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } }
                                              , value =
                                                    GrenSyntax.LetFunction
                                                        { documentation = Nothing
                                                        , signature = Nothing
                                                        , declaration =
                                                            { range = { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } }
                                                            , value =
                                                                { name =
                                                                    { range = { start = { row = 2, column = 3 }, end = { row = 2, column = 6 } }, value = "bar" }
                                                                , parameters = []
                                                                , expression =
                                                                    { range = { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } }
                                                                    , value = GrenSyntax.ExpressionInteger 1
                                                                    }
                                                                }
                                                            }
                                                        }
                                              }
                                            ]
                                        , result =
                                            { range = { start = { row = 4, column = 4 }, end = { row = 4, column = 7 } }
                                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "bar" }
                                            }
                                        }
                                }
                    )
                , Test.test "Let function with type annotation"
                    (\() ->
                        """let
    bar : Int
    bar = 1
  in
  bar"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                { range = { start = { row = 1, column = 1 }, end = { row = 5, column = 6 } }
                                , value =
                                    GrenSyntax.ExpressionLetIn
                                        { declarations =
                                            [ { range = { start = { row = 2, column = 5 }, end = { row = 3, column = 12 } }
                                              , value =
                                                    GrenSyntax.LetFunction
                                                        { documentation = Nothing
                                                        , signature =
                                                            Just
                                                                { range = { start = { row = 2, column = 5 }, end = { row = 2, column = 14 } }
                                                                , value =
                                                                    { name =
                                                                        { range = { start = { row = 2, column = 5 }, end = { row = 2, column = 8 } }, value = "bar" }
                                                                    , typeAnnotation =
                                                                        { range = { start = { row = 2, column = 11 }, end = { row = 2, column = 14 } }
                                                                        , value =
                                                                            GrenSyntax.TypeAnnotationConstruct
                                                                                { reference =
                                                                                    { range = { start = { row = 2, column = 11 }, end = { row = 2, column = 14 } }
                                                                                    , value = { qualification = [], name = "Int" }
                                                                                    }
                                                                                , arguments = []
                                                                                }
                                                                        }
                                                                    }
                                                                }
                                                        , declaration =
                                                            { range = { start = { row = 3, column = 5 }, end = { row = 3, column = 12 } }
                                                            , value =
                                                                { name =
                                                                    { range = { start = { row = 3, column = 5 }, end = { row = 3, column = 8 } }, value = "bar" }
                                                                , parameters = []
                                                                , expression =
                                                                    { range = { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } }
                                                                    , value = GrenSyntax.ExpressionInteger 1
                                                                    }
                                                                }
                                                            }
                                                        }
                                              }
                                            ]
                                        , result =
                                            { range = { start = { row = 5, column = 3 }, end = { row = 5, column = 6 } }
                                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "bar" }
                                            }
                                        }
                                }
                    )
                , Test.test "Let function with type annotation (separated by a few lines)"
                    (\() ->
                        """let
    bar : Int


    bar = 1
  in
  bar"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                { range = { start = { row = 1, column = 1 }, end = { row = 7, column = 6 } }
                                , value =
                                    GrenSyntax.ExpressionLetIn
                                        { declarations =
                                            [ { range = { start = { row = 2, column = 5 }, end = { row = 5, column = 12 } }
                                              , value =
                                                    GrenSyntax.LetFunction
                                                        { documentation = Nothing
                                                        , signature =
                                                            Just
                                                                { range = { start = { row = 2, column = 5 }, end = { row = 2, column = 14 } }
                                                                , value =
                                                                    { name =
                                                                        { range = { start = { row = 2, column = 5 }, end = { row = 2, column = 8 } }, value = "bar" }
                                                                    , typeAnnotation =
                                                                        { range = { start = { row = 2, column = 11 }, end = { row = 2, column = 14 } }
                                                                        , value =
                                                                            GrenSyntax.TypeAnnotationConstruct
                                                                                { reference =
                                                                                    { range = { start = { row = 2, column = 11 }, end = { row = 2, column = 14 } }
                                                                                    , value = { qualification = [], name = "Int" }
                                                                                    }
                                                                                , arguments = []
                                                                                }
                                                                        }
                                                                    }
                                                                }
                                                        , declaration =
                                                            { range = { start = { row = 5, column = 5 }, end = { row = 5, column = 12 } }
                                                            , value =
                                                                { name =
                                                                    { range = { start = { row = 5, column = 5 }, end = { row = 5, column = 8 } }, value = "bar" }
                                                                , parameters = []
                                                                , expression =
                                                                    { range = { start = { row = 5, column = 11 }, end = { row = 5, column = 12 } }
                                                                    , value = GrenSyntax.ExpressionInteger 1
                                                                    }
                                                                }
                                                            }
                                                        }
                                              }
                                            ]
                                        , result =
                                            { range = { start = { row = 7, column = 3 }, end = { row = 7, column = 6 } }
                                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "bar" }
                                            }
                                        }
                                }
                    )
                , Test.test "Let function with type annotation, signature does not repeat the name"
                    (\() ->
                        """let
    : Int
    bar = 1
  in
  bar"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                { range = { start = { row = 1, column = 1 }, end = { row = 5, column = 6 } }
                                , value =
                                    GrenSyntax.ExpressionLetIn
                                        { declarations =
                                            [ { range = { start = { row = 2, column = 5 }, end = { row = 3, column = 12 } }
                                              , value =
                                                    GrenSyntax.LetFunction
                                                        { documentation = Nothing
                                                        , signature =
                                                            Just
                                                                { range = { start = { row = 2, column = 5 }, end = { row = 2, column = 10 } }
                                                                , value =
                                                                    { name =
                                                                        { range = { start = { row = 2, column = 5 }, end = { row = 2, column = 5 } }, value = "bar" }
                                                                    , typeAnnotation =
                                                                        { range = { start = { row = 2, column = 7 }, end = { row = 2, column = 10 } }
                                                                        , value =
                                                                            GrenSyntax.TypeAnnotationConstruct
                                                                                { reference =
                                                                                    { range = { start = { row = 2, column = 7 }, end = { row = 2, column = 10 } }
                                                                                    , value = { qualification = [], name = "Int" }
                                                                                    }
                                                                                , arguments = []
                                                                                }
                                                                        }
                                                                    }
                                                                }
                                                        , declaration =
                                                            { range = { start = { row = 3, column = 5 }, end = { row = 3, column = 12 } }
                                                            , value =
                                                                { name =
                                                                    { range = { start = { row = 3, column = 5 }, end = { row = 3, column = 8 } }, value = "bar" }
                                                                , parameters = []
                                                                , expression =
                                                                    { range = { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } }
                                                                    , value = GrenSyntax.ExpressionInteger 1
                                                                    }
                                                                }
                                                            }
                                                        }
                                              }
                                            ]
                                        , result =
                                            { range = { start = { row = 5, column = 3 }, end = { row = 5, column = 6 } }
                                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "bar" }
                                            }
                                        }
                                }
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
                                { range = { start = { row = 1, column = 1 }, end = { row = 4, column = 12 } }
                                , value =
                                    GrenSyntax.ExpressionLetIn
                                        { declarations =
                                            [ { range = { start = { row = 2, column = 11 }, end = { row = 2, column = 24 } }
                                              , value =
                                                    GrenSyntax.LetFunction
                                                        { documentation = Nothing
                                                        , signature = Nothing
                                                        , declaration =
                                                            { range = { start = { row = 2, column = 11 }, end = { row = 2, column = 24 } }
                                                            , value =
                                                                { name =
                                                                    { range = { start = { row = 2, column = 11 }, end = { row = 2, column = 14 } }, value = "bar" }
                                                                , parameters =
                                                                    [ { range = { start = { row = 2, column = 15 }, end = { row = 2, column = 18 } }
                                                                      , value = GrenSyntax.PatternVariant { qualification = [], name = "Bar", value = Nothing }
                                                                      }
                                                                    , { range = { start = { row = 2, column = 19 }, end = { row = 2, column = 20 } }
                                                                      , value = GrenSyntax.PatternVariable "m"
                                                                      }
                                                                    ]
                                                                , expression =
                                                                    { range = { start = { row = 2, column = 23 }, end = { row = 2, column = 24 } }
                                                                    , value = GrenSyntax.ExpressionInteger 1
                                                                    }
                                                                }
                                                            }
                                                        }
                                              }
                                            ]
                                        , result =
                                            { range = { start = { row = 4, column = 9 }, end = { row = 4, column = 12 } }
                                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "bar" }
                                            }
                                        }
                                }
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
    (c   ) = e
    (Node _  ) = g
 in
    1"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                { range = { start = { row = 1, column = 1 }, end = { row = 7, column = 6 } }
                                , value =
                                    GrenSyntax.ExpressionLetIn
                                        { declarations =
                                            [ { range = { start = { row = 2, column = 5 }, end = { row = 2, column = 10 } }
                                              , value =
                                                    GrenSyntax.LetDestructuring
                                                        { pattern =
                                                            { range = { start = { row = 2, column = 5 }, end = { row = 2, column = 6 } }
                                                            , value = GrenSyntax.PatternIgnored Nothing
                                                            }
                                                        , expression =
                                                            { range = { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } }
                                                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "b" }
                                                            }
                                                        }
                                              }
                                            , { range = { start = { row = 3, column = 5 }, end = { row = 3, column = 12 } }
                                              , value =
                                                    GrenSyntax.LetDestructuring
                                                        { pattern =
                                                            { range = { start = { row = 3, column = 5 }, end = { row = 3, column = 8 } }
                                                            , value =
                                                                GrenSyntax.PatternRecord
                                                                    [ { value = Nothing
                                                                      , name =
                                                                            { range = { start = { row = 3, column = 6 }, end = { row = 3, column = 7 } }, value = "a" }
                                                                      }
                                                                    ]
                                                            }
                                                        , expression =
                                                            { range = { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } }
                                                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "b" }
                                                            }
                                                        }
                                              }
                                            , { range = { start = { row = 4, column = 5 }, end = { row = 4, column = 15 } }
                                              , value =
                                                    GrenSyntax.LetDestructuring
                                                        { pattern =
                                                            { range = { start = { row = 4, column = 5 }, end = { row = 4, column = 11 } }
                                                            , value =
                                                                GrenSyntax.PatternParenthesized
                                                                    { range = { start = { row = 4, column = 6 }, end = { row = 4, column = 7 } }
                                                                    , value = GrenSyntax.PatternVariable "c"
                                                                    }
                                                            }
                                                        , expression =
                                                            { range = { start = { row = 4, column = 14 }, end = { row = 4, column = 15 } }
                                                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "e" }
                                                            }
                                                        }
                                              }
                                            , { range = { start = { row = 5, column = 5 }, end = { row = 5, column = 19 } }
                                              , value =
                                                    GrenSyntax.LetDestructuring
                                                        { pattern =
                                                            { range = { start = { row = 5, column = 5 }, end = { row = 5, column = 15 } }
                                                            , value =
                                                                GrenSyntax.PatternParenthesized
                                                                    { range = { start = { row = 5, column = 6 }, end = { row = 5, column = 12 } }
                                                                    , value =
                                                                        GrenSyntax.PatternVariant
                                                                            { qualification = []
                                                                            , name = "Node"
                                                                            , value =
                                                                                Just
                                                                                    { range = { start = { row = 5, column = 11 }, end = { row = 5, column = 12 } }
                                                                                    , value = GrenSyntax.PatternIgnored Nothing
                                                                                    }
                                                                            }
                                                                    }
                                                            }
                                                        , expression =
                                                            { range = { start = { row = 5, column = 18 }, end = { row = 5, column = 19 } }
                                                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "g" }
                                                            }
                                                        }
                                              }
                                            ]
                                        , result =
                                            { range = { start = { row = 7, column = 5 }, end = { row = 7, column = 6 } }
                                            , value = GrenSyntax.ExpressionInteger 1
                                            }
                                        }
                                }
                    )
                , Test.test "On one line"
                    (\() ->
                        "let indent = String.length s in indent"
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 39 } }
                                , value =
                                    GrenSyntax.ExpressionLetIn
                                        { declarations =
                                            [ { range = { start = { row = 1, column = 5 }, end = { row = 1, column = 29 } }
                                              , value =
                                                    GrenSyntax.LetFunction
                                                        { documentation = Nothing
                                                        , signature = Nothing
                                                        , declaration =
                                                            { range = { start = { row = 1, column = 5 }, end = { row = 1, column = 29 } }
                                                            , value =
                                                                { name =
                                                                    { range = { start = { row = 1, column = 5 }, end = { row = 1, column = 11 } }, value = "indent" }
                                                                , parameters = []
                                                                , expression =
                                                                    { range = { start = { row = 1, column = 14 }, end = { row = 1, column = 29 } }
                                                                    , value =
                                                                        GrenSyntax.ExpressionCall
                                                                            [ { range = { start = { row = 1, column = 14 }, end = { row = 1, column = 27 } }
                                                                              , value = GrenSyntax.ExpressionReference { qualification = [ "String" ], name = "length" }
                                                                              }
                                                                            , { range = { start = { row = 1, column = 28 }, end = { row = 1, column = 29 } }
                                                                              , value = GrenSyntax.ExpressionReference { qualification = [], name = "s" }
                                                                              }
                                                                            ]
                                                                    }
                                                                }
                                                            }
                                                        }
                                              }
                                            ]
                                        , result =
                                            { range = { start = { row = 1, column = 33 }, end = { row = 1, column = 39 } }
                                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "indent" }
                                            }
                                        }
                                }
                    )
                , Test.test "let with list after in without space"
                    (\() ->
                        """let
        a = 1
    in[]"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                { range = { start = { row = 1, column = 1 }, end = { row = 3, column = 9 } }
                                , value =
                                    GrenSyntax.ExpressionLetIn
                                        { declarations =
                                            [ { range = { start = { row = 2, column = 9 }, end = { row = 2, column = 14 } }
                                              , value =
                                                    GrenSyntax.LetFunction
                                                        { documentation = Nothing
                                                        , signature = Nothing
                                                        , declaration =
                                                            { range = { start = { row = 2, column = 9 }, end = { row = 2, column = 14 } }
                                                            , value =
                                                                { name = { range = { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } }, value = "a" }
                                                                , parameters = []
                                                                , expression =
                                                                    { range = { start = { row = 2, column = 13 }, end = { row = 2, column = 14 } }
                                                                    , value = GrenSyntax.ExpressionInteger 1
                                                                    }
                                                                }
                                                            }
                                                        }
                                              }
                                            ]
                                        , result =
                                            { range = { start = { row = 3, column = 7 }, end = { row = 3, column = 9 } }
                                            , value = GrenSyntax.ExpressionArray []
                                            }
                                        }
                                }
                    )
                , Test.test "let with record after in without space"
                    (\() ->
                        """let
        a = 1
    in{}"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                { range = { start = { row = 1, column = 1 }, end = { row = 3, column = 9 } }
                                , value =
                                    GrenSyntax.ExpressionLetIn
                                        { declarations =
                                            [ { range = { start = { row = 2, column = 9 }, end = { row = 2, column = 14 } }
                                              , value =
                                                    GrenSyntax.LetFunction
                                                        { documentation = Nothing
                                                        , signature = Nothing
                                                        , declaration =
                                                            { range = { start = { row = 2, column = 9 }, end = { row = 2, column = 14 } }
                                                            , value =
                                                                { name = { range = { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } }, value = "a" }
                                                                , parameters = []
                                                                , expression =
                                                                    { range = { start = { row = 2, column = 13 }, end = { row = 2, column = 14 } }
                                                                    , value = GrenSyntax.ExpressionInteger 1
                                                                    }
                                                                }
                                                            }
                                                        }
                                              }
                                            ]
                                        , result =
                                            { range = { start = { row = 3, column = 7 }, end = { row = 3, column = 9 } }
                                            , value = GrenSyntax.ExpressionRecord []
                                            }
                                        }
                                }
                    )
                , Test.test "let with lambda after in without space"
                    (\() ->
                        """let
        a = 1
    in\\_ -> 1"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                { range = { start = { row = 1, column = 1 }, end = { row = 3, column = 14 } }
                                , value =
                                    GrenSyntax.ExpressionLetIn
                                        { declarations =
                                            [ { range = { start = { row = 2, column = 9 }, end = { row = 2, column = 14 } }
                                              , value =
                                                    GrenSyntax.LetFunction
                                                        { documentation = Nothing
                                                        , signature = Nothing
                                                        , declaration =
                                                            { range = { start = { row = 2, column = 9 }, end = { row = 2, column = 14 } }
                                                            , value =
                                                                { name = { range = { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } }, value = "a" }
                                                                , parameters = []
                                                                , expression =
                                                                    { range = { start = { row = 2, column = 13 }, end = { row = 2, column = 14 } }
                                                                    , value = GrenSyntax.ExpressionInteger 1
                                                                    }
                                                                }
                                                            }
                                                        }
                                              }
                                            ]
                                        , result =
                                            { range = { start = { row = 3, column = 7 }, end = { row = 3, column = 14 } }
                                            , value =
                                                GrenSyntax.ExpressionLambda
                                                    { parameters =
                                                        [ { range = { start = { row = 3, column = 8 }, end = { row = 3, column = 9 } }
                                                          , value = GrenSyntax.PatternIgnored Nothing
                                                          }
                                                        ]
                                                    , result =
                                                        { range = { start = { row = 3, column = 13 }, end = { row = 3, column = 14 } }
                                                        , value = GrenSyntax.ExpressionInteger 1
                                                        }
                                                    }
                                            }
                                        }
                                }
                    )
                , Test.test "let is not confused by a variable name starting with let"
                    (\() ->
                        "letterbox"
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                                , value = GrenSyntax.ExpressionReference { qualification = [], name = "letterbox" }
                                }
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
                                { range = { start = { row = 1, column = 1 }, end = { row = 3, column = 13 } }
                                , value =
                                    GrenSyntax.ExpressionCaseOf
                                        { expression =
                                            { range = { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } }
                                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "f" }
                                            }
                                        , cases =
                                            [ { pattern =
                                                    { range = { start = { row = 2, column = 3 }, end = { row = 2, column = 7 } }
                                                    , value = GrenSyntax.PatternVariant { qualification = [], name = "True", value = Nothing }
                                                    }
                                              , result =
                                                    { range = { start = { row = 2, column = 11 }, end = { row = 2, column = 12 } }
                                                    , value = GrenSyntax.ExpressionInteger 1
                                                    }
                                              }
                                            , { pattern =
                                                    { range = { start = { row = 3, column = 3 }, end = { row = 3, column = 8 } }
                                                    , value = GrenSyntax.PatternVariant { qualification = [], name = "False", value = Nothing }
                                                    }
                                              , result =
                                                    { range = { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } }
                                                    , value = GrenSyntax.ExpressionInteger 2
                                                    }
                                              }
                                            ]
                                        }
                                }
                    )
                , Test.test "allow . between case pattern and result"
                    (\() ->
                        """case [] of
    _ .  1"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                { range = { end = { column = 11, row = 2 }, start = { column = 1, row = 1 } }
                                , value =
                                    GrenSyntax.ExpressionCaseOf
                                        { cases =
                                            [ { pattern =
                                                    { range = { end = { column = 6, row = 2 }, start = { column = 5, row = 2 } }
                                                    , value = GrenSyntax.PatternIgnored Nothing
                                                    }
                                              , result =
                                                    { range = { end = { column = 11, row = 2 }, start = { column = 10, row = 2 } }
                                                    , value = GrenSyntax.ExpressionInteger 1
                                                    }
                                              }
                                            ]
                                        , expression =
                                            { range = { end = { column = 8, row = 1 }, start = { column = 6, row = 1 } }
                                            , value = GrenSyntax.ExpressionArray []
                                            }
                                        }
                                }
                    )
                , Test.test "allow no symbol at all between case pattern and result"
                    (\() ->
                        """case [] of
    _    1"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                { range = { end = { column = 11, row = 2 }, start = { column = 1, row = 1 } }
                                , value =
                                    GrenSyntax.ExpressionCaseOf
                                        { cases =
                                            [ { pattern =
                                                    { range = { end = { column = 6, row = 2 }, start = { column = 5, row = 2 } }
                                                    , value = GrenSyntax.PatternIgnored Nothing
                                                    }
                                              , result =
                                                    { range = { end = { column = 11, row = 2 }, start = { column = 10, row = 2 } }
                                                    , value = GrenSyntax.ExpressionInteger 1
                                                    }
                                              }
                                            ]
                                        , expression =
                                            { range = { end = { column = 8, row = 1 }, start = { column = 6, row = 1 } }
                                            , value = GrenSyntax.ExpressionArray []
                                            }
                                        }
                                }
                    )
                , Test.test "when-is expression with `when` after simple cased expression"
                    (\() ->
                        """f when
  True -> 1
  False -> 2"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                { range = { start = { row = 1, column = 1 }, end = { row = 3, column = 13 } }
                                , value =
                                    GrenSyntax.ExpressionCaseOf
                                        { expression =
                                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } }
                                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "f" }
                                            }
                                        , cases =
                                            [ { pattern =
                                                    { range = { start = { row = 2, column = 3 }, end = { row = 2, column = 7 } }
                                                    , value = GrenSyntax.PatternVariant { qualification = [], name = "True", value = Nothing }
                                                    }
                                              , result =
                                                    { range = { start = { row = 2, column = 11 }, end = { row = 2, column = 12 } }
                                                    , value = GrenSyntax.ExpressionInteger 1
                                                    }
                                              }
                                            , { pattern =
                                                    { range = { start = { row = 3, column = 3 }, end = { row = 3, column = 8 } }
                                                    , value = GrenSyntax.PatternVariant { qualification = [], name = "False", value = Nothing }
                                                    }
                                              , result =
                                                    { range = { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } }
                                                    , value = GrenSyntax.ExpressionInteger 2
                                                    }
                                              }
                                            ]
                                        }
                                }
                    )
                , Test.test "when-is expression with `when` after cased expression infix operation"
                    (\() ->
                        """f |> g when
  True -> 1
  False -> 2"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                { range = { start = { row = 1, column = 1 }, end = { row = 3, column = 13 } }
                                , value =
                                    GrenSyntax.ExpressionCaseOf
                                        { expression =
                                            { range = { end = { column = 7, row = 1 }, start = { column = 1, row = 1 } }
                                            , value =
                                                GrenSyntax.ExpressionInfixOperation
                                                    { operator = "|>"
                                                    , left =
                                                        { range = { end = { column = 2, row = 1 }, start = { column = 1, row = 1 } }
                                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "f" }
                                                        }
                                                    , right =
                                                        { range = { end = { column = 7, row = 1 }, start = { column = 6, row = 1 } }
                                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "g" }
                                                        }
                                                    }
                                            }
                                        , cases =
                                            [ { pattern =
                                                    { range = { start = { row = 2, column = 3 }, end = { row = 2, column = 7 } }
                                                    , value = GrenSyntax.PatternVariant { qualification = [], name = "True", value = Nothing }
                                                    }
                                              , result =
                                                    { range = { start = { row = 2, column = 11 }, end = { row = 2, column = 12 } }
                                                    , value = GrenSyntax.ExpressionInteger 1
                                                    }
                                              }
                                            , { pattern =
                                                    { range = { start = { row = 3, column = 3 }, end = { row = 3, column = 8 } }
                                                    , value = GrenSyntax.PatternVariant { qualification = [], name = "False", value = Nothing }
                                                    }
                                              , result =
                                                    { range = { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } }
                                                    , value = GrenSyntax.ExpressionInteger 2
                                                    }
                                              }
                                            ]
                                        }
                                }
                    )
                , Test.test "case expression with qualified imports"
                    (\() ->
                        """case f of
  Foo.Bar -> 1"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                { range = { start = { row = 1, column = 1 }, end = { row = 2, column = 15 } }
                                , value =
                                    GrenSyntax.ExpressionCaseOf
                                        { expression =
                                            { range = { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } }
                                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "f" }
                                            }
                                        , cases =
                                            [ { pattern =
                                                    { range = { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } }
                                                    , value = GrenSyntax.PatternVariant { qualification = [ "Foo" ], name = "Bar", value = Nothing }
                                                    }
                                              , result =
                                                    { range = { start = { row = 2, column = 14 }, end = { row = 2, column = 15 } }
                                                    , value = GrenSyntax.ExpressionInteger 1
                                                    }
                                              }
                                            ]
                                        }
                                }
                    )
                , Test.test "case expression with no space between pattern and value"
                    (\() ->
                        """case f of
  x->1"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                { range = { start = { row = 1, column = 1 }, end = { row = 2, column = 7 } }
                                , value =
                                    GrenSyntax.ExpressionCaseOf
                                        { expression =
                                            { range = { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } }
                                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "f" }
                                            }
                                        , cases =
                                            [ { pattern =
                                                    { range = { start = { row = 2, column = 3 }, end = { row = 2, column = 4 } }
                                                    , value = GrenSyntax.PatternVariable "x"
                                                    }
                                              , result =
                                                    { range = { start = { row = 2, column = 6 }, end = { row = 2, column = 7 } }
                                                    , value = GrenSyntax.ExpressionInteger 1
                                                    }
                                              }
                                            ]
                                        }
                                }
                    )
                , Test.test "should parse case expression with first branch on the same line as case of"
                    (\() ->
                        """case x of True -> 1
          False -> 2"""
                            |> expectSyntaxWithoutComments GrenParserLenient.expression
                                { range = { start = { row = 1, column = 1 }, end = { row = 2, column = 21 } }
                                , value =
                                    GrenSyntax.ExpressionCaseOf
                                        { expression =
                                            { range = { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } }
                                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "x" }
                                            }
                                        , cases =
                                            [ { pattern =
                                                    { range = { start = { row = 1, column = 11 }, end = { row = 1, column = 15 } }
                                                    , value = GrenSyntax.PatternVariant { qualification = [], name = "True", value = Nothing }
                                                    }
                                              , result =
                                                    { range = { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } }
                                                    , value = GrenSyntax.ExpressionInteger 1
                                                    }
                                              }
                                            , { pattern =
                                                    { range = { start = { row = 2, column = 11 }, end = { row = 2, column = 16 } }
                                                    , value = GrenSyntax.PatternVariant { qualification = [], name = "False", value = Nothing }
                                                    }
                                              , result =
                                                    { range = { start = { row = 2, column = 20 }, end = { row = 2, column = 21 } }
                                                    , value = GrenSyntax.ExpressionInteger 2
                                                    }
                                              }
                                            ]
                                        }
                                }
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
                                { range = { start = { row = 1, column = 1 }, end = { row = 7, column = 15 } }
                                , value =
                                    GrenSyntax.ExpressionCaseOf
                                        { expression =
                                            { range = { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } }
                                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "x" }
                                            }
                                        , cases =
                                            [ { pattern =
                                                    { range = { start = { row = 2, column = 9 }, end = { row = 2, column = 39 } }
                                                    , value =
                                                        GrenSyntax.PatternString
                                                            { quotingStyle = GrenSyntax.StringTripleQuoted, content = "single line triple quote" }
                                                    }
                                              , result =
                                                    { range = { start = { row = 3, column = 13 }, end = { row = 3, column = 14 } }
                                                    , value = GrenSyntax.ExpressionInteger 1
                                                    }
                                              }
                                            , { pattern =
                                                    { range = { start = { row = 4, column = 9 }, end = { row = 5, column = 28 } }
                                                    , value =
                                                        GrenSyntax.PatternString
                                                            { quotingStyle = GrenSyntax.StringTripleQuoted
                                                            , content = "multi line\n            triple quote"
                                                            }
                                                    }
                                              , result =
                                                    { range = { start = { row = 6, column = 13 }, end = { row = 6, column = 14 } }
                                                    , value = GrenSyntax.ExpressionInteger 2
                                                    }
                                              }
                                            , { pattern =
                                                    { range = { start = { row = 7, column = 9 }, end = { row = 7, column = 10 } }
                                                    , value = GrenSyntax.PatternIgnored Nothing
                                                    }
                                              , result =
                                                    { range = { start = { row = 7, column = 14 }, end = { row = 7, column = 15 } }
                                                    , value = GrenSyntax.ExpressionInteger 3
                                                    }
                                              }
                                            ]
                                        }
                                }
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
                                { range = { start = { row = 1, column = 1 }, end = { row = 6, column = 6 } }
                                , value =
                                    GrenSyntax.ExpressionCaseOf
                                        { expression =
                                            { range = { start = { row = 1, column = 6 }, end = { row = 1, column = 9 } }
                                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "msg" }
                                            }
                                        , cases =
                                            [ { pattern =
                                                    { range = { start = { row = 2, column = 3 }, end = { row = 2, column = 12 } }
                                                    , value = GrenSyntax.PatternVariant { qualification = [], name = "Increment", value = Nothing }
                                                    }
                                              , result =
                                                    { range = { start = { row = 3, column = 5 }, end = { row = 3, column = 6 } }
                                                    , value = GrenSyntax.ExpressionInteger 1
                                                    }
                                              }
                                            , { pattern =
                                                    { range = { start = { row = 5, column = 3 }, end = { row = 5, column = 12 } }
                                                    , value = GrenSyntax.PatternVariant { qualification = [], name = "Decrement", value = Nothing }
                                                    }
                                              , result =
                                                    { range = { start = { row = 6, column = 5 }, end = { row = 6, column = 6 } }
                                                    , value = GrenSyntax.ExpressionInteger 2
                                                    }
                                              }
                                            ]
                                        }
                                }
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
            ]
        , Test.describe "pattern"
            [ Test.test "Unit"
                (\() ->
                    "()"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } }
                            , value = GrenSyntax.PatternUnit
                            }
                )
            , Test.test "Unit with inner layout"
                (\() ->
                    """(
   -- comment
   )"""
                        |> expectSyntaxWithComments GrenParserLenient.pattern
                            { syntax =
                                { range = { start = { row = 1, column = 1 }, end = { row = 3, column = 5 } }
                                , value = GrenSyntax.PatternUnit
                                }
                            , comments =
                                [ { range = { start = { row = 2, column = 4 }, end = { row = 2, column = 14 } }
                                  , value = "-- comment"
                                  }
                                ]
                            }
                )
            , Test.test "String"
                (\() ->
                    "\"Foo\""
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                            , value = GrenSyntax.PatternString { quotingStyle = GrenSyntax.StringSingleQuoted, content = "Foo" }
                            }
                )
            , Test.test "Char"
                (\() ->
                    "'f'"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                            , value = GrenSyntax.PatternChar 'f'
                            }
                )
            , Test.test "Wildcard"
                (\() ->
                    "_"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } }
                            , value = GrenSyntax.PatternIgnored Nothing
                            }
                )
            , Test.test "Wildcard with custom name"
                (\() ->
                    "_name"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                            , value = GrenSyntax.PatternIgnored (Just "name")
                            }
                )
            , Test.test "Parenthesized"
                (\() ->
                    "(x)"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                            , value =
                                GrenSyntax.PatternParenthesized
                                    { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } }
                                    , value = GrenSyntax.PatternVariable "x"
                                    }
                            }
                )
            , Test.test "Int"
                (\() ->
                    "1"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } }
                            , value = GrenSyntax.PatternInt 1
                            }
                )
            , Test.test "Hex int"
                (\() ->
                    "0x1"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                            , value = GrenSyntax.PatternHex 1
                            }
                )
            , Test.test "Float should not be valid" (\() -> expectFailsToParse GrenParserLenient.pattern "1.0")
            , Test.test "Uncons"
                (\() ->
                    "n :: tail"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                            , value =
                                GrenSyntax.PatternListCons
                                    { head =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } }
                                        , value = GrenSyntax.PatternVariable "n"
                                        }
                                    , tail =
                                        { range = { start = { row = 1, column = 6 }, end = { row = 1, column = 10 } }
                                        , value = GrenSyntax.PatternVariable "tail"
                                        }
                                    }
                            }
                )
            , Test.test "Uncons multiple"
                (\() ->
                    "a :: b :: cUp"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 14 } }
                            , value =
                                GrenSyntax.PatternListCons
                                    { head =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } }
                                        , value = GrenSyntax.PatternVariable "a"
                                        }
                                    , tail =
                                        { range = { start = { row = 1, column = 6 }, end = { row = 1, column = 14 } }
                                        , value =
                                            GrenSyntax.PatternListCons
                                                { head =
                                                    { range = { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } }
                                                    , value = GrenSyntax.PatternVariable "b"
                                                    }
                                                , tail =
                                                    { range = { start = { row = 1, column = 11 }, end = { row = 1, column = 14 } }
                                                    , value = GrenSyntax.PatternVariable "cUp"
                                                    }
                                                }
                                        }
                                    }
                            }
                )
            , Test.test "Uncons with parens"
                (\() ->
                    "(X x) :: xs"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                            , value =
                                GrenSyntax.PatternListCons
                                    { head =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                                        , value =
                                            GrenSyntax.PatternParenthesized
                                                { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 5 } }
                                                , value =
                                                    GrenSyntax.PatternVariant
                                                        { qualification = []
                                                        , name = "X"
                                                        , value =
                                                            Just
                                                                { range = { start = { row = 1, column = 4 }, end = { row = 1, column = 5 } }
                                                                , value = GrenSyntax.PatternVariable "x"
                                                                }
                                                        }
                                                }
                                        }
                                    , tail =
                                        { range = { start = { row = 1, column = 10 }, end = { row = 1, column = 12 } }
                                        , value = GrenSyntax.PatternVariable "xs"
                                        }
                                    }
                            }
                )
            , Test.test "Empty list"
                (\() ->
                    "[]"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } }
                            , value = GrenSyntax.PatternListExact []
                            }
                )
            , Test.test "Empty list pattern with whitespace"
                (\() ->
                    "[ ]"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                            , value = GrenSyntax.PatternListExact []
                            }
                )
            , Test.test "Single element list"
                (\() ->
                    "[1]"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                            , value =
                                GrenSyntax.PatternListExact
                                    [ { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } }
                                      , value = GrenSyntax.PatternInt 1
                                      }
                                    ]
                            }
                )
            , Test.test "Single element list with trailing whitespace"
                (\() ->
                    "[1 ]"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }
                            , value =
                                GrenSyntax.PatternListExact
                                    [ { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } }
                                      , value = GrenSyntax.PatternInt 1
                                      }
                                    ]
                            }
                )
            , Test.test "Single element list with leading whitespace"
                (\() ->
                    "[ 1]"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }
                            , value =
                                GrenSyntax.PatternListExact
                                    [ { range = { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } }
                                      , value = GrenSyntax.PatternInt 1
                                      }
                                    ]
                            }
                )
            , Test.test "list with prefix extra comma"
                (\() ->
                    "[,1]"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }
                            , value =
                                GrenSyntax.PatternListExact
                                    [ { range = { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } }
                                      , value = GrenSyntax.PatternInt 1
                                      }
                                    ]
                            }
                )
            , Test.test "list with extra comma between elements"
                (\() ->
                    "[1,,2]"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                            , value =
                                GrenSyntax.PatternListExact
                                    [ { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } }
                                      , value = GrenSyntax.PatternInt 1
                                      }
                                    , { range = { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } }
                                      , value = GrenSyntax.PatternInt 2
                                      }
                                    ]
                            }
                )
            , Test.test "Empty record"
                (\() ->
                    "{}"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } }
                            , value = GrenSyntax.PatternRecord []
                            }
                )
            , Test.test "Empty record with whitespace"
                (\() ->
                    "{ }"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                            , value = GrenSyntax.PatternRecord []
                            }
                )
            , Test.test "Record"
                (\() ->
                    "{a,b}"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                            , value =
                                GrenSyntax.PatternRecord
                                    [ { value = Nothing
                                      , name =
                                            { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } }, value = "a" }
                                      }
                                    , { value = Nothing
                                      , name =
                                            { range = { start = { row = 1, column = 4 }, end = { row = 1, column = 5 } }, value = "b" }
                                      }
                                    ]
                            }
                )
            , Test.test "Record pattern with whitespace"
                (\() ->
                    "{a , b}"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                            , value =
                                GrenSyntax.PatternRecord
                                    [ { value = Nothing
                                      , name =
                                            { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } }, value = "a" }
                                      }
                                    , { value = Nothing
                                      , name =
                                            { range = { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } }, value = "b" }
                                      }
                                    ]
                            }
                )
            , Test.test "Record pattern with renamed field"
                (\() ->
                    "{a=a_,b}"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 9 } }
                            , value =
                                GrenSyntax.PatternRecord
                                    [ { value =
                                            Just
                                                { range = { start = { row = 1, column = 4 }, end = { row = 1, column = 6 } }
                                                , value = GrenSyntax.PatternVariable "a_"
                                                }
                                      , name =
                                            { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } }, value = "a" }
                                      }
                                    , { value = Nothing
                                      , name =
                                            { range = { start = { row = 1, column = 7 }, end = { row = 1, column = 8 } }, value = "b" }
                                      }
                                    ]
                            }
                )
            , Test.test "Record pattern with field destructured to unit"
                (\() ->
                    "{a={},b}"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 9 } }
                            , value =
                                GrenSyntax.PatternRecord
                                    [ { value =
                                            Just
                                                { range = { start = { row = 1, column = 4 }, end = { row = 1, column = 6 } }
                                                , value = GrenSyntax.PatternRecord []
                                                }
                                      , name =
                                            { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } }, value = "a" }
                                      }
                                    , { value = Nothing
                                      , name =
                                            { range = { start = { row = 1, column = 7 }, end = { row = 1, column = 8 } }, value = "b" }
                                      }
                                    ]
                            }
                )
            , Test.test "Record pattern with extra comma between fields"
                (\() ->
                    "{a,, b}"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                            , value =
                                GrenSyntax.PatternRecord
                                    [ { value = Nothing
                                      , name =
                                            { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } }, value = "a" }
                                      }
                                    , { value = Nothing
                                      , name =
                                            { range = { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } }, value = "b" }
                                      }
                                    ]
                            }
                )
            , Test.test "Record pattern with prefixed comma"
                (\() ->
                    "{ , a , b }"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                            , value =
                                GrenSyntax.PatternRecord
                                    [ { value = Nothing
                                      , name =
                                            { range = { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } }, value = "a" }
                                      }
                                    , { value = Nothing
                                      , name =
                                            { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } }, value = "b" }
                                      }
                                    ]
                            }
                )
            , Test.test "Record pattern with field name colliding with keyword"
                (\() ->
                    "{ ,as , b }"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                            , value =
                                GrenSyntax.PatternRecord
                                    [ { value = Nothing
                                      , name =
                                            { range = { start = { row = 1, column = 4 }, end = { row = 1, column = 6 } }, value = "as_" }
                                      }
                                    , { value = Nothing
                                      , name =
                                            { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } }, value = "b" }
                                      }
                                    ]
                            }
                )
            , Test.test "Record pattern with trailing whitespace"
                (\() ->
                    "{a }"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }
                            , value =
                                GrenSyntax.PatternRecord
                                    [ { value = Nothing
                                      , name =
                                            { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } }, value = "a" }
                                      }
                                    ]
                            }
                )
            , Test.test "Record pattern with leading whitespace"
                (\() ->
                    "{ a}"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }
                            , value =
                                GrenSyntax.PatternRecord
                                    [ { value = Nothing
                                      , name =
                                            { range = { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } }, value = "a" }
                                      }
                                    ]
                            }
                )
            , Test.test "Named"
                (\() ->
                    "True"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }
                            , value = GrenSyntax.PatternVariant { qualification = [], name = "True", value = Nothing }
                            }
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
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                            , value = GrenSyntax.PatternVariant { qualification = [ "Basics" ], name = "True", value = Nothing }
                            }
                )
            , Test.test "Named pattern with data"
                (\() ->
                    "Set x"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                            , value =
                                GrenSyntax.PatternVariant
                                    { qualification = []
                                    , name = "Set"
                                    , value =
                                        Just
                                            { range = { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } }
                                            , value = GrenSyntax.PatternVariable "x"
                                            }
                                    }
                            }
                )
            , Test.test "Qualified named pattern with data"
                (\() ->
                    "Set.Set x"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                            , value =
                                GrenSyntax.PatternVariant
                                    { qualification = [ "Set" ]
                                    , name = "Set"
                                    , value =
                                        Just
                                            { range = { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } }
                                            , value = GrenSyntax.PatternVariable "x"
                                            }
                                    }
                            }
                )
            , Test.test "As pattern"
                (\() ->
                    "x as y"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                            , value =
                                GrenSyntax.PatternAs
                                    { pattern =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } }
                                        , value = GrenSyntax.PatternVariable "x"
                                        }
                                    , variable =
                                        { range = { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } }, value = "y" }
                                    }
                            }
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
                            { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 28 } }
                            , value =
                                GrenSyntax.PatternAs
                                    { pattern =
                                        { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 16 } }
                                        , value =
                                            GrenSyntax.PatternRecord
                                                [ { value = Nothing
                                                  , name =
                                                        { range = { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } }, value = "model" }
                                                  }
                                                , { value = Nothing
                                                  , name =
                                                        { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 15 } }, value = "context" }
                                                  }
                                                ]
                                        }
                                    , variable =
                                        { range = { start = { row = 1, column = 20 }, end = { row = 1, column = 28 } }, value = "appState" }
                                    }
                            }
                )
            , Test.test "Complex pattern"
                (\() ->
                    "RBNode_gren_builtin     (RBNode_gren_builtin      (RBNode_gren_builtin Red   ))"
                        |> expectSyntaxWithoutComments GrenParserLenient.pattern
                            { range = { end = { column = 80, row = 1 }, start = { column = 1, row = 1 } }
                            , value =
                                GrenSyntax.PatternVariant
                                    { qualification = []
                                    , name = "RBNode_gren_builtin"
                                    , value =
                                        Just
                                            { range = { end = { column = 80, row = 1 }, start = { column = 25, row = 1 } }
                                            , value =
                                                GrenSyntax.PatternParenthesized
                                                    { range = { end = { column = 79, row = 1 }, start = { column = 26, row = 1 } }
                                                    , value =
                                                        GrenSyntax.PatternVariant
                                                            { qualification = []
                                                            , name = "RBNode_gren_builtin"
                                                            , value =
                                                                Just
                                                                    { range = { end = { column = 79, row = 1 }, start = { column = 51, row = 1 } }
                                                                    , value =
                                                                        GrenSyntax.PatternParenthesized
                                                                            { range = { end = { column = 75, row = 1 }, start = { column = 52, row = 1 } }
                                                                            , value =
                                                                                GrenSyntax.PatternVariant
                                                                                    { qualification = []
                                                                                    , name = "RBNode_gren_builtin"
                                                                                    , value =
                                                                                        Just
                                                                                            { range = { end = { column = 75, row = 1 }, start = { column = 72, row = 1 } }
                                                                                            , value = GrenSyntax.PatternVariant { qualification = [], name = "Red", value = Nothing }
                                                                                            }
                                                                                    }
                                                                            }
                                                                    }
                                                            }
                                                    }
                                            }
                                    }
                            }
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
                                    { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                    , value =
                                        GrenSyntax.NormalModule
                                            { moduleName =
                                                { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }, value = [ "Bar" ] }
                                            , exposingList =
                                                { range = { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } }
                                                , value = GrenSyntax.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                                                }
                                            }
                                    }
                                , imports =
                                    [ { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 14 } }
                                      , value =
                                            { moduleName =
                                                { range = { start = { row = 3, column = 8 }, end = { row = 3, column = 14 } }
                                                , value = [ "String" ]
                                                }
                                            , moduleAlias = Nothing
                                            , exposingList = Nothing
                                            }
                                      }
                                    ]
                                , declarations =
                                    [ { range = { start = { row = 5, column = 1 }, end = { row = 7, column = 8 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { documentation =
                                                    Just
                                                        { range = { start = { row = 5, column = 1 }, end = { row = 6, column = 3 } }
                                                        , value = "{-| The docs\n-}"
                                                        }
                                                , signature = Nothing
                                                , declaration =
                                                    { range = { start = { row = 7, column = 1 }, end = { row = 7, column = 8 } }
                                                    , value =
                                                        { name =
                                                            { range = { start = { row = 7, column = 1 }, end = { row = 7, column = 4 } }, value = "bar" }
                                                        , parameters = []
                                                        , expression =
                                                            { range = { start = { row = 7, column = 7 }, end = { row = 7, column = 8 } }
                                                            , value = GrenSyntax.ExpressionInteger 1
                                                            }
                                                        }
                                                    }
                                                }
                                      }
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
                                    { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                    , value =
                                        GrenSyntax.NormalModule
                                            { moduleName =
                                                { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }, value = [ "Bar" ] }
                                            , exposingList =
                                                { range = { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } }
                                                , value = GrenSyntax.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                                                }
                                            }
                                    }
                                , imports =
                                    [ { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 14 } }
                                      , value =
                                            { moduleName =
                                                { range = { start = { row = 3, column = 8 }, end = { row = 3, column = 14 } }
                                                , value = [ "String" ]
                                                }
                                            , moduleAlias = Nothing
                                            , exposingList = Nothing
                                            }
                                      }
                                    ]
                                , declarations =
                                    [ { range = { start = { row = 5, column = 1 }, end = { row = 8, column = 8 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { documentation =
                                                    Just
                                                        { range = { start = { row = 5, column = 1 }, end = { row = 6, column = 3 } }
                                                        , value = "{-| The docs\n-}"
                                                        }
                                                , signature =
                                                    Just
                                                        { range = { start = { row = 7, column = 1 }, end = { row = 7, column = 10 } }
                                                        , value =
                                                            { name =
                                                                { range = { start = { row = 7, column = 1 }, end = { row = 7, column = 4 } }, value = "bar" }
                                                            , typeAnnotation =
                                                                { range = { start = { row = 7, column = 7 }, end = { row = 7, column = 10 } }
                                                                , value =
                                                                    GrenSyntax.TypeAnnotationConstruct
                                                                        { reference =
                                                                            { range = { start = { row = 7, column = 7 }, end = { row = 7, column = 10 } }
                                                                            , value = { qualification = [], name = "Int" }
                                                                            }
                                                                        , arguments = []
                                                                        }
                                                                }
                                                            }
                                                        }
                                                , declaration =
                                                    { range = { start = { row = 8, column = 1 }, end = { row = 8, column = 8 } }
                                                    , value =
                                                        { name =
                                                            { range = { start = { row = 8, column = 1 }, end = { row = 8, column = 4 } }, value = "bar" }
                                                        , parameters = []
                                                        , expression =
                                                            { range = { start = { row = 8, column = 7 }, end = { row = 8, column = 8 } }
                                                            , value = GrenSyntax.ExpressionInteger 1
                                                            }
                                                        }
                                                    }
                                                }
                                      }
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
                                    { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                    , value =
                                        GrenSyntax.NormalModule
                                            { moduleName =
                                                { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }, value = [ "Bar" ] }
                                            , exposingList =
                                                { range = { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } }
                                                , value = GrenSyntax.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                                                }
                                            }
                                    }
                                , imports = []
                                , declarations =
                                    [ { range = { start = { row = 4, column = 1 }, end = { row = 4, column = 8 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { documentation = Nothing
                                                , signature = Nothing
                                                , declaration =
                                                    { range = { start = { row = 4, column = 1 }, end = { row = 4, column = 8 } }
                                                    , value =
                                                        { name =
                                                            { range = { start = { row = 4, column = 1 }, end = { row = 4, column = 4 } }, value = "bar" }
                                                        , parameters = []
                                                        , expression =
                                                            { range = { start = { row = 4, column = 7 }, end = { row = 4, column = 8 } }
                                                            , value = GrenSyntax.ExpressionInteger 1
                                                            }
                                                        }
                                                    }
                                                }
                                      }
                                    ]
                                , comments = [ { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 10 } }, value = "--The Doc" } ]
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
                                    { range = { start = { row = 2, column = 1 }, end = { row = 2, column = 25 } }
                                    , value =
                                        GrenSyntax.NormalModule
                                            { moduleName =
                                                { range = { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } }, value = [ "Bar" ] }
                                            , exposingList =
                                                { range = { start = { row = 2, column = 12 }, end = { row = 2, column = 25 } }
                                                , value = GrenSyntax.All { start = { row = 2, column = 22 }, end = { row = 2, column = 24 } }
                                                }
                                            }
                                    }
                                , imports = []
                                , declarations =
                                    [ { range = { start = { row = 5, column = 1 }, end = { row = 5, column = 24 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { documentation = Nothing
                                                , signature = Nothing
                                                , declaration =
                                                    { range = { start = { row = 5, column = 1 }, end = { row = 5, column = 24 } }
                                                    , value =
                                                        { name =
                                                            { range = { start = { row = 5, column = 1 }, end = { row = 5, column = 4 } }, value = "bar" }
                                                        , parameters = []
                                                        , expression =
                                                            { range = { start = { row = 5, column = 23 }, end = { row = 5, column = 24 } }
                                                            , value = GrenSyntax.ExpressionInteger 1
                                                            }
                                                        }
                                                    }
                                                }
                                      }
                                    ]
                                , comments =
                                    [ { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                                      , value = "-- comment 1"
                                      }
                                    , { range = { start = { row = 4, column = 1 }, end = { row = 4, column = 13 } }
                                      , value = "-- comment 2"
                                      }
                                    , { range = { start = { row = 5, column = 7 }, end = { row = 5, column = 22 } }
                                      , value = "{- comment 3 -}"
                                      }
                                    , { range = { start = { row = 5, column = 25 }, end = { row = 5, column = 37 } }
                                      , value = "-- comment 4"
                                      }
                                    , { range = { start = { row = 6, column = 2 }, end = { row = 6, column = 14 } }
                                      , value = "-- comment 5"
                                      }
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
                                    { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                    , value =
                                        GrenSyntax.NormalModule
                                            { moduleName =
                                                { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }, value = [ "Bar" ] }
                                            , exposingList =
                                                { range = { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } }
                                                , value = GrenSyntax.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                                                }
                                            }
                                    }
                                , imports = []
                                , declarations =
                                    [ { range = { start = { row = 4, column = 1 }, end = { row = 4, column = 8 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { documentation = Nothing
                                                , signature = Nothing
                                                , declaration =
                                                    { range = { start = { row = 4, column = 1 }, end = { row = 4, column = 8 } }
                                                    , value =
                                                        { name =
                                                            { range = { start = { row = 4, column = 1 }, end = { row = 4, column = 4 } }, value = "bar" }
                                                        , parameters = []
                                                        , expression =
                                                            { range = { start = { row = 4, column = 7 }, end = { row = 4, column = 8 } }
                                                            , value = GrenSyntax.ExpressionInteger 1
                                                            }
                                                        }
                                                    }
                                                }
                                      }
                                    ]
                                , comments =
                                    [ { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 14 } }
                                      , value = "{- The Doc -}"
                                      }
                                    ]
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
                                    { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                    , value =
                                        GrenSyntax.NormalModule
                                            { moduleName =
                                                { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }, value = [ "Bar" ] }
                                            , exposingList =
                                                { range = { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } }
                                                , value = GrenSyntax.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                                                }
                                            }
                                    }
                                , imports =
                                    [ { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 14 } }
                                      , value =
                                            { moduleName =
                                                { range = { start = { row = 3, column = 8 }, end = { row = 3, column = 14 } }
                                                , value = [ "String" ]
                                                }
                                            , moduleAlias = Nothing
                                            , exposingList = Nothing
                                            }
                                      }
                                    ]
                                , declarations =
                                    [ { range = { start = { row = 5, column = 1 }, end = { row = 7, column = 23 } }
                                      , value =
                                            GrenSyntax.AliasDeclaration
                                                { documentation =
                                                    Just
                                                        { range = { start = { row = 5, column = 1 }, end = { row = 5, column = 15 } }
                                                        , value = "{-| The Doc -}"
                                                        }
                                                , name =
                                                    { range = { start = { row = 6, column = 12 }, end = { row = 6, column = 15 } }, value = "Foo" }
                                                , generics = []
                                                , typeAnnotation =
                                                    { range = { start = { row = 7, column = 6 }, end = { row = 7, column = 23 } }
                                                    , value =
                                                        GrenSyntax.TypeAnnotationRecord
                                                            [ { range = { start = { row = 7, column = 8 }, end = { row = 7, column = 21 } }
                                                              , value =
                                                                    { name =
                                                                        { range = { start = { row = 7, column = 8 }, end = { row = 7, column = 12 } }, value = "name" }
                                                                    , value =
                                                                        { range = { start = { row = 7, column = 15 }, end = { row = 7, column = 21 } }
                                                                        , value =
                                                                            GrenSyntax.TypeAnnotationConstruct
                                                                                { reference =
                                                                                    { range = { start = { row = 7, column = 15 }, end = { row = 7, column = 21 } }
                                                                                    , value = { qualification = [], name = "String" }
                                                                                    }
                                                                                , arguments = []
                                                                                }
                                                                        }
                                                                    }
                                                              }
                                                            ]
                                                    }
                                                }
                                      }
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
                                { moduleDefinition =
                                    { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                    , value =
                                        GrenSyntax.NormalModule
                                            { moduleName =
                                                { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }, value = [ "Bar" ] }
                                            , exposingList =
                                                { range = { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } }
                                                , value = GrenSyntax.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                                                }
                                            }
                                    }
                                , imports =
                                    [ { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 14 } }
                                      , value =
                                            { moduleName =
                                                { range = { start = { row = 3, column = 8 }, end = { row = 3, column = 14 } }
                                                , value = [ "String" ]
                                                }
                                            , moduleAlias = Nothing
                                            , exposingList = Nothing
                                            }
                                      }
                                    ]
                                , declarations =
                                    [ { range = { start = { row = 5, column = 1 }, end = { row = 8, column = 10 } }
                                      , value =
                                            GrenSyntax.ChoiceTypeDeclaration
                                                { documentation =
                                                    Just
                                                        { range = { start = { row = 5, column = 1 }, end = { row = 5, column = 15 } }
                                                        , value = "{-| The Doc -}"
                                                        }
                                                , name =
                                                    { range = { start = { row = 6, column = 6 }, end = { row = 6, column = 9 } }, value = "Foo" }
                                                , generics = []
                                                , constructors =
                                                    [ { range = { start = { row = 7, column = 6 }, end = { row = 7, column = 9 } }
                                                      , value =
                                                            { name =
                                                                { range = { start = { row = 7, column = 6 }, end = { row = 7, column = 9 } }, value = "Red" }
                                                            , value = Nothing
                                                            }
                                                      }
                                                    , { range = { start = { row = 8, column = 6 }, end = { row = 8, column = 10 } }
                                                      , value =
                                                            { name =
                                                                { range = { start = { row = 8, column = 6 }, end = { row = 8, column = 10 } }, value = "Blue" }
                                                            , value = Nothing
                                                            }
                                                      }
                                                    ]
                                                }
                                      }
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
                                    { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 42 } }
                                    , value =
                                        GrenSyntax.NormalModule
                                            { moduleName =
                                                { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 27 } }
                                                , value = [ "Simplify", "AstHelpers" ]
                                                }
                                            , exposingList =
                                                { range = { start = { row = 1, column = 28 }, end = { row = 1, column = 42 } }
                                                , value =
                                                    GrenSyntax.Explicit
                                                        [ { range = { start = { row = 1, column = 38 }, end = { row = 1, column = 41 } }
                                                          , value = GrenSyntax.FunctionExpose "log"
                                                          }
                                                        ]
                                                }
                                            }
                                    }
                                , imports = []
                                , declarations =
                                    [ { range = { start = { row = 4, column = 1 }, end = { row = 6, column = 21 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { documentation = Nothing
                                                , signature =
                                                    Just
                                                        { range = { start = { row = 4, column = 1 }, end = { row = 4, column = 17 } }
                                                        , value =
                                                            { name =
                                                                { range = { start = { row = 4, column = 1 }, end = { row = 4, column = 4 } }, value = "log" }
                                                            , typeAnnotation =
                                                                { range = { start = { row = 4, column = 7 }, end = { row = 4, column = 17 } }
                                                                , value =
                                                                    GrenSyntax.TypeAnnotationFunction
                                                                        { input =
                                                                            { range = { start = { row = 4, column = 7 }, end = { row = 4, column = 10 } }
                                                                            , value =
                                                                                GrenSyntax.TypeAnnotationConstruct
                                                                                    { reference =
                                                                                        { range = { start = { row = 4, column = 7 }, end = { row = 4, column = 10 } }
                                                                                        , value = { qualification = [], name = "Int" }
                                                                                        }
                                                                                    , arguments = []
                                                                                    }
                                                                            }
                                                                        , output =
                                                                            { range = { start = { row = 4, column = 14 }, end = { row = 4, column = 17 } }
                                                                            , value =
                                                                                GrenSyntax.TypeAnnotationConstruct
                                                                                    { reference =
                                                                                        { range = { start = { row = 4, column = 14 }, end = { row = 4, column = 17 } }
                                                                                        , value = { qualification = [], name = "Int" }
                                                                                        }
                                                                                    , arguments = []
                                                                                    }
                                                                            }
                                                                        }
                                                                }
                                                            }
                                                        }
                                                , declaration =
                                                    { range = { start = { row = 5, column = 1 }, end = { row = 6, column = 21 } }
                                                    , value =
                                                        { name =
                                                            { range = { start = { row = 5, column = 1 }, end = { row = 5, column = 4 } }, value = "log" }
                                                        , parameters =
                                                            [ { range = { start = { row = 5, column = 5 }, end = { row = 5, column = 6 } }
                                                              , value = GrenSyntax.PatternVariable "a"
                                                              }
                                                            ]
                                                        , expression =
                                                            { range = { start = { row = 6, column = 5 }, end = { row = 6, column = 21 } }
                                                            , value =
                                                                GrenSyntax.ExpressionCall
                                                                    [ { range = { start = { row = 6, column = 5 }, end = { row = 6, column = 14 } }
                                                                      , value = GrenSyntax.ExpressionReference { qualification = [ "Debug" ], name = "log" }
                                                                      }
                                                                    , { range = { start = { row = 6, column = 15 }, end = { row = 6, column = 19 } }
                                                                      , value = GrenSyntax.ExpressionString { quotingStyle = GrenSyntax.StringSingleQuoted, content = "ok" }
                                                                      }
                                                                    , { range = { start = { row = 6, column = 20 }, end = { row = 6, column = 21 } }
                                                                      , value = GrenSyntax.ExpressionReference { qualification = [], name = "a" }
                                                                      }
                                                                    ]
                                                            }
                                                        }
                                                    }
                                                }
                                      }
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
                                    { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                    , value =
                                        GrenSyntax.NormalModule
                                            { moduleName =
                                                { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }, value = [ "Bar" ] }
                                            , exposingList =
                                                { range = { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } }
                                                , value = GrenSyntax.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                                                }
                                            }
                                    }
                                , imports = []
                                , declarations =
                                    [ { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 24 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { documentation = Nothing
                                                , signature = Nothing
                                                , declaration =
                                                    { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 24 } }
                                                    , value =
                                                        { name =
                                                            { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 4 } }, value = "bar" }
                                                        , parameters = []
                                                        , expression =
                                                            { range = { start = { row = 3, column = 7 }, end = { row = 3, column = 24 } }
                                                            , value =
                                                                GrenSyntax.ExpressionInfixOperation
                                                                    { operator = "*"
                                                                    , left =
                                                                        { range = { start = { row = 3, column = 7 }, end = { row = 3, column = 14 } }
                                                                        , value =
                                                                            GrenSyntax.ExpressionParenthesized
                                                                                { range = { start = { row = 3, column = 8 }, end = { row = 3, column = 13 } }
                                                                                , value =
                                                                                    GrenSyntax.ExpressionInfixOperation
                                                                                        { operator = "+"
                                                                                        , left =
                                                                                            { range = { start = { row = 3, column = 8 }, end = { row = 3, column = 9 } }
                                                                                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "x" }
                                                                                            }
                                                                                        , right =
                                                                                            { range = { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } }
                                                                                            , value = GrenSyntax.ExpressionInteger 1
                                                                                            }
                                                                                        }
                                                                                }
                                                                        }
                                                                    , right =
                                                                        { range = { start = { row = 3, column = 17 }, end = { row = 3, column = 24 } }
                                                                        , value =
                                                                            GrenSyntax.ExpressionParenthesized
                                                                                { range = { start = { row = 3, column = 18 }, end = { row = 3, column = 23 } }
                                                                                , value =
                                                                                    GrenSyntax.ExpressionInfixOperation
                                                                                        { operator = "*"
                                                                                        , left =
                                                                                            { range = { start = { row = 3, column = 18 }, end = { row = 3, column = 19 } }
                                                                                            , value = GrenSyntax.ExpressionInteger 2
                                                                                            }
                                                                                        , right =
                                                                                            { range = { start = { row = 3, column = 22 }, end = { row = 3, column = 23 } }
                                                                                            , value = GrenSyntax.ExpressionReference { qualification = [], name = "y" }
                                                                                            }
                                                                                        }
                                                                                }
                                                                        }
                                                                    }
                                                            }
                                                        }
                                                    }
                                                }
                                      }
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
                                    { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                    , value =
                                        GrenSyntax.NormalModule
                                            { moduleName =
                                                { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }, value = [ "Bar" ] }
                                            , exposingList =
                                                { range = { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } }
                                                , value = GrenSyntax.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                                                }
                                            }
                                    }
                                , imports = []
                                , declarations =
                                    [ { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 16 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { documentation = Nothing
                                                , signature = Nothing
                                                , declaration =
                                                    { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 16 } }
                                                    , value =
                                                        { name =
                                                            { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 4 } }, value = "bar" }
                                                        , parameters = []
                                                        , expression =
                                                            { range = { start = { row = 3, column = 7 }, end = { row = 3, column = 16 } }
                                                            , value =
                                                                GrenSyntax.ExpressionInfixOperation
                                                                    { operator = "+"
                                                                    , left =
                                                                        { range = { start = { row = 3, column = 7 }, end = { row = 3, column = 8 } }
                                                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "x" }
                                                                        }
                                                                    , right =
                                                                        { range = { start = { row = 3, column = 11 }, end = { row = 3, column = 16 } }
                                                                        , value =
                                                                            GrenSyntax.ExpressionInfixOperation
                                                                                { operator = "*"
                                                                                , left =
                                                                                    { range = { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } }
                                                                                    , value = GrenSyntax.ExpressionInteger 1
                                                                                    }
                                                                                , right =
                                                                                    { range = { start = { row = 3, column = 15 }, end = { row = 3, column = 16 } }
                                                                                    , value = GrenSyntax.ExpressionInteger 2
                                                                                    }
                                                                                }
                                                                        }
                                                                    }
                                                            }
                                                        }
                                                    }
                                                }
                                      }
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
                                    { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                    , value =
                                        GrenSyntax.NormalModule
                                            { moduleName =
                                                { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }, value = [ "Bar" ] }
                                            , exposingList =
                                                { range = { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } }
                                                , value = GrenSyntax.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                                                }
                                            }
                                    }
                                , imports = []
                                , declarations =
                                    [ { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 16 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { documentation = Nothing
                                                , signature = Nothing
                                                , declaration =
                                                    { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 16 } }
                                                    , value =
                                                        { name =
                                                            { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 4 } }, value = "bar" }
                                                        , parameters = []
                                                        , expression =
                                                            { range = { start = { row = 3, column = 7 }, end = { row = 3, column = 16 } }
                                                            , value =
                                                                GrenSyntax.ExpressionInfixOperation
                                                                    { operator = "+"
                                                                    , left =
                                                                        { range = { start = { row = 3, column = 7 }, end = { row = 3, column = 12 } }
                                                                        , value =
                                                                            GrenSyntax.ExpressionInfixOperation
                                                                                { operator = "*"
                                                                                , left =
                                                                                    { range = { start = { row = 3, column = 7 }, end = { row = 3, column = 8 } }
                                                                                    , value = GrenSyntax.ExpressionReference { qualification = [], name = "x" }
                                                                                    }
                                                                                , right =
                                                                                    { range = { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } }
                                                                                    , value = GrenSyntax.ExpressionInteger 1
                                                                                    }
                                                                                }
                                                                        }
                                                                    , right =
                                                                        { range = { start = { row = 3, column = 15 }, end = { row = 3, column = 16 } }
                                                                        , value = GrenSyntax.ExpressionInteger 2
                                                                        }
                                                                    }
                                                            }
                                                        }
                                                    }
                                                }
                                      }
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
                                    { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                    , value =
                                        GrenSyntax.NormalModule
                                            { moduleName =
                                                { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }, value = [ "Bar" ] }
                                            , exposingList =
                                                { range = { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } }
                                                , value = GrenSyntax.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                                                }
                                            }
                                    }
                                , imports = []
                                , declarations =
                                    [ { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 15 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { documentation = Nothing
                                                , signature = Nothing
                                                , declaration =
                                                    { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 15 } }
                                                    , value =
                                                        { name =
                                                            { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 4 } }, value = "bar" }
                                                        , parameters = []
                                                        , expression =
                                                            { range = { start = { row = 3, column = 7 }, end = { row = 3, column = 15 } }
                                                            , value =
                                                                GrenSyntax.ExpressionNegation
                                                                    { range = { start = { row = 3, column = 8 }, end = { row = 3, column = 15 } }
                                                                    , value =
                                                                        GrenSyntax.ExpressionParenthesized
                                                                            { range = { start = { row = 3, column = 9 }, end = { row = 3, column = 14 } }
                                                                            , value =
                                                                                GrenSyntax.ExpressionInfixOperation
                                                                                    { operator = "*"
                                                                                    , left =
                                                                                        { range = { start = { row = 3, column = 9 }, end = { row = 3, column = 10 } }
                                                                                        , value = GrenSyntax.ExpressionInteger 1
                                                                                        }
                                                                                    , right =
                                                                                        { range = { start = { row = 3, column = 13 }, end = { row = 3, column = 14 } }
                                                                                        , value = GrenSyntax.ExpressionInteger 2
                                                                                        }
                                                                                    }
                                                                            }
                                                                    }
                                                            }
                                                        }
                                                    }
                                                }
                                      }
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
                                    { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                                    , value =
                                        GrenSyntax.NormalModule
                                            { moduleName =
                                                { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }, value = [ "Bar" ] }
                                            , exposingList =
                                                { range = { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } }
                                                , value = GrenSyntax.All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } }
                                                }
                                            }
                                    }
                                , imports = []
                                , declarations =
                                    [ { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 16 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { documentation = Nothing
                                                , signature = Nothing
                                                , declaration =
                                                    { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 16 } }
                                                    , value =
                                                        { name =
                                                            { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 4 } }, value = "bar" }
                                                        , parameters = []
                                                        , expression =
                                                            { range = { start = { row = 3, column = 7 }, end = { row = 3, column = 16 } }
                                                            , value =
                                                                GrenSyntax.ExpressionRecordAccess
                                                                    { record =
                                                                        { range = { start = { row = 3, column = 7 }, end = { row = 3, column = 14 } }
                                                                        , value =
                                                                            GrenSyntax.ExpressionParenthesized
                                                                                { range = { start = { row = 3, column = 8 }, end = { row = 3, column = 13 } }
                                                                                , value =
                                                                                    GrenSyntax.ExpressionInfixOperation
                                                                                        { operator = "*"
                                                                                        , left =
                                                                                            { range = { start = { row = 3, column = 8 }, end = { row = 3, column = 9 } }
                                                                                            , value = GrenSyntax.ExpressionInteger 1
                                                                                            }
                                                                                        , right =
                                                                                            { range = { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } }
                                                                                            , value = GrenSyntax.ExpressionInteger 2
                                                                                            }
                                                                                        }
                                                                                }
                                                                        }
                                                                    , field =
                                                                        { range = { start = { row = 3, column = 15 }, end = { row = 3, column = 16 } }
                                                                        , value = "x"
                                                                        }
                                                                    }
                                                            }
                                                        }
                                                    }
                                                }
                                      }
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
                                    { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 23 } }
                                    , value =
                                        GrenSyntax.NormalModule
                                            { moduleName =
                                                { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } }, value = [ "A" ] }
                                            , exposingList =
                                                { range = { start = { row = 1, column = 10 }, end = { row = 1, column = 23 } }
                                                , value = GrenSyntax.All { start = { row = 1, column = 20 }, end = { row = 1, column = 22 } }
                                                }
                                            }
                                    }
                                , imports = []
                                , declarations =
                                    [ { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 29 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { documentation = Nothing
                                                , signature = Nothing
                                                , declaration =
                                                    { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 29 } }
                                                    , value =
                                                        { name =
                                                            { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 6 } }, value = "bool1" }
                                                        , parameters = []
                                                        , expression =
                                                            { range = { start = { row = 3, column = 9 }, end = { row = 3, column = 29 } }
                                                            , value =
                                                                GrenSyntax.ExpressionInfixOperation
                                                                    { operator = "||"
                                                                    , left =
                                                                        { range = { start = { row = 3, column = 9 }, end = { row = 3, column = 21 } }
                                                                        , value =
                                                                            GrenSyntax.ExpressionInfixOperation
                                                                                { operator = "&&"
                                                                                , left =
                                                                                    { range = { start = { row = 3, column = 9 }, end = { row = 3, column = 13 } }
                                                                                    , value = GrenSyntax.ExpressionReference { qualification = [], name = "True" }
                                                                                    }
                                                                                , right =
                                                                                    { range = { start = { row = 3, column = 17 }, end = { row = 3, column = 21 } }
                                                                                    , value = GrenSyntax.ExpressionReference { qualification = [], name = "True" }
                                                                                    }
                                                                                }
                                                                        }
                                                                    , right =
                                                                        { range = { start = { row = 3, column = 25 }, end = { row = 3, column = 29 } }
                                                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "True" }
                                                                        }
                                                                    }
                                                            }
                                                        }
                                                    }
                                                }
                                      }
                                    , { range = { start = { row = 4, column = 1 }, end = { row = 4, column = 29 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { documentation = Nothing
                                                , signature = Nothing
                                                , declaration =
                                                    { range = { start = { row = 4, column = 1 }, end = { row = 4, column = 29 } }
                                                    , value =
                                                        { name =
                                                            { range = { start = { row = 4, column = 1 }, end = { row = 4, column = 6 } }, value = "bool2" }
                                                        , parameters = []
                                                        , expression =
                                                            { range = { start = { row = 4, column = 9 }, end = { row = 4, column = 29 } }
                                                            , value =
                                                                GrenSyntax.ExpressionInfixOperation
                                                                    { operator = "||"
                                                                    , left =
                                                                        { range = { start = { row = 4, column = 9 }, end = { row = 4, column = 13 } }
                                                                        , value = GrenSyntax.ExpressionReference { qualification = [], name = "True" }
                                                                        }
                                                                    , right =
                                                                        { range = { start = { row = 4, column = 17 }, end = { row = 4, column = 29 } }
                                                                        , value =
                                                                            GrenSyntax.ExpressionInfixOperation
                                                                                { operator = "&&"
                                                                                , left =
                                                                                    { range = { start = { row = 4, column = 17 }, end = { row = 4, column = 21 } }
                                                                                    , value = GrenSyntax.ExpressionReference { qualification = [], name = "True" }
                                                                                    }
                                                                                , right =
                                                                                    { range = { start = { row = 4, column = 25 }, end = { row = 4, column = 29 } }
                                                                                    , value = GrenSyntax.ExpressionReference { qualification = [], name = "True" }
                                                                                    }
                                                                                }
                                                                        }
                                                                    }
                                                            }
                                                        }
                                                    }
                                                }
                                      }
                                    , { range = { start = { row = 6, column = 1 }, end = { row = 6, column = 25 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { documentation = Nothing
                                                , signature = Nothing
                                                , declaration =
                                                    { range = { start = { row = 6, column = 1 }, end = { row = 6, column = 25 } }
                                                    , value =
                                                        { name =
                                                            { range = { start = { row = 6, column = 1 }, end = { row = 6, column = 9 } }, value = "numeric1" }
                                                        , parameters = []
                                                        , expression =
                                                            { range = { start = { row = 6, column = 12 }, end = { row = 6, column = 25 } }
                                                            , value =
                                                                GrenSyntax.ExpressionInfixOperation
                                                                    { operator = "+"
                                                                    , left =
                                                                        { range = { start = { row = 6, column = 12 }, end = { row = 6, column = 21 } }
                                                                        , value =
                                                                            GrenSyntax.ExpressionInfixOperation
                                                                                { operator = "*"
                                                                                , left =
                                                                                    { range = { start = { row = 6, column = 12 }, end = { row = 6, column = 17 } }
                                                                                    , value =
                                                                                        GrenSyntax.ExpressionInfixOperation
                                                                                            { operator = "^"
                                                                                            , left =
                                                                                                { range = { start = { row = 6, column = 12 }, end = { row = 6, column = 13 } }
                                                                                                , value = GrenSyntax.ExpressionInteger 1
                                                                                                }
                                                                                            , right =
                                                                                                { range = { start = { row = 6, column = 16 }, end = { row = 6, column = 17 } }
                                                                                                , value = GrenSyntax.ExpressionInteger 2
                                                                                                }
                                                                                            }
                                                                                    }
                                                                                , right =
                                                                                    { range = { start = { row = 6, column = 20 }, end = { row = 6, column = 21 } }
                                                                                    , value = GrenSyntax.ExpressionInteger 3
                                                                                    }
                                                                                }
                                                                        }
                                                                    , right =
                                                                        { range = { start = { row = 6, column = 24 }, end = { row = 6, column = 25 } }
                                                                        , value = GrenSyntax.ExpressionInteger 4
                                                                        }
                                                                    }
                                                            }
                                                        }
                                                    }
                                                }
                                      }
                                    , { range = { start = { row = 7, column = 1 }, end = { row = 7, column = 25 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { documentation = Nothing
                                                , signature = Nothing
                                                , declaration =
                                                    { range = { start = { row = 7, column = 1 }, end = { row = 7, column = 25 } }
                                                    , value =
                                                        { name =
                                                            { range = { start = { row = 7, column = 1 }, end = { row = 7, column = 9 } }, value = "numeric2" }
                                                        , parameters = []
                                                        , expression =
                                                            { range = { start = { row = 7, column = 12 }, end = { row = 7, column = 25 } }
                                                            , value =
                                                                GrenSyntax.ExpressionInfixOperation
                                                                    { operator = "+"
                                                                    , left =
                                                                        { range = { start = { row = 7, column = 12 }, end = { row = 7, column = 13 } }
                                                                        , value = GrenSyntax.ExpressionInteger 1
                                                                        }
                                                                    , right =
                                                                        { range = { start = { row = 7, column = 16 }, end = { row = 7, column = 25 } }
                                                                        , value =
                                                                            GrenSyntax.ExpressionInfixOperation
                                                                                { operator = "*"
                                                                                , left =
                                                                                    { range = { start = { row = 7, column = 16 }, end = { row = 7, column = 17 } }
                                                                                    , value = GrenSyntax.ExpressionInteger 2
                                                                                    }
                                                                                , right =
                                                                                    { range = { start = { row = 7, column = 20 }, end = { row = 7, column = 25 } }
                                                                                    , value =
                                                                                        GrenSyntax.ExpressionInfixOperation
                                                                                            { operator = "^"
                                                                                            , left =
                                                                                                { range = { start = { row = 7, column = 20 }, end = { row = 7, column = 21 } }
                                                                                                , value = GrenSyntax.ExpressionInteger 3
                                                                                                }
                                                                                            , right =
                                                                                                { range = { start = { row = 7, column = 24 }, end = { row = 7, column = 25 } }
                                                                                                , value = GrenSyntax.ExpressionInteger 4
                                                                                                }
                                                                                            }
                                                                                    }
                                                                                }
                                                                        }
                                                                    }
                                                            }
                                                        }
                                                    }
                                                }
                                      }
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
                                    { range = { start = { row = 1, column = 1 }, end = { row = 1, column = 23 } }
                                    , value =
                                        GrenSyntax.NormalModule
                                            { moduleName =
                                                { range = { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } }, value = [ "A" ] }
                                            , exposingList =
                                                { range = { start = { row = 1, column = 10 }, end = { row = 1, column = 23 } }
                                                , value = GrenSyntax.All { start = { row = 1, column = 20 }, end = { row = 1, column = 22 } }
                                                }
                                            }
                                    }
                                , imports = []
                                , declarations =
                                    [ { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 21 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { documentation = Nothing
                                                , signature = Nothing
                                                , declaration =
                                                    { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 21 } }
                                                    , value =
                                                        { name =
                                                            { range = { start = { row = 3, column = 1 }, end = { row = 3, column = 9 } }, value = "numeric1" }
                                                        , parameters = []
                                                        , expression =
                                                            { range = { start = { row = 3, column = 12 }, end = { row = 3, column = 21 } }
                                                            , value =
                                                                GrenSyntax.ExpressionInfixOperation
                                                                    { operator = "-"
                                                                    , left =
                                                                        { range = { start = { row = 3, column = 12 }, end = { row = 3, column = 17 } }
                                                                        , value =
                                                                            GrenSyntax.ExpressionInfixOperation
                                                                                { operator = "+"
                                                                                , left =
                                                                                    { range = { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } }
                                                                                    , value = GrenSyntax.ExpressionInteger 1
                                                                                    }
                                                                                , right =
                                                                                    { range = { start = { row = 3, column = 16 }, end = { row = 3, column = 17 } }
                                                                                    , value = GrenSyntax.ExpressionInteger 2
                                                                                    }
                                                                                }
                                                                        }
                                                                    , right =
                                                                        { range = { start = { row = 3, column = 20 }, end = { row = 3, column = 21 } }
                                                                        , value = GrenSyntax.ExpressionInteger 3
                                                                        }
                                                                    }
                                                            }
                                                        }
                                                    }
                                                }
                                      }
                                    , { range = { start = { row = 5, column = 1 }, end = { row = 5, column = 24 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { documentation = Nothing
                                                , signature = Nothing
                                                , declaration =
                                                    { range = { start = { row = 5, column = 1 }, end = { row = 5, column = 24 } }
                                                    , value =
                                                        { name =
                                                            { range = { start = { row = 5, column = 1 }, end = { row = 5, column = 10 } }, value = "pipeline1" }
                                                        , parameters = []
                                                        , expression =
                                                            { range = { start = { row = 5, column = 13 }, end = { row = 5, column = 24 } }
                                                            , value =
                                                                GrenSyntax.ExpressionInfixOperation
                                                                    { operator = "|>"
                                                                    , left =
                                                                        { range = { start = { row = 5, column = 13 }, end = { row = 5, column = 19 } }
                                                                        , value =
                                                                            GrenSyntax.ExpressionInfixOperation
                                                                                { operator = "|>"
                                                                                , left =
                                                                                    { range = { start = { row = 5, column = 13 }, end = { row = 5, column = 14 } }
                                                                                    , value = GrenSyntax.ExpressionInteger 1
                                                                                    }
                                                                                , right =
                                                                                    { range = { start = { row = 5, column = 18 }, end = { row = 5, column = 19 } }
                                                                                    , value = GrenSyntax.ExpressionInteger 2
                                                                                    }
                                                                                }
                                                                        }
                                                                    , right =
                                                                        { range = { start = { row = 5, column = 23 }, end = { row = 5, column = 24 } }
                                                                        , value = GrenSyntax.ExpressionInteger 3
                                                                        }
                                                                    }
                                                            }
                                                        }
                                                    }
                                                }
                                      }
                                    ]
                                , comments = []
                                }
                            )
                )
            , Test.test "/= is equivalent to !="
                (\() ->
                    """
module A exposing (..)

pipeline0 = 1 /= 2

pipeline1 = 1 != 2
"""
                        |> String.trim
                        |> GrenParserLenient.run GrenParserLenient.module_
                        |> Expect.equal
                            (Just
                                { comments = []
                                , declarations =
                                    [ { range = { end = { column = 19, row = 3 }, start = { column = 1, row = 3 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { declaration =
                                                    { range = { end = { column = 19, row = 3 }, start = { column = 1, row = 3 } }
                                                    , value =
                                                        { parameters = []
                                                        , expression =
                                                            { range = { end = { column = 19, row = 3 }, start = { column = 13, row = 3 } }
                                                            , value =
                                                                GrenSyntax.ExpressionInfixOperation
                                                                    { operator = "!="
                                                                    , left =
                                                                        { range = { end = { column = 14, row = 3 }, start = { column = 13, row = 3 } }
                                                                        , value = GrenSyntax.ExpressionInteger 1
                                                                        }
                                                                    , right =
                                                                        { range = { end = { column = 19, row = 3 }, start = { column = 18, row = 3 } }
                                                                        , value = GrenSyntax.ExpressionInteger 2
                                                                        }
                                                                    }
                                                            }
                                                        , name =
                                                            { range = { end = { column = 10, row = 3 }, start = { column = 1, row = 3 } }, value = "pipeline0" }
                                                        }
                                                    }
                                                , documentation = Nothing
                                                , signature = Nothing
                                                }
                                      }
                                    , { range = { end = { column = 19, row = 5 }, start = { column = 1, row = 5 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { declaration =
                                                    { range = { end = { column = 19, row = 5 }, start = { column = 1, row = 5 } }
                                                    , value =
                                                        { parameters = []
                                                        , expression =
                                                            { range = { end = { column = 19, row = 5 }, start = { column = 13, row = 5 } }
                                                            , value =
                                                                GrenSyntax.ExpressionInfixOperation
                                                                    { operator = "!="
                                                                    , left =
                                                                        { range = { end = { column = 14, row = 5 }, start = { column = 13, row = 5 } }
                                                                        , value = GrenSyntax.ExpressionInteger 1
                                                                        }
                                                                    , right =
                                                                        { range = { end = { column = 19, row = 5 }, start = { column = 18, row = 5 } }
                                                                        , value = GrenSyntax.ExpressionInteger 2
                                                                        }
                                                                    }
                                                            }
                                                        , name =
                                                            { range = { end = { column = 10, row = 5 }, start = { column = 1, row = 5 } }, value = "pipeline1" }
                                                        }
                                                    }
                                                , documentation = Nothing
                                                , signature = Nothing
                                                }
                                      }
                                    ]
                                , imports = []
                                , moduleDefinition =
                                    { range = { end = { column = 23, row = 1 }, start = { column = 1, row = 1 } }
                                    , value =
                                        GrenSyntax.NormalModule
                                            { exposingList =
                                                { range = { end = { column = 23, row = 1 }, start = { column = 10, row = 1 } }
                                                , value = GrenSyntax.All { end = { column = 22, row = 1 }, start = { column = 20, row = 1 } }
                                                }
                                            , moduleName =
                                                { range = { end = { column = 9, row = 1 }, start = { column = 8, row = 1 } }, value = [ "A" ] }
                                            }
                                    }
                                }
                            )
                )
            , Test.test "!== is equivalent to !="
                (\() ->
                    """
module A exposing (..)

pipeline0 = 1 !== 2

pipeline1 = 1 != 2
"""
                        |> String.trim
                        |> GrenParserLenient.run GrenParserLenient.module_
                        |> Expect.equal
                            (Just
                                { comments = []
                                , declarations =
                                    [ { range = { end = { column = 20, row = 3 }, start = { column = 1, row = 3 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { declaration =
                                                    { range = { end = { column = 20, row = 3 }, start = { column = 1, row = 3 } }
                                                    , value =
                                                        { parameters = []
                                                        , expression =
                                                            { range = { end = { column = 20, row = 3 }, start = { column = 13, row = 3 } }
                                                            , value =
                                                                GrenSyntax.ExpressionInfixOperation
                                                                    { operator = "!="
                                                                    , left =
                                                                        { range = { end = { column = 14, row = 3 }, start = { column = 13, row = 3 } }
                                                                        , value = GrenSyntax.ExpressionInteger 1
                                                                        }
                                                                    , right =
                                                                        { range = { end = { column = 20, row = 3 }, start = { column = 19, row = 3 } }
                                                                        , value = GrenSyntax.ExpressionInteger 2
                                                                        }
                                                                    }
                                                            }
                                                        , name =
                                                            { range = { end = { column = 10, row = 3 }, start = { column = 1, row = 3 } }, value = "pipeline0" }
                                                        }
                                                    }
                                                , documentation = Nothing
                                                , signature = Nothing
                                                }
                                      }
                                    , { range = { end = { column = 19, row = 5 }, start = { column = 1, row = 5 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { declaration =
                                                    { range = { end = { column = 19, row = 5 }, start = { column = 1, row = 5 } }
                                                    , value =
                                                        { parameters = []
                                                        , expression =
                                                            { range = { end = { column = 19, row = 5 }, start = { column = 13, row = 5 } }
                                                            , value =
                                                                GrenSyntax.ExpressionInfixOperation
                                                                    { operator = "!="
                                                                    , left =
                                                                        { range = { end = { column = 14, row = 5 }, start = { column = 13, row = 5 } }
                                                                        , value = GrenSyntax.ExpressionInteger 1
                                                                        }
                                                                    , right =
                                                                        { range = { end = { column = 19, row = 5 }, start = { column = 18, row = 5 } }
                                                                        , value = GrenSyntax.ExpressionInteger 2
                                                                        }
                                                                    }
                                                            }
                                                        , name =
                                                            { range = { end = { column = 10, row = 5 }, start = { column = 1, row = 5 } }, value = "pipeline1" }
                                                        }
                                                    }
                                                , documentation = Nothing
                                                , signature = Nothing
                                                }
                                      }
                                    ]
                                , imports = []
                                , moduleDefinition =
                                    { range = { end = { column = 23, row = 1 }, start = { column = 1, row = 1 } }
                                    , value =
                                        GrenSyntax.NormalModule
                                            { exposingList =
                                                { range = { end = { column = 23, row = 1 }, start = { column = 10, row = 1 } }
                                                , value = GrenSyntax.All { end = { column = 22, row = 1 }, start = { column = 20, row = 1 } }
                                                }
                                            , moduleName =
                                                { range = { end = { column = 9, row = 1 }, start = { column = 8, row = 1 } }, value = [ "A" ] }
                                            }
                                    }
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
                                    [ { range = { end = { column = 20, row = 3 }, start = { column = 1, row = 3 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { declaration =
                                                    { range = { end = { column = 20, row = 3 }, start = { column = 1, row = 3 } }
                                                    , value =
                                                        { parameters = []
                                                        , expression =
                                                            { range = { end = { column = 20, row = 3 }, start = { column = 13, row = 3 } }
                                                            , value =
                                                                GrenSyntax.ExpressionInfixOperation
                                                                    { operator = "=="
                                                                    , left =
                                                                        { range = { end = { column = 14, row = 3 }, start = { column = 13, row = 3 } }
                                                                        , value = GrenSyntax.ExpressionInteger 1
                                                                        }
                                                                    , right =
                                                                        { range = { end = { column = 20, row = 3 }, start = { column = 19, row = 3 } }
                                                                        , value = GrenSyntax.ExpressionInteger 2
                                                                        }
                                                                    }
                                                            }
                                                        , name =
                                                            { range = { end = { column = 10, row = 3 }, start = { column = 1, row = 3 } }, value = "pipeline0" }
                                                        }
                                                    }
                                                , documentation = Nothing
                                                , signature = Nothing
                                                }
                                      }
                                    , { range = { end = { column = 19, row = 5 }, start = { column = 1, row = 5 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { declaration =
                                                    { range = { end = { column = 19, row = 5 }, start = { column = 1, row = 5 } }
                                                    , value =
                                                        { parameters = []
                                                        , expression =
                                                            { range = { end = { column = 19, row = 5 }, start = { column = 13, row = 5 } }
                                                            , value =
                                                                GrenSyntax.ExpressionInfixOperation
                                                                    { operator = "=="
                                                                    , left =
                                                                        { range = { end = { column = 14, row = 5 }, start = { column = 13, row = 5 } }
                                                                        , value = GrenSyntax.ExpressionInteger 1
                                                                        }
                                                                    , right =
                                                                        { range = { end = { column = 19, row = 5 }, start = { column = 18, row = 5 } }
                                                                        , value = GrenSyntax.ExpressionInteger 2
                                                                        }
                                                                    }
                                                            }
                                                        , name =
                                                            { range = { end = { column = 10, row = 5 }, start = { column = 1, row = 5 } }, value = "pipeline1" }
                                                        }
                                                    }
                                                , documentation = Nothing
                                                , signature = Nothing
                                                }
                                      }
                                    ]
                                , imports = []
                                , moduleDefinition =
                                    { range = { end = { column = 23, row = 1 }, start = { column = 1, row = 1 } }
                                    , value =
                                        GrenSyntax.NormalModule
                                            { exposingList =
                                                { range = { end = { column = 23, row = 1 }, start = { column = 10, row = 1 } }
                                                , value = GrenSyntax.All { end = { column = 22, row = 1 }, start = { column = 20, row = 1 } }
                                                }
                                            , moduleName =
                                                { range = { end = { column = 9, row = 1 }, start = { column = 8, row = 1 } }, value = [ "A" ] }
                                            }
                                    }
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
                                    [ { range = { end = { column = 19, row = 3 }, start = { column = 1, row = 3 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { declaration =
                                                    { range = { end = { column = 19, row = 3 }, start = { column = 1, row = 3 } }
                                                    , value =
                                                        { parameters = []
                                                        , expression =
                                                            { range = { end = { column = 19, row = 3 }, start = { column = 13, row = 3 } }
                                                            , value =
                                                                GrenSyntax.ExpressionInfixOperation
                                                                    { operator = "^"
                                                                    , left =
                                                                        { range = { end = { column = 14, row = 3 }, start = { column = 13, row = 3 } }
                                                                        , value = GrenSyntax.ExpressionInteger 1
                                                                        }
                                                                    , right =
                                                                        { range = { end = { column = 19, row = 3 }, start = { column = 18, row = 3 } }
                                                                        , value = GrenSyntax.ExpressionInteger 2
                                                                        }
                                                                    }
                                                            }
                                                        , name =
                                                            { range = { end = { column = 10, row = 3 }, start = { column = 1, row = 3 } }, value = "pipeline0" }
                                                        }
                                                    }
                                                , documentation = Nothing
                                                , signature = Nothing
                                                }
                                      }
                                    , { range = { end = { column = 18, row = 5 }, start = { column = 1, row = 5 } }
                                      , value =
                                            GrenSyntax.ValueOrFunctionDeclaration
                                                { declaration =
                                                    { range = { end = { column = 18, row = 5 }, start = { column = 1, row = 5 } }
                                                    , value =
                                                        { parameters = []
                                                        , expression =
                                                            { range = { end = { column = 18, row = 5 }, start = { column = 13, row = 5 } }
                                                            , value =
                                                                GrenSyntax.ExpressionInfixOperation
                                                                    { operator = "^"
                                                                    , left =
                                                                        { range = { end = { column = 14, row = 5 }, start = { column = 13, row = 5 } }
                                                                        , value = GrenSyntax.ExpressionInteger 1
                                                                        }
                                                                    , right =
                                                                        { range = { end = { column = 18, row = 5 }, start = { column = 17, row = 5 } }
                                                                        , value = GrenSyntax.ExpressionInteger 2
                                                                        }
                                                                    }
                                                            }
                                                        , name =
                                                            { range = { end = { column = 10, row = 5 }, start = { column = 1, row = 5 } }, value = "pipeline1" }
                                                        }
                                                    }
                                                , documentation = Nothing
                                                , signature = Nothing
                                                }
                                      }
                                    ]
                                , imports = []
                                , moduleDefinition =
                                    { range = { end = { column = 23, row = 1 }, start = { column = 1, row = 1 } }
                                    , value =
                                        GrenSyntax.NormalModule
                                            { exposingList =
                                                { range = { end = { column = 23, row = 1 }, start = { column = 10, row = 1 } }
                                                , value = GrenSyntax.All { end = { column = 22, row = 1 }, start = { column = 20, row = 1 } }
                                                }
                                            , moduleName =
                                                { range = { end = { column = 9, row = 1 }, start = { column = 8, row = 1 } }, value = [ "A" ] }
                                            }
                                    }
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
