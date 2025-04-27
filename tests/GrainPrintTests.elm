module GrainPrintTests exposing (suite)

import Expect
import GrenParserLenient
import GrenPrint
import Test exposing (Test)


suite : Test
suite =
    Test.describe "gren-format-unofficial"
        [ Test.describe "import"
            [ Test.test "only name, already same line"
                (\() ->
                    """module A exposing (..)
import List"""
                        |> expectPrintedAs
                            """module A exposing (..)

import List



"""
                )
            , Test.test "only name, multiline"
                (\() ->
                    """module A exposing (..)
import
    List"""
                        |> expectPrintedAs
                            """module A exposing (..)

import List



"""
                )
            , Test.test "name + alias, already same line"
                (\() ->
                    """module A exposing (..)
import List as CoreList"""
                        |> expectPrintedAs
                            """module A exposing (..)

import List as CoreList



"""
                )
            , Test.test "name + alias, multiline"
                (\() ->
                    """module A exposing (..)
import List
    as CoreList"""
                        |> expectPrintedAs
                            """module A exposing (..)

import List as CoreList



"""
                )
            , Test.test "name + alias + exposing all, already same line"
                (\() ->
                    """module A exposing (..)
import List as CoreList exposing (..)"""
                        |> expectPrintedAs
                            """module A exposing (..)

import List as CoreList exposing (..)



"""
                )
            , Test.test "name + alias + exposing all, multiline"
                (\() ->
                    """module A exposing (..)
import List
    as CoreList
        exposing (..
            )"""
                        |> expectPrintedAs
                            """module A exposing (..)

import List as CoreList exposing (..)



"""
                )
            , Test.test "name + alias + exposing one, already same line"
                (\() ->
                    """module A exposing (..)
import List as CoreList exposing (map)"""
                        |> expectPrintedAs
                            """module A exposing (..)

import List as CoreList exposing (map)



"""
                )
            , Test.test "name + alias + exposing one, multiline"
                -- This is in important difference between module exposing and import exposing.
                (\() ->
                    """module A exposing (..)
import List as CoreList
    exposing (map
    )"""
                        |> expectPrintedAs
                            """module A exposing (..)

import List as CoreList
    exposing
        ( map
        )



"""
                )
            , Test.test "name + alias + exposing multiple, already same line"
                (\() ->
                    """module A exposing (..)
import List as CoreList exposing (filter, map)"""
                        |> expectPrintedAs
                            """module A exposing (..)

import List as CoreList exposing (filter, map)



"""
                )
            , Test.test "name + alias + exposing multiple, multiline"
                (\() ->
                    """module A exposing (..)
import List as CoreList exposing (filter,
    map)"""
                        |> expectPrintedAs
                            """module A exposing (..)

import List as CoreList
    exposing
        ( filter
        , map
        )



"""
                )
            , Test.test "exposes get sorted"
                (\() ->
                    """module A exposing (..)
import List exposing (map, filter)"""
                        |> expectPrintedAs
                            """module A exposing (..)

import List exposing (filter, map)



"""
                )
            , Test.test "exposes get deduplicated"
                (\() ->
                    """module A exposing (..)
import List exposing (List, filter, map, filter, List)"""
                        |> expectPrintedAs
                            """module A exposing (..)

import List exposing (List, filter, map)



"""
                )
            , Test.test "open type exposes get deduplicated"
                (\() ->
                    """module A exposing (..)
import Maybe exposing (Maybe(..), map, Maybe)"""
                        |> expectPrintedAs
                            """module A exposing (..)

import Maybe exposing (Maybe(..), map)



"""
                )
            , Test.test "open type exposes get deduplicated across imports"
                (\() ->
                    """module A exposing (..)
import Maybe exposing (Maybe(..), map)
import Maybe exposing (Maybe)"""
                        |> expectPrintedAs
                            """module A exposing (..)

import Maybe exposing (Maybe(..), map)



"""
                )
            , Test.test "exposes get deduplicated across imports, preserving line offset of higher"
                (\() ->
                    """module A exposing (..)
import Maybe exposing (Maybe)
import Maybe exposing (Maybe(..),
    map)"""
                        |> expectPrintedAs
                            """module A exposing (..)

import Maybe
    exposing
        ( Maybe(..)
        , map
        )



"""
                )
            , Test.test "import aliases get deduplicated across imports"
                (\() ->
                    """module A exposing (..)
import List exposing (map)
import List as CoreList"""
                        |> expectPrintedAs
                            """module A exposing (..)

import List as CoreList exposing (map)



"""
                )
            , Test.test "imports get sorted"
                (\() ->
                    """module A exposing (..)
import A
import C
import B"""
                        |> expectPrintedAs
                            """module A exposing (..)

import A
import B
import C



"""
                )
            , Test.test "comments in imports (except in exposing list)"
                (\() ->
                    -- eating comments of earlier duplicate imports is a rudiment from elm-format
                    """module A exposing (..)
import -- -1
    A
import -- 0
    A -- 1
import B as
    -- 2
    B2
import C as C2
    -- 3
    exposing
    -- 4
    (..)
"""
                        |> expectPrintedAs
                            """module A exposing (..)

-- 1

import
    -- 0
    A
import
    B
        as
            -- 2
            B2
import C as C2
    exposing
        -- 3
        -- 4
        (..)



"""
                )
            ]
        , Test.describe "module header"
            [ Test.test "exposing all, already on same line"
                (\() ->
                    """module A exposing (..)
import Dummy"""
                        |> expectPrintedAs
                            """module A exposing (..)

import Dummy



"""
                )
            , Test.test "port exposing all, already on same line"
                (\() ->
                    """port module A exposing (..)
import Dummy"""
                        |> expectPrintedAs
                            """module A exposing (..)

import Dummy



"""
                )
            , Test.test "effect where command exposing all, already on same line"
                (\() ->
                    """effect module A where { command = MyCmd } exposing (..)
import Dummy"""
                        |> expectPrintedAs
                            """effect module A where { command = MyCmd } exposing (..)

import Dummy



"""
                )
            , Test.test "effect where subscription exposing all, already on same line"
                (\() ->
                    """effect module A where { subscription = MySub } exposing (..)
import Dummy"""
                        |> expectPrintedAs
                            """effect module A where { subscription = MySub } exposing (..)

import Dummy



"""
                )
            , Test.test "effect where command, subscription exposing all, already on same line"
                (\() ->
                    """effect module A where { command = MyCmd, subscription = MySub } exposing (..)
import Dummy"""
                        |> expectPrintedAs
                            """effect module A where { command = MyCmd, subscription = MySub } exposing (..)

import Dummy



"""
                )
            , Test.test "exposing all, multiline"
                (\() ->
                    """module A
    exposing (
        ..)
import Dummy"""
                        |> expectPrintedAs
                            """module A exposing (..)

import Dummy



"""
                )
            , Test.test "exposing one, already on same line"
                (\() ->
                    """module A exposing (a)
import Dummy"""
                        |> expectPrintedAs
                            """module A exposing (a)

import Dummy



"""
                )
            , Test.test "exposing one, multiline"
                (\() ->
                    """module A
    exposing (
        a)
import Dummy"""
                        |> expectPrintedAs
                            """module A exposing (a)

import Dummy



"""
                )
            , Test.test "exposing multiple, one line"
                (\() ->
                    """module A exposing ((||), B, C(..), a)
import Dummy"""
                        |> expectPrintedAs
                            """module A exposing ((||), B, C(..), a)

import Dummy



"""
                )
            , Test.test "exposing multiple, multiline"
                (\() ->
                    """module A exposing ((||), B, C(..), a
    )
import Dummy"""
                        |> expectPrintedAs
                            """module A exposing
    ( (||)
    , B
    , C(..)
    , a
    )

import Dummy



"""
                )
            , Test.test "exposing multiple, invalid @docs, empty @docs, multiline"
                (\() ->
                    """module A exposing ((||), B, C(..), a
    )
{-| A

@docs (&&)
@docs b
@docs
@docsa

-}
import Dummy"""
                        |> expectPrintedAs
                            """module A exposing
    ( (||)
    , B
    , C(..)
    , a
    )

{-| A

@docs (&&)
@docs b
@docsa

-}

import Dummy



"""
                )
            , Test.test "exposing multiple with @docs tags, not covering all exposes"
                (\() ->
                    """module A exposing ((||), B, C(..), a)
{-| A

@docs (&&)
@docs b
@docs a, B

-}
import Dummy"""
                        |> expectPrintedAs
                            """module A exposing
    ( a, B
    , (||), C(..)
    )

{-| A

@docs (&&)
@docs b
@docs a, B

-}

import Dummy



"""
                )
            ]
        , Test.describe "module documentation"
            [ Test.test "before imports"
                (\() ->
                    """module A exposing (..)
{-| A module about A.
-}
import Dummy"""
                        |> expectPrintedAs
                            """module A exposing (..)

{-| A module about A.
-}

import Dummy



"""
                )
            , Test.test "before declarations when no import exists"
                (\() ->
                    """module A exposing (..)
{-| A module about A.
-}
a =
    "a\""""
                        |> expectPrintedAs
                            """module A exposing (..)

{-| A module about A.
-}


a =
    "a"
"""
                )
            , Test.test "before comments"
                (\() ->
                    """module A exposing (..)
{-| A module about A.
-}
--
a =
    "a\""""
                        |> expectPrintedAs
                            """module A exposing (..)

{-| A module about A.
-}

--


a =
    "a"
"""
                )
            ]
        , Test.describe "module-level comments"
            [ Test.test "before imports without module documentation"
                (\() ->
                    """module A exposing (..)
-- A module about A.
import Dummy"""
                        |> expectPrintedAs
                            """module A exposing (..)

-- A module about A.

import Dummy



"""
                )
            , Test.test "between module documentation and imports"
                (\() ->
                    """module A exposing (..)
{-| The module about A.
-}
-- A module about A.
import Dummy"""
                        |> expectPrintedAs
                            """module A exposing (..)

{-| The module about A.
-}

-- A module about A.

import Dummy



"""
                )
            , Test.test "between module header and module documentation"
                (\() ->
                    """module A exposing (..)
-- A module about A.
{-| The module about A.
-}
import Dummy"""
                        |> expectPrintedAs
                            -- these comments are moved to _after_ the module documentation
                            """module A exposing (..)

{-| The module about A.
-}

-- A module about A.

import Dummy



"""
                )
            , Test.test "between imports and first declaration"
                (\() ->
                    """module A exposing (..)
import Dummy
-- A module about A.
zero = 0"""
                        |> expectPrintedAs
                            """module A exposing (..)

import Dummy



-- A module about A.


zero =
    0
"""
                )
            , Test.test "between declaration documentation and declaration"
                (\() ->
                    """module A exposing (..)
import Dummy
{-| 0
-}
-- not one
zero = 0"""
                        |> expectPrintedAs
                            """module A exposing (..)

import Dummy


{-| 0
-}



-- not one


zero =
    0
"""
                )
            , Test.test "between last declaration and end of file"
                (\() ->
                    """module A exposing (..)
import Dummy
zero = 0
-- A module about A."""
                        |> expectPrintedAs
                            """module A exposing (..)

import Dummy


zero =
    0



-- A module about A.
"""
                )
            ]
        , Test.describe "type alias declaration"
            [ Test.test "multiple parameters"
                (\() ->
                    """module A exposing (..)
type alias T a b =
    ( a )"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias T a b =
    a
"""
                )
            , Test.test "one parameter"
                (\() ->
                    """module A exposing (..)
type alias T a =
    List a"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias T a =
    List a
"""
                )
            , Test.test "no parameter"
                (\() ->
                    """module A exposing (..)
type alias T =
    String"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias T =
    String
"""
                )
            , Test.test "comments between parameters"
                (\() ->
                    """module A exposing (..)
type alias A parameterA {--} parameterB =
    parameterA"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias
    A
        parameterA
        {--}
        parameterB
    =
    parameterA
"""
                )
            , Test.test "comments before first parameter"
                (\() ->
                    """module A exposing (..)
type alias A {--} parameterA parameterB =
    (parameterA)"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias
    A
        {--}
        parameterA
        parameterB
    =
    parameterA
"""
                )
            , Test.test "consecutive collapsible comments before parameter"
                (\() ->
                    """module A exposing (..)
type alias A {- 0 -}
    {- 1 -} parameterA parameterB =
    (parameterA)"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A {- 0 -} {- 1 -} parameterA parameterB =
    parameterA
"""
                )
            , Test.test "comments between last parameter and type"
                (\() ->
                    """module A exposing (..)
type alias A parameter {- 0 -} =
    {- 1 -}
    parameter"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A parameter =
    {- 0 -}
    {- 1 -}
    parameter
"""
                )
            , Test.test "comments between name and type"
                (\() ->
                    """module A exposing (..)
type alias A {- 0 -} =
    {- 1 -}
    Int"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A =
    {- 0 -}
    {- 1 -}
    Int
"""
                )
            ]
        , Test.describe "choice type declaration"
            [ Test.test "multiple parameters, one variant with one parameter"
                (\() ->
                    """module A exposing (..)
type T a b
    = T ( a )"""
                        |> expectPrintedAs
                            """module A exposing (..)


type T a b
    = T a
"""
                )
            , Test.test "no parameter, one variant without parameters"
                (\() ->
                    """module A exposing (..)
type T
    = T"""
                        |> expectPrintedAs
                            """module A exposing (..)


type T
    = T
"""
                )
            , Test.test "no parameter, multiple variants without parameters"
                (\() ->
                    """module A exposing (..)
type T
    = X
    | Y"""
                        |> expectPrintedAs
                            """module A exposing (..)


type T
    = X
    | Y
"""
                )
            , Test.test "comments between type parameters"
                (\() ->
                    """module A exposing (..)
type A parameterA {--} parameterB
    = A (parameterA)"""
                        |> expectPrintedAs
                            """module A exposing (..)


type
    A
        parameterA
        {--}
        parameterB
    = A parameterA
"""
                )
            , Test.test "comments before first parameter"
                (\() ->
                    """module A exposing (..)
type A {--} parameterA parameterB
    = A (parameterA)"""
                        |> expectPrintedAs
                            """module A exposing (..)


type
    A
        {--}
        parameterA
        parameterB
    = A parameterA
"""
                )
            , Test.test "consecutive collapsible comments before parameter"
                (\() ->
                    """module A exposing (..)
type A {- 0 -}
     {- 1 -} parameterA parameterB
    = A (parameterA)"""
                        |> expectPrintedAs
                            """module A exposing (..)


type A {- 0 -} {- 1 -} parameterA parameterB
    = A parameterA
"""
                )
            , Test.test "consecutive collapsible comments between last parameter and single-line first variant"
                (\() ->
                    """module A exposing (..)
type A parameter {- 0 -}
    = {- 1 -} A parameter"""
                        |> expectPrintedAs
                            """module A exposing (..)


type A parameter
    = {- 0 -} {- 1 -} A parameter
"""
                )
            , Test.test "consecutive collapsible comments between name and single-line first variant"
                (\() ->
                    """module A exposing (..)
type A {- 0 -}
    = {- 1 -} A Int"""
                        |> expectPrintedAs
                            """module A exposing (..)


type A
    = {- 0 -} {- 1 -} A Int
"""
                )
            , Test.test "comments between name and first variant"
                (\() ->
                    """module A exposing (..)
type A -- 0
    = {- 1 -} A Int"""
                        |> expectPrintedAs
                            """module A exposing (..)


type A
    = -- 0
      {- 1 -}
      A Int
"""
                )
            , Test.test "consecutive comments before non-first single-line variant"
                (\() ->
                    """module A exposing (..)
type A
    = A Int -- 0
    | {- 1 -} B String"""
                        |> expectPrintedAs
                            """module A exposing (..)


type A
    = A Int
    | -- 0
      {- 1 -}
      B String
"""
                )
            , Test.test "consecutive collapsible comments before non-first single-line variant"
                (\() ->
                    """module A exposing (..)
type A
    = A Int {- 0 -}
    | {- 1 -} B String"""
                        |> expectPrintedAs
                            """module A exposing (..)


type A
    = A Int
    | {- 0 -} {- 1 -} B String
"""
                )
            , Test.test "consecutive collapsible comments before non-first multi-line variant"
                (\() ->
                    """module A exposing (..)
type A
    = A Int {- 0 -}
    | {- 1 -} B {--} String"""
                        |> expectPrintedAs
                            """module A exposing (..)


type A
    = A Int
    | {- 0 -} {- 1 -}
      B
        {--}
        String
"""
                )
            , Test.test "comments before first variant parameter"
                (\() ->
                    """module A exposing (..)
type A
    = A {--} Int"""
                        |> expectPrintedAs
                            """module A exposing (..)


type A
    = A
        {--}
        Int
"""
                )
            , Test.test "consecutive collapsible comments before first variant parameter"
                (\() ->
                    """module A exposing (..)
type A
    = A {- 0 -} {- 1 -}
        Int"""
                        |> expectPrintedAs
                            """module A exposing (..)


type A
    = A {- 0 -} {- 1 -} Int
"""
                )
            ]
        , Test.describe "declaration port"
            [ Test.test "already single-line"
                (\() ->
                    """port module A exposing (..)
port sendMessage : String -> Cmd msg
port messageReceiver : (String -> msg) -> Sub msg"""
                        |> expectPrintedAs
                            """port module A exposing (..)


port sendMessage : String -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg
"""
                )
            , Test.test "documentation comments"
                (\() ->
                    """port module A exposing (..)
port sendMessage : String -> Cmd msg
{-| :blushes:
-}
port messageReceiver : (String -> msg) -> Sub msg"""
                        |> expectPrintedAs
                            """port module A exposing (..)


port sendMessage : String -> Cmd msg


{-| :blushes:
-}
port messageReceiver : (String -> msg) -> Sub msg
"""
                )
            , Test.test "type on next should be single-line"
                (\() ->
                    """port module A exposing (..)
port sendMessage :
    String -> Cmd msg"""
                        |> expectPrintedAs
                            """port module A exposing (..)


port sendMessage : String -> Cmd msg
"""
                )
            ]
        , Test.describe "declaration infix"
            [ Test.test "from Basics"
                (\() ->
                    """module A exposing (..)
infix right 0 (<|) = apL
infix left  0 (|>) = apR
infix right 2 (||) = or
infix right 3 (&&) = and
infix non   4 (==) = eq
infix non   4 (/=) = neq
infix non   4 (<)  = lt
infix non   4 (>)  = gt
infix non   4 (<=) = le
infix non   4 (>=) = ge
infix right 5 (++) = append
infix left  6 (+)  = add
infix left  6 (-)  = sub
infix left  7 (*)  = mul
infix left  7 (/)  = fdiv
infix left  7 (//) = idiv
infix right 8 (^)  = pow
infix left  9 (<<) = composeL
infix right 9 (>>) = composeR"""
                        |> expectPrintedAs
                            """module A exposing (..)


infix right 0 (<|) = apL
infix left  0 (|>) = apR
infix right 2 (||) = or
infix right 3 (&&) = and
infix non   4 (==) = eq
infix non   4 (/=) = neq
infix non   4 (<) = lt
infix non   4 (>) = gt
infix non   4 (<=) = le
infix non   4 (>=) = ge
infix right 5 (++) = append
infix left  6 (+) = add
infix left  6 (-) = sub
infix left  7 (*) = mul
infix left  7 (/) = fdiv
infix left  7 (//) = idiv
infix right 8 (^) = pow
infix left  9 (<<) = composeL
infix right 9 (>>) = composeR
"""
                )
            ]
        , Test.describe "declaration expression"
            [ Test.test "value, single-line annotation as multiline"
                (\() ->
                    """module A exposing (..)
a
    :
 Int
a =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a : Int
a =
    0
"""
                )
            , Test.test "value, comment before single-line annotation"
                (\() ->
                    """module A exposing (..)
a : {- will always be 0 -} Int
a =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a :
    {- will always be 0 -}
    Int
a =
    0
"""
                )
            , Test.test "function, comment before multi-line annotation"
                (\() ->
                    """module A exposing (..)
a : {- will always return 0 -} Int
 -> Int
a _ =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a :
    {- will always return 0 -}
    Int
    -> Int
a _ =
    0
"""
                )
            , Test.test "value, comment between signature and implementation name"
                (\() ->
                    """module A exposing (..)
a : Int {- 0 -}
a =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a : Int



{- 0 -}


a =
    0
"""
                )
            ]
        , Test.describe "type"
            [ Test.test "all kinds, single-line"
                (\() ->
                    """module A exposing (..)
type alias T a =
    {p0:{a : Basics.Int, b : {}}, p1: { a | v : List String }, p2:{}->a  ->(Int)}"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias T a =
    { p0 : { a : Basics.Int, b : {} }, p1 : { a | v : List String }, p2 : {} -> a -> Int }
"""
                )
            , Test.test "all kinds, multi-line"
                (\() ->
                    """module A exposing (..)
type alias T a =
    {p0:{a : Basics.Int, b : {}}, p1: { a | v : List String }, p2:{}->a  ->(Int)
    }"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias T a =
    { p0 : { a : Basics.Int, b : {} }
    , p1 : { a | v : List String }
    , p2 : {} -> a -> Int
    }
"""
                )
            , Test.test "function input function is parenthesized"
                (\() ->
                    """module A exposing (..)
type alias T a =
    (((Int -> Int))) -> a"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias T a =
    (Int -> Int) -> a
"""
                )
            , Test.test "function input function is parenthesized even in trailing argument"
                (\() ->
                    """module A exposing (..)
type alias T a =
    Int -> (((Int -> Int))) -> a"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias T a =
    Int -> (Int -> Int) -> a
"""
                )
            , Test.test "function comments between types"
                (\() ->
                    """module A exposing (..)
type alias T =
    Int -> -- integer
    Int"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias T =
    Int
    ->
        -- integer
        Int
"""
                )
            , Test.test "function consecutive collapsible comments between types"
                (\() ->
                    """module A exposing (..)
type alias T =
    Int -> {- 0 -} {- 1 -}
    Int"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias T =
    Int
    -> {- 0 -} {- 1 -} Int
"""
                )
            , Test.test "consecutive function comments between types"
                (\() ->
                    """module A exposing (..)
type alias T =
    Int -> -- 0
    -- 1
    Int"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias T =
    Int
    ->
        -- 0
        -- 1
        Int
"""
                )
            , Test.test "comments before first record field"
                (\() ->
                    """module A exposing (..)
type alias A = { -- zero
    zero : Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A =
    { -- zero
      zero : Int
    }
"""
                )
            , Test.test "consecutive comments before first record field"
                (\() ->
                    """module A exposing (..)
type alias A = { -- 0
    -- 1
    zero : Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A =
    { -- 0
      -- 1
      zero : Int
    }
"""
                )
            , Test.test "consecutive collapsible comments before first record field"
                (\() ->
                    """module A exposing (..)
type alias A = { {- 0 -}
    {- 1 -}
    zero : Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A =
    { {- 0 -} {- 1 -} zero : Int
    }
"""
                )
            , Test.test "comments between record field name and value"
                (\() ->
                    """module A exposing (..)
type alias A = { zero : -- zero
    Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A =
    { zero :
        -- zero
        Int
    }
"""
                )
            , Test.test "consecutive collapsible comments between record field name and single-line value on next line"
                (\() ->
                    """module A exposing (..)
type alias A = { zero : {- 0 -} {- 1 -}
    Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A =
    { zero :
        {- 0 -} {- 1 -} Int
    }
"""
                )
            , Test.test "consecutive collapsible comments between record field name and multi-line value on same line"
                (\() ->
                    """module A exposing (..)
type alias A = { zero : {- 0 -} {- 1 -} List
    Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A =
    { zero :
        {- 0 -} {- 1 -}
        List
            Int
    }
"""
                )
            , Test.test "consecutive comments between record field name and value"
                (\() ->
                    """module A exposing (..)
type alias A = { zero : -- 0
    -- 1
    Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A =
    { zero :
        -- 0
        -- 1
        Int
    }
"""
                )
            , Test.test "comments between record fields"
                (\() ->
                    """module A exposing (..)
type alias A = { zero : Int, -- zero
    one : Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A =
    { zero : Int
    , -- zero
      one : Int
    }
"""
                )
            , Test.test "consecutive comments between record fields"
                (\() ->
                    """module A exposing (..)
type alias A = { zero : Int, -- 0
    -- 1
    one : Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A =
    { zero : Int
    , -- 0
      -- 1
      one : Int
    }
"""
                )
            , Test.test "comments after record fields"
                (\() ->
                    """module A exposing (..)
type alias A = { zero : Int, one : Int
    -- zero
    }"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A =
    { zero : Int
    , one : Int

    -- zero
    }
"""
                )
            , Test.test "consecutive comments after record fields"
                (\() ->
                    """module A exposing (..)
type alias A = { zero : Int, one : Int
    -- 0
    -- 1
    }"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A =
    { zero : Int
    , one : Int

    -- 0
    -- 1
    }
"""
                )
            , Test.test "comments before record extension record variable"
                (\() ->
                    """module A exposing (..)
type alias A r = { -- zero
    r | zero : Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A r =
    { -- zero
      r
        | zero : Int
    }
"""
                )
            , Test.test "consecutive comments before record extension record variable"
                (\() ->
                    """module A exposing (..)
type alias A r = { -- 0
    -- 1
    r | zero : Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A r =
    { -- 0
      -- 1
      r
        | zero : Int
    }
"""
                )
            , Test.test "comments collapsible before record extension record variable"
                (\() ->
                    """module A exposing (..)
type alias A r = { {- zero -}
    r | zero : Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A r =
    { {- zero -} r
        | zero : Int
    }
"""
                )
            , Test.test "comments before first record extension field"
                (\() ->
                    """module A exposing (..)
type alias A r = { r | -- zero
    zero : Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A r =
    { r
        | -- zero
          zero : Int
    }
"""
                )
            , Test.test "consecutive comments before first record extension field"
                (\() ->
                    """module A exposing (..)
type alias A r = { r | -- 0
    -- 1
    zero : Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A r =
    { r
        | -- 0
          -- 1
          zero : Int
    }
"""
                )
            , Test.test "comments collapsible before first record extension field"
                (\() ->
                    """module A exposing (..)
type alias A r = { r | {- zero -}
    zero : Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A r =
    { r
        | {- zero -} zero : Int
    }
"""
                )
            , Test.test "comments between record extension field name and value"
                (\() ->
                    """module A exposing (..)
type alias A r = { r | zero : -- zero
    Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A r =
    { r
        | zero :
            -- zero
            Int
    }
"""
                )
            , Test.test "comments between record extension field name and value not on the same line"
                (\() ->
                    """module A exposing (..)
type alias A r = { r | zero : {- zero -}
    Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A r =
    { r
        | zero :
            {- zero -} Int
    }
"""
                )
            , Test.test "comments between record extension fields"
                (\() ->
                    """module A exposing (..)
type alias A r = { r | zero : Int, -- zero
    one : Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A r =
    { r
        | zero : Int
        , -- zero
          one : Int
    }
"""
                )
            , Test.test "consecutive comments between record extension fields"
                (\() ->
                    """module A exposing (..)
type alias A r = { r | zero : Int, -- zero
 -- one
    one : Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A r =
    { r
        | zero : Int
        , -- zero
          -- one
          one : Int
    }
"""
                )
            , Test.test "comments collapsed between record extension fields"
                (\() ->
                    """module A exposing (..)
type alias A r = { r | zero : Int, {- one -}
    one : Int }"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A r =
    { r
        | zero : Int
        , {- one -} one : Int
    }
"""
                )
            , Test.test "comments after record extension fields"
                (\() ->
                    """module A exposing (..)
type alias A r = { r | zero : Int, one : Int
    -- zero
    }"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A r =
    { r
        | zero : Int
        , one : Int

        -- zero
    }
"""
                )
            , Test.test "single-line construct"
                (\() ->
                    """module A exposing (..)
type alias A = List Int"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A =
    List Int
"""
                )
            , Test.test "multi-line construct"
                (\() ->
                    """module A exposing (..)
type alias A = List
               Int"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A =
    List
        Int
"""
                )
            , Test.test "construct written in single line with consecutive comments before argument"
                (\() ->
                    """module A exposing (..)
type alias A = List {--}{--} Int"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A =
    List
        {--}
        {--}
        Int
"""
                )
            , Test.test "single-line construct with comments collapsible before argument"
                (\() ->
                    """module A exposing (..)
type alias A = List {- 0 -} Int"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A =
    List {- 0 -} Int
"""
                )
            , Test.test "construct with consecutive comments collapsible before multi-line argument"
                (\() ->
                    """module A exposing (..)
type alias A = List {- 0 -}{- 1 -} (List
                             Int)"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A =
    List
        {- 0 -} {- 1 -}
        (List
            Int
        )
"""
                )
            , Test.test "multi-line construct with consecutive comments collapsible before single-line argument"
                (\() ->
                    """module A exposing (..)
type alias A = Result {- 0 -}{- 1 -} Int (List
                                          Int)"""
                        |> expectPrintedAs
                            """module A exposing (..)


type alias A =
    Result
        {- 0 -} {- 1 -} Int
        (List
            Int
        )
"""
                )
            ]
        , Test.describe "expression"
            [ Test.test "negate 0"
                (\() ->
                    """module A exposing (..)
a = -0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    0
"""
                )
            , Test.test "negate 0, parenthesized"
                (\() ->
                    """module A exposing (..)
a = -((((((0))))))"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    0
"""
                )
            , Test.test "negate 0 multiple times, parenthesized"
                (\() ->
                    """module A exposing (..)
a = -(((-(((-0))))))"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    0
"""
                )
            , Test.test "negate 0x0"
                (\() ->
                    """module A exposing (..)
a = -0x0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    0x00
"""
                )
            , Test.test "doubly negated not literal-zero"
                (\() ->
                    """module A exposing (..)
a = -(-1)"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    -(-1)
"""
                )
            , Test.test "doubly negated not literal-zero, parenthesized"
                (\() ->
                    """module A exposing (..)
a = -((-((1))))"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    -(-1)
"""
                )
            , Test.test "if-then-else with single-line condition"
                (\() ->
                    """module A exposing (..)
a =
    if
        (True
        )
    then 0 else 1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    if True then
        0

    else
        1
"""
                )
            , Test.test "if-then-else with multi-line condition"
                (\() ->
                    """module A exposing (..)
a =
    if
        Basics.not
            True
    then 0 else 1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    if
        Basics.not
            True
    then
        0

    else
        1
"""
                )
            , Test.test "if-then-else with comments before condition"
                (\() ->
                    """module A exposing (..)
a =
    if -- condition
        True then 0 else 1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    if
        -- condition
        True
    then
        0

    else
        1
"""
                )
            , Test.test "if-then-else with comments before on True branch"
                (\() ->
                    """module A exposing (..)
a =
    if True then -- 0
        0 else 1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    if True then
        -- 0
        0

    else
        1
"""
                )
            , Test.test "if-then-else with comments before on False branch"
                (\() ->
                    """module A exposing (..)
a =
    if True then 0 else -- 1
        1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    if True then
        0

    else
        -- 1
        1
"""
                )
            , Test.test "if-then-else with another if-then-else in the else branch"
                (\() ->
                    """module A exposing (..)
a =
    if True then
        0

    else
        (if False then
            1

         else
            2
        )"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    if True then
        0

    else if False then
        1

    else
        2
"""
                )
            , Test.test "if-then-else with another parenthesized if-then-else in the else branch with comments"
                (\() ->
                    """module A exposing (..)
a =
    if True then
        0

    else
        (-- on False
         if False then
            1

         else
            2
        )"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    if True then
        0

    else
        (-- on False
         if False then
            1

         else
            2
        )
"""
                )
            , Test.test "if-then-else with another parenthesized if-then-else in the else branch with comments and comments before parens"
                (\() ->
                    """module A exposing (..)
a =
    if True then
        0

    else
        -- on False
        (-- in parens
         if False then
            1

         else
            2
        )"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    if True then
        0

    else
        -- on False
        (-- in parens
         if False then
            1

         else
            2
        )
"""
                )
            , Test.test "if-then-else with comments before if-then-else in the else branch without comments itself (!)"
                (\() ->
                    """module A exposing (..)
a =
    if True then
        0

    else -- on False
        ((if False then
            1

        else
            2))"""
                        |> expectPrintedAs
                            -- rudiment from elm-format
                            """module A exposing (..)


a =
    if True then
        0

    else
    -- on False
    if
        False
    then
        1

    else
        2
"""
                )
            , Test.test "when-is with one case"
                (\() ->
                    """module A exposing (..)
a =
    when {} is
        {} -> 0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    when {} is
        {} ->
            0
"""
                )
            , Test.test "case-of with one case"
                (\() ->
                    """module A exposing (..)
a =
    case () of
        () -> 0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    when {} is
        {} ->
            0
"""
                )
            , Test.test "case-of with consecutive comments before cased expression"
                (\() ->
                    """module A exposing (..)
a =
    case -- 0
    -- 1
    () of
        () -> 0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    when
        -- 0
        -- 1
        {}
    is
        {} ->
            0
"""
                )
            , Test.test "case-of with consecutive comments before case pattern"
                (\() ->
                    """module A exposing (..)
a =
    case () of -- 0
        -- 1
        () -> 0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    when {} is
        -- 0
        -- 1
        {} ->
            0
"""
                )
            , Test.test "case-of with consecutive comments before case result"
                (\() ->
                    """module A exposing (..)
a =
    case () of
        () -> -- 0
            -- 1
            0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    when {} is
        {} ->
            -- 0
            -- 1
            0
"""
                )
            , Test.test "case-of with multiple cases"
                (\() ->
                    """module A exposing (..)
a =
    case 0 == 1 of
        True -> 0
        Basics.False -> 1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    when 0 == 1 is
        True ->
            0

        Basics.False ->
            1
"""
                )
            , Test.test "lambda, consecutive comments before result"
                (\() ->
                    """module A exposing (..)
a =
    \\b -> -- 0
    -- 1
    b"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \\b ->
        -- 0
        -- 1
        b
"""
                )
            , Test.test "lambda, consecutive {- -} comments before result do not get collapsed"
                (\() ->
                    """module A exposing (..)
a =
    \\b -> {- 0 -} {- 1 -} b"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \\b ->
        {- 0 -}
        {- 1 -}
        b
"""
                )
            , Test.test "lambda, multi-line result in single line"
                (\() ->
                    """module A exposing (..)
a =
    \\b -> if True then 0 else b"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \\b ->
        if True then
            0

        else
            b
"""
                )
            , Test.test "lambda, consecutive comments before first parameter"
                (\() ->
                    """module A exposing (..)
a =
    \\ -- 0
    -- 1
    b -> b"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \\
     -- 0
     -- 1
     b
    ->
        b
"""
                )
            , Test.test "lambda, consecutive comments between parameters"
                (\() ->
                    """module A exposing (..)
a =
    \\b -- 0
    -- 1
    c -> b"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \\
     b
     -- 0
     -- 1
     c
    ->
        b
"""
                )
            , Test.test "single-line lambda, consecutive collapsible comments before single-line parameter"
                (\() ->
                    """module A exposing (..)
a =
    \\ {- 0 -} {- 1 -}
    b -> b"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \\{- 0 -} {- 1 -} b ->
        b
"""
                )
            , Test.test "multi-line lambda, consecutive collapsible comments before single-line parameter"
                (\() ->
                    """module A exposing (..)
a =
    \\{- 0 -} {- 1 -} b (c{--}) -> b"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \\
     {- 0 -} {- 1 -} b
     (c
      {--}
     )
    ->
        b
"""
                )
            , Test.test "lambda, consecutive collapsible comments before multi-line parameter"
                (\() ->
                    """module A exposing (..)
a =
    \\{- 0 -} {- 1 -} (b{--}) -> b"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \\
     {- 0 -} {- 1 -}
     (b
      {--}
     )
    ->
        b
"""
                )
            , Test.test "let-in with one destructuring declaration"
                (\() ->
                    """module A exposing (..)
a =
    let { b } = {b=0} in b"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    let
        { b } =
            { b = 0 }
    in
    b
"""
                )
            , Test.test "let-in with one function declaration without type"
                (\() ->
                    """module A exposing (..)
a =
    let b () = 0 in b ()"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    let
        b {} =
            0
    in
    b {}
"""
                )
            , Test.test "let-in with one value declaration without type"
                (\() ->
                    """module A exposing (..)
a =
    let b = 0 in b"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    let
        b =
            0
    in
    b
"""
                )
            , Test.test "let-in with one value declaration, comments before result"
                (\() ->
                    """module A exposing (..)
a =
    let b = 0 in {- 0 -} b"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    let
        b =
            0
    in
    {- 0 -}
    b
"""
                )
            , Test.test "let-in with one value declaration, comments before first let declaration"
                (\() ->
                    """module A exposing (..)
a =
    let -- 0
        b = 0 in b"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    let
        -- 0
        b =
            0
    in
    b
"""
                )
            , Test.test "let-in with one value declaration, comments between let declarations"
                (\() ->
                    """module A exposing (..)
a =
    let b = 0
        -- 0
        c = 1 in b"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    let
        b =
            0

        -- 0
        c =
            1
    in
    b
"""
                )
            , Test.test "let-in with one destructuring, consecutive comments before destructured expression"
                (\() ->
                    """module A exposing (..)
a =
    let b = -- 0
            0 in b"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    let
        b =
            -- 0
            0
    in
    b
"""
                )
            , Test.test "let-in with one value declaration with type"
                (\() ->
                    """module A exposing (..)
a =
    let b:Int

        b = 0 in b"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    let
        b : Int
        b =
            0
    in
    b
"""
                )
            , Test.test "let-in with one value declaration with type, comment between signature and implementation name"
                (\() ->
                    """module A exposing (..)
a =
    let
        b : Int {- 0 -}
        b =
            0
    in
    b"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    let
        b : Int
        {- 0 -}
        b =
            0
    in
    b
"""
                )
            , Test.test "let-in with multiple destructuring declarations"
                (\() ->
                    """module A exposing (..)
a =
    let { b } = {b=0}
        { c } = {c=1}
    in b+c"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    let
        { b } =
            { b = 0 }

        { c } =
            { c = 1 }
    in
    b + c
"""
                )
            , Test.test "call, applied function is multi-line"
                (\() ->
                    """module A exposing (..)
a =
    (identity {--}) identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    (identity
     {--}
    )
        identity
"""
                )
            , Test.test "call, multi-line comments before first argument"
                (\() ->
                    """module A exposing (..)
a =
    identity {--} identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity
        {--}
        identity
"""
                )
            , Test.test "single-line call, consecutive collapsible comments before first argument"
                (\() ->
                    """module A exposing (..)
a =
    identity {- 0 -} {- 1 -} identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity {- 0 -} {- 1 -} identity
"""
                )
            , Test.test "multi-line call, consecutive collapsible comments before first argument"
                (\() ->
                    """module A exposing (..)
a =
    identity {- 0 -} {- 1 -} (identity
    )"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity
        {- 0 -} {- 1 -} identity
"""
                )
            , Test.test "call, consecutive collapsible comments before multi-line first argument"
                (\() ->
                    """module A exposing (..)
a =
    identity {- 0 -} {- 1 -} (identity{--})"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity
        {- 0 -} {- 1 -}
        (identity
         {--}
        )
"""
                )
            , Test.test "call, consecutive collapsible comments before first single-line argument, multi-line follow-up argument"
                (\() ->
                    """module A exposing (..)
a =
    identity {- 0 -} {- 1 -} identity
        identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity {- 0 -} {- 1 -} identity
        identity
"""
                )
            , Test.test "single-line call, consecutive collapsible comments before last argument"
                (\() ->
                    """module A exposing (..)
a =
    identity identity {- 0 -} {- 1 -} identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity identity {- 0 -} {- 1 -} identity
"""
                )
            , Test.test "call, consecutive collapsible comments before last multi-line argument"
                (\() ->
                    """module A exposing (..)
a =
    identity identity {- 0 -} {- 1 -} (identity{--})"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity identity
        {- 0 -} {- 1 -}
        (identity
         {--}
        )
"""
                )
            , Test.test "multi-line call, consecutive collapsible comments before last argument"
                (\() ->
                    """module A exposing (..)
a =
    identity identity {- 0 -} {- 1 -} (identity
    )"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity identity
        {- 0 -} {- 1 -} identity
"""
                )
            , Test.test "|> pipeline, multi-line"
                (\() ->
                    """module A exposing (..)
a =
    identity |> identity identity |>
    identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity
        |> identity identity
        |> identity
"""
                )
            , Test.test "|> pipeline with multi-line function"
                (\() ->
                    """module A exposing (..)
a =
    identity |> identity
    identity |> identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity
        |> identity
            identity
        |> identity
"""
                )
            , Test.test "++ pipeline with multi-line list"
                (\() ->
                    """module A exposing (..)
a =
    [] ++ [ 0
    ] ++ []"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    []
        ++ [ 0
           ]
        ++ []
"""
                )
            , Test.test "|> pipeline with parenthesized multi-line function"
                (\() ->
                    """module A exposing (..)
a =
    identity
        |> (if True then
                identity

            else
                identity
           )
        |> identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity
        |> (if True then
                identity

            else
                identity
           )
        |> identity
"""
                )
            , Test.test "|> pipeline, single-line"
                (\() ->
                    """module A exposing (..)
a =
    identity |> identity identity |> identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity |> identity identity |> identity
"""
                )
            , Test.test "|> pipeline, written as single-line with consecutive comments before rightest expression"
                (\() ->
                    """module A exposing (..)
a =
    identity |> identity identity |> {- 0 -} {--} identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity
        |> identity identity
        |> {- 0 -}
           {--}
           identity
"""
                )
            , Test.test "|> pipeline, written as single-line with consecutive comments collapsible before single-line rightest expression"
                (\() ->
                    """module A exposing (..)
a =
    identity |> identity identity |> {- 0 -} {- 1 -} identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity |> identity identity |> {- 0 -} {- 1 -} identity
"""
                )
            , Test.test "|> pipeline, with consecutive comments collapsible before multi-line rightest expression"
                (\() ->
                    """module A exposing (..)
a =
    identity |> identity identity |> {- 0 -} {- 1 -} identity
    identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity
        |> identity identity
        |> {- 0 -} {- 1 -}
           identity
            identity
"""
                )
            , Test.test "|> pipeline, written as single-line with consecutive comments before not rightest expression"
                (\() ->
                    """module A exposing (..)
a =
    identity |> {- 0 -} {--} identity identity |> identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity
        |> {- 0 -}
           {--}
           identity identity
        |> identity
"""
                )
            , Test.test "|> pipeline, written as single-line with consecutive collapsible comments before not rightest expression"
                (\() ->
                    """module A exposing (..)
a =
    identity |> {- 0 -} {- 1 -} identity identity |> identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity |> {- 0 -} {- 1 -} identity identity |> identity
"""
                )
            , Test.test "<| pipeline, multi-line"
                (\() ->
                    """module A exposing (..)
a =
    identity <| identity identity <|
    identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity <|
        identity identity <|
            identity
"""
                )
            , Test.test "<| pipeline, multi-line, consecutive comments before rightest expression"
                (\() ->
                    """module A exposing (..)
a =
    identity <| identity identity <| -- 0
    -- 1
    identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity <|
        identity identity <|
            -- 0
            -- 1
            identity
"""
                )
            , Test.test "<| pipeline, multi-line, consecutive comments collapsible before rightest expression"
                (\() ->
                    """module A exposing (..)
a =
    identity <| identity identity <| {- 0 -}
    {- 1 -}
    identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity <|
        identity identity <|
            {- 0 -} {- 1 -} identity
"""
                )
            , Test.test "<| pipeline, multi-line, consecutive comments before non-rightest expression"
                (\() ->
                    """module A exposing (..)
a =
    identity <| -- 0
    -- 1
    identity identity <| identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity <|
        -- 0
        -- 1
        identity identity <|
            identity
"""
                )
            , Test.test "<| pipeline, multi-line, consecutive comments collapsible before non-rightest expression"
                (\() ->
                    """module A exposing (..)
a =
    identity <| {- 0 -}
    {- 1 -}
    identity identity <| identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity <|
        {- 0 -} {- 1 -} identity identity <|
            identity
"""
                )
            , Test.test "<| pipeline with multi-line application part"
                (\() ->
                    """module A exposing (..)
a =
    identity <| identity
    identity <| identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity <|
        identity
            identity
        <|
            identity
"""
                )
            , Test.test "<| pipeline with multi-line lambda part"
                (\() ->
                    """module A exposing (..)
a =
    identity <| (\\_ -> (identity
    )) <| identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity <|
        (\\_ ->
            identity
        )
        <|
            identity
"""
                )
            , Test.test "<| pipeline, single-line"
                (\() ->
                    """module A exposing (..)
a =
    identity <| identity identity <| identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity <| identity identity <| identity
"""
                )
            , Test.test "<| pipeline, lambda as the rightest expression"
                (\() ->
                    """module A exposing (..)
a =
    identity <| identity identity <| (\\_ -> identity)"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity <| identity identity <| \\_ -> identity
"""
                )
            , Test.test "<| pipeline, lambda as not the rightest expression"
                (\() ->
                    """module A exposing (..)
a =
    identity <| ((\\_ -> identity)) <| identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    identity <| (\\_ -> identity) <| identity
"""
                )
            , Test.test "various operations, single-line"
                (\() ->
                    """module A exposing (..)
a =
    3 + (4 * 5 // 3) // 5 - 0 |> identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    3 + (4 * 5 // 3) // 5 - 0 |> identity
"""
                )
            , Test.test "various operations, multi-line"
                (\() ->
                    """module A exposing (..)
a =
    3 + (4 * 5 // 3) // 5 - 0 |>
    identity"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    3
        + (4 * 5 // 3)
        // 5
        - 0
        |> identity
"""
                )
            , Test.test "parenthesized with comments before"
                (\() ->
                    """module A exposing (..)
a = ((-- zero
    (0)))"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    (-- zero
     0
    )
"""
                )
            , Test.test "parenthesized with consecutive comments before"
                (\() ->
                    """module A exposing (..)
a = ((-- 0
    -- 1
    (0)))"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    (-- 0
     -- 1
     0
    )
"""
                )
            , Test.test "parenthesized with comments after"
                (\() ->
                    """module A exposing (..)
a = (((0)
    -- zero
   ))"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    (0
     -- zero
    )
"""
                )
            , Test.test "parenthesized with consecutive comments after"
                (\() ->
                    """module A exposing (..)
a = (((0)
    -- 0
    -- 1
   ))"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    (0
     -- 0
     -- 1
    )
"""
                )
            , Test.test "parenthesized with comments before and after"
                (\() ->
                    """module A exposing (..)
a = (-- before
  ((0)
    -- after
   ))"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    (-- before
     0
     -- after
    )
"""
                )
            , Test.test "empty list with consecutive comments"
                (\() ->
                    """module A exposing (..)
a = [ {--}{- 0 -} ]"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    [{--}
     {- 0 -}
    ]
"""
                )
            , Test.test "empty list with consecutive comments collapsible"
                (\() ->
                    """module A exposing (..)
a = [ {- 0 -}{- 1 -} ]"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    [{- 0 -} {- 1 -}]
"""
                )
            , Test.test "comments before first list element"
                (\() ->
                    """module A exposing (..)
a = [ -- zero
    0 ]"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    [ -- zero
      0
    ]
"""
                )
            , Test.test "consecutive comments before first list element"
                (\() ->
                    """module A exposing (..)
a = [ -- 0
    -- 1
    0 ]"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    [ -- 0
      -- 1
      0
    ]
"""
                )
            , Test.test "comments between list elements"
                (\() ->
                    """module A exposing (..)
a = [ 0, -- zero
    0 ]"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    [ 0
    , -- zero
      0
    ]
"""
                )
            , Test.test "consecutive comments between list elements"
                (\() ->
                    """module A exposing (..)
a = [ 0, -- 0
    -- 1
    0 ]"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    [ 0
    , -- 0
      -- 1
      0
    ]
"""
                )
            , Test.test "consecutive comments collapsible before single-line list element"
                (\() ->
                    """module A exposing (..)
a = [ 0, {- 0 -}
    {- 1 -}
    0 ]"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    [ 0
    , {- 0 -} {- 1 -} 0
    ]
"""
                )
            , Test.test "consecutive comments collapsible before multi-line list element"
                (\() ->
                    """module A exposing (..)
a = [ 0, {- 0 -}
    {- 1 -}
    (0{--}) ]"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    [ 0
    , {- 0 -} {- 1 -}
      (0
       {--}
      )
    ]
"""
                )
            , Test.test "comments after list elements"
                (\() ->
                    """module A exposing (..)
a = [ 0, 0
    -- zero
    ]"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    [ 0
    , 0

    -- zero
    ]
"""
                )
            , Test.test "consecutive comments after list elements"
                (\() ->
                    """module A exposing (..)
a = [ 0, 0
    -- 0
    -- 1
    ]"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    [ 0
    , 0

    -- 0
    -- 1
    ]
"""
                )
            , Test.test "empty record with consecutive comments"
                (\() ->
                    """module A exposing (..)
a = { -- 0
      -- 1
    }"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    {-- 0
     -- 1
    }
"""
                )
            , Test.test "empty record with consecutive comments collapsible"
                (\() ->
                    """module A exposing (..)
a = { {- 0 -}
      {- 1 -}
    }"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    {{- 0 -} {- 1 -}}
"""
                )
            , Test.test "comments before first record field"
                (\() ->
                    """module A exposing (..)
a = { -- zero
    zero = 0 }"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    { -- zero
      zero = 0
    }
"""
                )
            , Test.test "single-line record, comments collapsible before first field"
                (\() ->
                    """module A exposing (..)
a = {{- zero -} zero = 0}"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    { {- zero -} zero = 0 }
"""
                )
            , Test.test "multi-line record, comments collapsible before first field"
                (\() ->
                    """module A exposing (..)
a = { {- zero -}
    zero = 0 }"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    { {- zero -} zero = 0
    }
"""
                )
            , Test.test "comments between record field name and value"
                (\() ->
                    """module A exposing (..)
a = { zero = -- zero
    0 }"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    { zero =
        -- zero
        0
    }
"""
                )
            , Test.test "multi-line record, comments between field name and value not on same line"
                (\() ->
                    """module A exposing (..)
a = { zero = {- zero -}
    0 }"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    { zero =
        {- zero -} 0
    }
"""
                )
            , Test.test "multi-line record, comments between field name and multi-line value"
                (\() ->
                    """module A exposing (..)
a = { zero = {- zero -} identity
    0 }"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    { zero =
        {- zero -}
        identity
            0
    }
"""
                )
            , Test.test "multi-line record, comments between field name and value on same line"
                (\() ->
                    """module A exposing (..)
a = { zero = {- zero -} 0
    }"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    { zero = {- zero -} 0
    }
"""
                )
            , Test.test "comments between record fields"
                (\() ->
                    """module A exposing (..)
a = { zero = 0, -- zero
    one = 1 }"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    { zero = 0
    , -- zero
      one = 1
    }
"""
                )
            , Test.test "multi-line record, comments collapsible between fields"
                (\() ->
                    """module A exposing (..)
a = { zero = 0, {- zero -}
    one = 1 }"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    { zero = 0
    , {- zero -} one = 1
    }
"""
                )
            , Test.test "comments after record fields"
                (\() ->
                    """module A exposing (..)
a = { zero = 0, one = 1
    -- zero
    }"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    { zero = 0
    , one = 1

    -- zero
    }
"""
                )
            , Test.test "char without escapes"
                (\() ->
                    """module A exposing (..)
a = 'n' """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    'n'
"""
                )
            , Test.test "char with escape"
                (\() ->
                    """module A exposing (..)
a = '\\u{000D}' """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    '\\u{000D}'
"""
                )
            , Test.test "char emoji (multiple codes, SymbolOther)"
                (\() ->
                    """module A exposing (..)
a = '🟩' """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    '🟩'
"""
                )
            , Test.test "single double quote string without escapes"
                (\() ->
                    """module A exposing (..)
a = "normal text" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    "normal text"
"""
                )
            , Test.test "single double quote string with escaped backslash followed by n"
                (\() ->
                    """module A exposing (..)
a = "\\\\n" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    "\\\\n"
"""
                )
            , Test.test "single double quote string with escaped carriage return"
                (\() ->
                    """module A exposing (..)
a = "\\r" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    "\\u{000D}"
"""
                )
            , Test.test "single double quote string with escapes"
                (\() ->
                    """module A exposing (..)
a = "\\"\\\\\\t\\u{000D}" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    "\\"\\\\\\t\\u{000D}"
"""
                )
            , Test.test "single double quote string with emoji (multiple codes, SymbolOther)"
                (\() ->
                    """module A exposing (..)
a = "🟩" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    "🟩"
"""
                )
            , Test.test "triple double quote string single-line without escapes"
                (\() ->
                    """module A exposing (..)
a = \"\"\"normal text\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"normal text\"\"\"
"""
                )
            , Test.test "triple double quote string single-line with emoji (multiple codes, SymbolOther)"
                (\() ->
                    """module A exposing (..)
a = \"\"\"🟩\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"🟩\"\"\"
"""
                )
            , Test.test "triple double quote string with unicode escape ansi hide cursor"
                (\() ->
                    """module A exposing (..)
a = \"\"\"\\u{1B}[?25l\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"\\u{001B}[?25l\"\"\"
"""
                )
            , Test.test "triple double quote string multi-line without escapes"
                (\() ->
                    """module A exposing (..)
a = \"\"\"first line
second line
    \"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"first line
second line
    \"\"\"
"""
                )
            , Test.test "triple double quote string un-escapes double quote after first before last char"
                (\() ->
                    """module A exposing (..)
a = \"\"\"normal \\" text\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"normal " text\"\"\"
"""
                )
            , Test.test "triple double quote string does not escape double quote as first char if not followed by single double quote"
                (\() ->
                    """module A exposing (..)
a = \"\"\"\\"normal text\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"\"normal text\"\"\"
"""
                )
            , Test.test "triple double quote string does not escape double quote as first and second char if not followed by single double quote"
                (\() ->
                    """module A exposing (..)
a = \"\"\"\\""normal text\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"\"\"normal text\"\"\"
"""
                )
            , Test.test "triple double quote string escapes double quote as first and second and third char"
                (\() ->
                    """module A exposing (..)
a = \"\"\"\\\"\"\"normal text\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"\\"\\"\\"normal text\"\"\"
"""
                )
            , Test.test "triple double quote string escapes double quote as first and second and third and fourth char"
                (\() ->
                    """module A exposing (..)
a = \"\"\"\\\"\"\"\\"normal text\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"\\"\\"\\"\\"normal text\"\"\"
"""
                )
            , Test.test "triple double quote string escapes double quote as last char"
                (\() ->
                    """module A exposing (..)
a = \"\"\"normal text\\\"\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"normal text\\\"\"\"\"
"""
                )
            , Test.test "triple double quote string escapes double quote as second-last and last char"
                (\() ->
                    """module A exposing (..)
a = \"\"\"normal text"\\\"\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"normal text\\"\\\"\"\"\"
"""
                )
            , Test.test "triple double quote string does not escape double quote as second-last before last char not double quote"
                (\() ->
                    """module A exposing (..)
a = \"\"\"normal text".\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"normal text".\"\"\"
"""
                )
            , Test.test "triple double quote string does not escape double quote not as the first or last char and not neighboring double quotes"
                (\() ->
                    """module A exposing (..)
a = \"\"\"normal " text\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"normal " text\"\"\"
"""
                )
            , Test.test "triple double quote string does not escape 2 consecutive double quotes not as the first or last char and not neighboring double quotes"
                (\() ->
                    """module A exposing (..)
a = \"\"\"normal "" text\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"normal "" text\"\"\"
"""
                )
            , Test.test "triple double quote string escapes 3 consecutive double quotes not as the first or last char and not neighboring double quotes"
                (\() ->
                    """module A exposing (..)
a = \"\"\"normal \\""\\" text\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"normal \\"\\"\\" text\"\"\"
"""
                )
            , Test.test "triple double quote string escapes 4 consecutive double quotes not as the first or last char and not neighboring double quotes"
                (\() ->
                    """module A exposing (..)
a = \"\"\"normal \\""\\"" text\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"normal \\"\\"\\"\\" text\"\"\"
"""
                )
            , Test.test "triple double quote string with escaped backslash followed by n"
                (\() ->
                    """module A exposing (..)
a = \"\"\"\\\\n\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"\\\\n\"\"\"
"""
                )
            , Test.test "triple double quote string with escaped backslash followed by r"
                (\() ->
                    """module A exposing (..)
a = \"\"\"\\\\r\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"\\\\r\"\"\"
"""
                )
            , Test.test "triple double quote string with escaped backslash followed by u{000D}"
                (\() ->
                    """module A exposing (..)
a = \"\"\"\\\\u{000D}\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"\\\\u{000D}\"\"\"
"""
                )
            , Test.test "triple double quote string with escaped backslash followed by u{1234}"
                (\() ->
                    """module A exposing (..)
a = \"\"\"\\\\u{1234}\"\"\" """
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    \"\"\"\\\\u{1234}\"\"\"
"""
                )
            , Test.test "comments between parameters"
                (\() ->
                    """module A exposing (..)
a parameterA {--} parameterB =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a
    parameterA
    {--}
    parameterB
    =
    0
"""
                )
            , Test.test "comments before first parameter"
                (\() ->
                    """module A exposing (..)
a {--} parameterA parameterB =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a
    {--}
    parameterA
    parameterB
    =
    0
"""
                )
            , Test.test "comments between last parameter and result"
                (\() ->
                    """module A exposing (..)
a parameter {- 0 -} =
    {- 1 -}
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a parameter =
    {- 0 -}
    {- 1 -}
    0
"""
                )
            , Test.test "comments between implementation name and result"
                (\() ->
                    """module A exposing (..)
a {- 0 -} =
    {- 1 -}
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    {- 0 -}
    {- 1 -}
    0
"""
                )
            ]
        , Test.describe "pattern"
            [ Test.test "parenthesized with comments only before"
                (\() ->
                    """module A exposing (..)
a ({--}_ as argument) =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a
    ({--}
     _ as argument
    )
    =
    0
"""
                )
            , Test.test "parenthesized with comments only after"
                (\() ->
                    """module A exposing (..)
a (_ as argument{--}) =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a
    (_ as argument
     {--}
    )
    =
    0
"""
                )
            , Test.test "parenthesized with comments before and after"
                (\() ->
                    """module A exposing (..)
a ({--}_ as argument{--}) =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a
    ({--}
     _ as argument
     {--}
    )
    =
    0
"""
                )
            , Test.test "parenthesized with consecutive collapsible comments only before single-line"
                (\() ->
                    """module A exposing (..)
a ({-0-}{-1-}
    _ as argument) =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a ({- 0 -} {- 1 -} _ as argument) =
    0
"""
                )
            , Test.test "parenthesized with consecutive collapsible comments only after single-line"
                (\() ->
                    """module A exposing (..)
a (_ as argument
   {-0-}{-1-}) =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a (_ as argument {- 0 -} {- 1 -}) =
    0
"""
                )
            , Test.test "parenthesized with consecutive collapsible comments before and after single-line"
                (\() ->
                    """module A exposing (..)
a ({-0-}{-1-}
   _ as argument
   {-2-}{-3-}) =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a ({- 0 -} {- 1 -} _ as argument {- 2 -} {- 3 -}) =
    0
"""
                )
            , Test.test "parenthesized with consecutive collapsible comments before and after multi-line"
                (\() ->
                    """module A exposing (..)
a ({-0-}{-1-}
   _ as -- line breaker
   argument
   {-2-}{-3-}) =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a
    ({- 0 -} {- 1 -}
     _
     as
        -- line breaker
        argument
     {- 2 -} {- 3 -}
    )
    =
    0
"""
                )
            , Test.test "as with comments before name"
                (\() ->
                    """module A exposing (..)
a (_ as -- argument
  argument) =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a
    (_
     as
        -- argument
        argument
    )
    =
    0
"""
                )
            , Test.test "single-line as with comments collapsible before name"
                (\() ->
                    """module A exposing (..)
a (_ as {- argument -}
  argument) =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a (_ as {- argument -} argument) =
    0
"""
                )
            , Test.test "as with multi-line pattern"
                (\() ->
                    """module A exposing (..)
a ([-- in list
    _ 
   ] as argument) =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a
    ([ -- in list
       _
     ]
     as
        argument
    )
    =
    0
"""
                )
            , Test.test "multi-line as with comments collapsible before name"
                (\() ->
                    """module A exposing (..)
a ([-- in list
    _ 
   ] as {- argument -}
  argument) =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a
    ([ -- in list
       _
     ]
     as
        {- argument -} argument
    )
    =
    0
"""
                )
            , Test.test "empty record with consecutive comments"
                (\() ->
                    """module A exposing (..)
a { -- 0
  -- 1
  } =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a
    {-- 0
     -- 1
    }
    =
    0
"""
                )
            , Test.test "empty record with consecutive collapsible comments"
                (\() ->
                    """module A exposing (..)
a { {- 0 -}
  {- 1 -}
  } =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a {{- 0 -} {- 1 -}} =
    0
"""
                )
            , Test.test "record with comments before first field"
                (\() ->
                    """module A exposing (..)
a { -- 0
  -- 1
  zero } =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a
    { -- 0
      -- 1
      zero
    }
    =
    0
"""
                )
            , Test.test "record with consecutive collapsible comments before first field"
                (\() ->
                    """module A exposing (..)
a { {- 0 -}
  {- 1 -}
  zero } =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a { {- 0 -} {- 1 -} zero } =
    0
"""
                )
            , Test.test "record with comments between fields"
                (\() ->
                    """module A exposing (..)
a { zero, -- 0
  -- 1
  one } =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a
    { zero
    , -- 0
      -- 1
      one
    }
    =
    0
"""
                )
            , Test.test "record with collapsible comments between fields"
                (\() ->
                    """module A exposing (..)
a { zero, {- 0 -}
  {- 1 -}
  one } =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a { zero, {- 0 -} {- 1 -} one } =
    0
"""
                )
            , Test.test "record with comments after fields"
                (\() ->
                    """module A exposing (..)
a { zero,
  one -- 0
  -- 1
  } =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a
    { zero
    , one
      -- 0
      -- 1
    }
    =
    0
"""
                )
            , Test.test "record with collapsible comments between renamed fields"
                (\() ->
                    """module A exposing (..)
a { zero = n0, {- 0 -}
  {- 1 -}
  one = n1 } =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a { zero = n0, one = {- 0 -} {- 1 -} n1 } =
    0
"""
                )
            , Test.test "record with comments after renamed fields"
                (\() ->
                    """module A exposing (..)
a { zero = n0,
  one = n1 -- 0
  -- 1
  } =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a
    { zero = n0
    , one = n1
      -- 0
      -- 1
    }
    =
    0
"""
                )
            , Test.test "record with field destructured to multi-line pattern"
                (\() ->
                    """module A exposing (..)
a { zero = n0,
  one = (n1 -- 0
  -- 1
  )
  } =
    0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a
    { zero = n0
    , one =
        (n1
         -- 0
         -- 1
        )
    }
    =
    0
"""
                )
            , Test.test "consecutive comments in empty list"
                (\() ->
                    """module A exposing (..)
a =
    case [] of
        [ -- 0
         -- 1
         ] -> 0
        _ -> 1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    when [] is
        [-- 0
         -- 1
        ]
        ->
            0

        _ ->
            1
"""
                )
            , Test.test "consecutive collapsible comments in empty list"
                (\() ->
                    """module A exposing (..)
a =
    case [] of
        [ {- 0 -}
         {- 1 -}
         ] -> 0
        _ -> 1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    when [] is
        [{- 0 -} {- 1 -}] ->
            0

        _ ->
            1
"""
                )
            , Test.test "consecutive comments before first list element"
                (\() ->
                    """module A exposing (..)
a =
    case [] of
        [ -- 0
         -- 1
         0 ] -> 0
        _ -> 1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    when [] is
        [ -- 0
          -- 1
          0
        ]
        ->
            0

        _ ->
            1
"""
                )
            , Test.test "comments between list elements"
                (\() ->
                    """module A exposing (..)
a =
    case [] of
        [ 0, -- zero
          0 ] -> 0
        _ -> 1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    when [] is
        [ 0
        , -- zero
          0
        ]
        ->
            0

        _ ->
            1
"""
                )
            , Test.test "consecutive comments between list elements"
                (\() ->
                    """module A exposing (..)
a =
    case [] of
        [ 0, -- 0
         -- 1
         0 ] -> 0
        _ -> 1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    when [] is
        [ 0
        , -- 0
          -- 1
          0
        ]
        ->
            0

        _ ->
            1
"""
                )
            , Test.test "consecutive comments collapsible before single-line list element"
                (\() ->
                    """module A exposing (..)
a =
    case [] of
        [ 0, {- 0 -}
         {- 1 -}
         0 ] -> 0
        _ -> 1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    when [] is
        [ 0, {- 0 -} {- 1 -} 0 ] ->
            0

        _ ->
            1
"""
                )
            , Test.test "consecutive comments collapsible before single-line list element among multi-line list elements"
                (\() ->
                    """module A exposing (..)
a =
    case [] of
        [ {--} 0, {- 0 -}
         {- 1 -}
         0 ] -> 0
        _ -> 1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    when [] is
        [ {--}
          0
        , {- 0 -} {- 1 -} 0
        ]
        ->
            0

        _ ->
            1
"""
                )
            , Test.test "consecutive comments collapsible before multi-line list element"
                (\() ->
                    """module A exposing (..)
a =
    case [] of
        [ 0, {- 0 -} {- 1 -} (0{--}) ] -> 0
        _ -> 1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    when [] is
        [ 0
        , {- 0 -} {- 1 -}
          (0
           {--}
          )
        ]
        ->
            0

        _ ->
            1
"""
                )
            , Test.test "comments after list elements"
                (\() ->
                    """module A exposing (..)
a =
    case [] of
        [ 0, 0
          -- zero
         ] -> 0
        _ -> 1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    when [] is
        [ 0
        , 0
          -- zero
        ]
        ->
            0

        _ ->
            1
"""
                )
            , Test.test "consecutive comments after list elements"
                (\() ->
                    """module A exposing (..)
a =
    case [] of
        [ 0, 0
          -- 0
          -- 1
          ] -> 0
        _ -> 1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    when [] is
        [ 0
        , 0
          -- 0
          -- 1
        ]
        ->
            0

        _ ->
            1
"""
                )
            , Test.test ":: with multiple patterns as tail, consecutive comments before rightest tail pattern"
                (\() ->
                    """module A exposing (..)
a =
    case [] of
        b :: Just c :: {- 0 -} {--} d ->
            b

        _ ->
            0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    when [] is
        b
            :: (Just c)
            :: {- 0 -}
               {--}
               d
        ->
            b

        _ ->
            0
"""
                )
            , Test.test ":: with multiple patterns as tail, consecutive comments collapsible before single-line tail pattern"
                (\() ->
                    """module A exposing (..)
a =
    case [] of
        b :: Just c :: {- 0 -} {- 1 -} d ->
            b

        _ ->
            0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    when [] is
        b :: (Just c) :: {- 0 -} {- 1 -} d ->
            b

        _ ->
            0
"""
                )
            , Test.test ":: with multiple patterns as tail, consecutive comments collapsible before multi-line tail pattern"
                (\() ->
                    """module A exposing (..)
a =
    case [] of
        b :: Just c {- 0 -} {- 1 -} :: (d --
         ) ->
            b

        _ ->
            0"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    when [] is
        b
            :: (Just c)
            :: {- 0 -} {- 1 -}
               (d
                --
               )
        ->
            b

        _ ->
            0
"""
                )
            ]
        , Test.describe "comment"
            [ Test.test "module level {--} has new lines in front if preceded by other comments"
                (\() ->
                    """module A exposing (..)
a =
    0
--
{--}
--
b =
    1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    0



--


{--}
--


b =
    1
"""
                )
            , Test.test "module level {--} eats linebreaks after if not followed by comments"
                (\() ->
                    """module A exposing (..)
a =
    0
--
{--}
b =
    1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    0



--


{--}
b =
    1
"""
                )
            , Test.test "{- -} trimmed same-line"
                (\() ->
                    """module A exposing (..)
a =
    0
{-x
-}
b =
    1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    0



{- x -}


b =
    1
"""
                )
            , Test.test "{- -} only one linebreak"
                (\() ->
                    """module A exposing (..)
a =
    0
{-
-}
b =
    1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    0



{-  -}


b =
    1
"""
                )
            , Test.test "{- -} multiple linebreaks and spaces"
                (\() ->
                    """module A exposing (..)
a =
    0
{-

-}
b =
    1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    0



{-

-}


b =
    1
"""
                )
            , Test.test "{- -} multiple linebreaks, some characters and spaces"
                (\() ->
                    """module A exposing (..)
a =
    0
{-   x

a-}
b =
    1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    0



{- x

   a
-}


b =
    1
"""
                )
            , Test.test "{- -} multiple linebreaks, indented characters and spaces"
                (\() ->
                    """module A exposing (..)
a =
    0
{-   x
        
   a-}
b =
    1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    0



{- x

   a
-}


b =
    1
"""
                )
            , Test.test "{- -} multiple linebreaks, multiple differently-indented characters and spaces"
                (\() ->
                    """module A exposing (..)
a =
    0
{-   x
        
    a
        b-}
b =
    1"""
                        |> expectPrintedAs
                            """module A exposing (..)


a =
    0



{- x

   a
       b
-}


b =
    1
"""
                )
            ]
        , Test.describe "full module samples"
            [ Test.test "example from readme"
                (\() ->
                    """
module   Sample  exposing(...)
plus2 (n)= {- this adds 2-} n
+
2
"""
                        |> GrenParserLenient.run GrenParserLenient.module_
                        |> Maybe.map
                            (\syntaxModule ->
                                syntaxModule
                                    |> GrenPrint.module_
                                    |> GrenPrint.toString
                            )
                        |> Expect.equal
                            (Just
                                """module Sample exposing (..)


plus2 n =
    {- this adds 2 -}
    n
        + 2
"""
                            )
                )
            , Test.test "example flight booker"
                (\() ->
                    """module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, option, select, text)
import Html.Attributes exposing (disabled, id, style, value)
import Html.Events as Event exposing (onInput)
import Task


main : Program {} Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type FlightType
    = OneWay
    | ReturnFlight


flightTypeToString : FlightType -> String
flightTypeToString flightType =
    when flightType is
        OneWay ->
            "One-way flight"

        ReturnFlight ->
            "Return flight"


flightTypeFromString : String -> FlightType
flightTypeFromString str =
    when str is
        "Return flight" ->
            ReturnFlight

        _ ->
            OneWay


type alias Model =
    { flightType : FlightType
    , input1 : String
    , input2 : String
    , date1 : Maybe Date
    , date2 : Maybe Date
    }


init : Model
init =
    let
        date =
            { year = 2022
            , month = 6
            , day = 14
            }
    in
    { flightType = OneWay
    , input1 = formatDate date
    , input2 = formatDate date
    , date1 = Just date
    , date2 = Just date
    }


type Msg
    = FlightTypeChanged String
    | Input1Changed String
    | Input2Changed String


update : Msg -> Model -> Model
update msg model =
    when msg is
        FlightTypeChanged asString ->
            { model | flightType = flightTypeFromString asString }

        Input1Changed str ->
            { model
                | input1 = str
                , date1 = parseDate str
            }

        Input2Changed str ->
            { model
                | input2 = str
                , date2 = parseDate str
            }



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "width" "100px"
        , style "padding" "40px"
        ]
        [ flightTypeSelector
        , dateInput
            { id = "departure"
            , value = model.input1
            , date = model.date1
            , changeMsg = Input1Changed
            , isDisabled = False
            }
        , dateInput
            { id = "arrival"
            , value = model.input2
            , date = model.date2
            , changeMsg = Input2Changed
            , isDisabled = model.flightType == OneWay
            }
        , bookButton
            { isDisabled = checkIfDisabled model.flightType model.date1 model.date2 }
        ]


flightTypeSelector : Html Msg
flightTypeSelector =
    select
        [ id "flight-type"
        , onInput FlightTypeChanged
        ]
        [ option [] [ text (flightTypeToString OneWay) ]
        , option [] [ text (flightTypeToString ReturnFlight) ]
        ]


type alias DateInputConfig =
    { id : String
    , value : String
    , date : Maybe Date
    , changeMsg : String -> Msg
    , isDisabled : Bool
    }


dateInput : DateInputConfig -> Html Msg
dateInput config =
    input
        [ id config.id
        , value config.value
        , onInput config.changeMsg
        , disabled config.isDisabled
        , style "background-color"
            (if config.date == Nothing then
                "red"

             else
                "white"
            )
        ]
        []


bookButton : { isDisabled : Bool } -> Html Msg
bookButton { isDisabled } =
    button
        [ id "book"
        , disabled isDisabled
        ]
        [ text "Book" ]



-- Helper functions


type alias Date =
    { day : Int
    , month : Int
    , year : Int
    }


parseDate : String -> Maybe Date
parseDate dateString =
    if String.count dateString /= 10 then
        Nothing

    else
        let
            parts =
                dateString
                    |> String.split "."
                    |> Array.mapAndKeepJust String.toInt
        in
        when parts is
            [ day, month, year ] ->
                Just
                    { day = day
                    , month = month
                    , year = year
                    }

            _ ->
                Nothing


formatDate : Date -> String
formatDate date =
    String.fromInt date.day
        ++ "."
        ++ String.fromInt date.month
        ++ "."
        ++ String.fromInt date.year


compareDate : Date -> Date -> Order
compareDate date1 date2 =
    when compare date1.year date2.year is
        EQ ->
            when compare date1.month date2.month is
                EQ ->
                    compare date1.day date2.day

                monthComparison ->
                    monthComparison

        yearComparison ->
            yearComparison


checkIfDisabled : FlightType -> Maybe Date -> Maybe Date -> Bool
checkIfDisabled flightType maybeDate1 maybeDate2 =
    when flightType is
        OneWay ->
            maybeDate1 == Nothing

        ReturnFlight ->
            let
                compare =
                    Maybe.map2 compareDate
                        maybeDate1
                        maybeDate2
            in
            when compare is
                Just GT ->
                    True

                _ ->
                    False
"""
                        |> expectPrintedAsSame
                )
            ]
        ]


expectPrintedAs : String -> String -> Expect.Expectation
expectPrintedAs expected source =
    case source |> GrenParserLenient.run GrenParserLenient.module_ of
        Nothing ->
            Expect.fail ("failed to parse actual source:\n" ++ source)

        Just parsed ->
            let
                printed : String
                printed =
                    parsed
                        |> GrenPrint.module_
                        |> GrenPrint.toString
            in
            if printed == expected then
                Expect.pass

            else
                Expect.fail
                    ("actual printed source is\n\n"
                        ++ (printed |> String.lines |> List.take 16 |> String.join "\n")
                        ++ "...\n\nbut the expected source differs in lines\n"
                        ++ (List.map2
                                (\actualLine expectedLine -> { actual = actualLine, expected = expectedLine })
                                (printed |> String.lines)
                                (expected |> String.lines)
                                |> List.indexedMap
                                    (\i lines ->
                                        if lines.actual == lines.expected then
                                            Nothing

                                        else
                                            Just ((i |> String.fromInt) ++ ": " ++ lines.actual)
                                    )
                                |> List.filterMap identity
                                |> List.take 10
                                |> String.join "\n"
                           )
                    )


expectPrintedAsSame : String -> Expect.Expectation
expectPrintedAsSame alreadyFormattedSource =
    alreadyFormattedSource |> expectPrintedAs alreadyFormattedSource
