module Gren.Syntax.ModuleName exposing
    ( ModuleName
    , encode, decoder
    )

{-| This syntax represents the module names in Gren. These can be used for imports, module names (duh), and for qualified access.
For example:

    module Gren.Syntax.ModuleName ...

    import Foo.Bar ...

    import ... as Something

    My.Module.something

    My.Module.SomeType


## Types

@docs ModuleName


## Serialization

@docs encode, decoder

-}

import Json.Decode
import Json.Encode


{-| Base representation for a module name
-}
type alias ModuleName =
    List String



-- Serialization


{-| Encode a `ModuleName` syntax element to JSON.
-}
encode : ModuleName -> Json.Encode.Value
encode =
    Json.Encode.list Json.Encode.string


{-| JSON decoder for a `ModuleName` syntax element.
-}
decoder : Json.Decode.Decoder ModuleName
decoder =
    Json.Decode.list Json.Decode.string
