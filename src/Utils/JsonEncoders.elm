module Utils.JsonEncoders exposing (..)

import Json.Encode exposing (..)

type alias Encoder a = a -> Value

code : Encoder a -> a -> String
code enc = prettyCode enc 0

prettyCode : Encoder a -> Int -> a -> String
prettyCode enc n val = encode n (enc val)

