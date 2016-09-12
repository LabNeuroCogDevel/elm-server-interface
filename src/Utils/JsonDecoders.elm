module Utils.JsonDecoders exposing (..)

import Json.Decode exposing (..)
import ISO8601 exposing (..)

apply : Decoder (a -> b) -> Decoder a -> Decoder b
apply decF decA = decF `andThen` \f -> map f decA

(|:) = apply

maybeNull : Decoder a -> Decoder (Maybe a)
maybeNull dec = oneOf [ null Nothing, map Just dec ]

nullOrList : Decoder a -> Decoder (List a)
nullOrList dec = oneOf [ null [], list dec ]

stringList : Decoder (List String)
stringList = nullOrList string

stringNull : Decoder (Maybe String)
stringNull = maybeNull string

date : Decoder Time
date = 
  customDecoder string fromString
