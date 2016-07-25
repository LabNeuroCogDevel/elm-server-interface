module Types.Person.Json exposing (..)

import Json.Decode exposing (..)
import Json.Encode exposing (..)
import Utils.JsonDecoders exposing (..)
import Utils.JsonEncoders exposing (..)
import Types.Person exposing (..)
import Types.Person.JsonDecoders exposing (..)
import Types.Person.JsonEncoders exposing (..)



decode : Decoder Person
decode  = memberDecoderLarge

encode : Encoder Person
encode = personEncoder

