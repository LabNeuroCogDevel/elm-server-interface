module Types.Visit.Json exposing (..)

import Types.Visit exposing (..)
import Json.Decode exposing (..)
import Json.Encode exposing (..)
import Utils.JsonDecoders exposing (..)
import Utils.JsonEncoders exposing (..)
import Types.Visit.JsonDecoders exposing (visit)
import Types.Visit.JsonEncoders exposing (visitEncoder)




decode : Decoder Visit
decode = visit

encode : Encoder Visit
encode = visitEncoder
