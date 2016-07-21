module Types.Visit.Json exposing (..)

import Types.Visit exposing (..)
import Json.Decode exposing (..)
import Json.Encode exposing (..)
import Utils.JsonDecoders exposing (..)
import Utils.JsonEncoders exposing (..)
import Maybe exposing (..)

import String exposing (join,concat)



decode : Decoder Visit
decode = succeed Visit
  |: ("vid"        := int)
  |: ("pid"        := int)
  |: ("vtype"      := string)
  |: (maybe <| "vscore" := float)
  |: (maybe <| "dur_hr"  := float)
  |: ("age"        := float)
  |: (maybe <| "vtimestamp" := date)
  |: (maybe <| "visitno" := int)
  |: (maybe <| "googleuri" := string)
  |: ("vstatus"     := string)



encode : Encoder Visit
encode v = 
  object
    [ ("pid", int v.pid)
    , ("vtype", string v.vtype)
    , ("vscore", withDefault null <| map float v.score)
    , ("dur_hr", withDefault null <| map float v.score)
    , ("age", float v.age)
    , ("vtimestamp", withDefault null <| map dateEncoder v.timestamp)
    , ("visitno", withDefault null <| map int v.visitno)
    , ("googleuri", withDefault null <| map string v.googleuri)
    , ("vstatus", string v.status)
    ]


