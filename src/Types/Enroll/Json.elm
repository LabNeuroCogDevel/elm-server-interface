module Types.Enroll.Json exposing (..)

import Types.Enroll exposing (..)
import Json.Decode exposing (..)
import Utils.JsonDecoders exposing (..)
import Json.Encode exposing (..)
import Utils.JsonEncoders exposing (..)


decode : Decoder Enroll
decode = succeed Enroll
  |: ("eid" := int)
  |: ("pid" := int)
  |: ("etype" := string)
  |: ("id" := string)
  |: ("edate" := date)


encode : Encoder Enroll
encode e =
  object
    [ ("pid", int e.pid)
    , ("etype", string e.etype)
    , ("id", string e.id)
    , ("edate", dateEncoder e.date)
    ]
