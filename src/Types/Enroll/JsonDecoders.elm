module Types.Enroll.JsonDecoders exposing (..)

import Types.Enroll exposing (..)
import Json.Decode exposing (..)
import Utils.JsonDecoders exposing (..)

enrollDecoder : Decoder Enroll
enrollDecoder = succeed Enroll
  |: ("eid" := int)
  |: ("pid" := int)
  |: ("etype" := string)
  |: ("id" := string)
  |: ("edate" := date)
