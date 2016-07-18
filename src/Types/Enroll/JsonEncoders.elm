module Types.Enroll.JsonEncoders exposing (..)

import Types.Enroll exposing (..)

import Json.Encode exposing (..)
import Utils.JsonEncoders exposing (..)

enrollEncoder : Encoder Enroll
enrollEncoder e =
  object
    [ ("pid", int e.pid)
    , ("etype", string e.etype)
    , ("id", string e.id)
    , ("edate", dateEncoder e.date)
    ]


