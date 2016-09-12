module Types.Visit.JsonEncoders exposing (..)

import Types.Note exposing (..)
import Utils.JsonEncoders exposing (..)

import Maybe exposing (..)
import Json.Encode exposing (..)

visitEncoder : Encoder Visit
visitEncoder v = 
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
