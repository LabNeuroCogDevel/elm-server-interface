module Types.VisitTask.JsonEncoders exposing (..)

import Types.VisitTask exposing (..)

import Utils.JsonEncoders exposing (..)
import Json.Encode exposing (..)

import List as L

visitTaskEncoder : Encoder VisitTask
visitTaskEncoder vt =
  object 
    [ ("vtid", int vt.vtid)
    , ("vid", int vt.vid)
    , ("task", string vt.task)
    , ("measures", object <| L.map (\(x,y) -> (x, float y)) vt.measures)
    , ("files", list <| L.map string vt.files)
    ]

