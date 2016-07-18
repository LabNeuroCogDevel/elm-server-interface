module Types.VisitTask.JsonDecoders exposing (..)

import Types.VisitTask exposing (..)

import Utils.JsonDecoders exposing (..)
import Json.Decode exposing (..)


visitTaskDecode : Decoder VisitTask
visitTaskDecode = succeed VisitTask
  |: ("vtid" := int)
  |: ("vid" := int)
  |: ("task" := string)
  |: ("measures" := oneOf [ null [], keyValuePairs float ])
  |: ("files" := stringList)


