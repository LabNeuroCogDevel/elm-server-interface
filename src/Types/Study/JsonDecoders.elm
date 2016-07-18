module Types.Study.JsonDecoders exposing (..)

import Types.Study exposing (..)
import Utils.JsonDecoders exposing (..)
import Json.Decode exposing (..)

import List as L

studyDecoder : Decoder Study
studyDecoder = succeed Study
  |: ("study" := string)
  |: ("grantname" := string)
  |: ("cohorts" := stringList)
  |: ("visit_types" := stringList)




