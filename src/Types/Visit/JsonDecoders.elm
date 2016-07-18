module Types.Visit.JsonDecoders exposing (..)

import Types.Visit exposing (..)
import Json.Decode exposing (..)
import Utils.JsonDecoders exposing (..)

import Maybe exposing (Maybe, withDefault)
import String exposing (join,concat)



visitDecoder : Decoder Visit
visitDecoder = succeed Visit
  |: ("vid"        := int)
  |: ("pid"        := int)
  |: ("vtype"      := string)
  |: (maybe <| "vscore" := float)
  |: (maybe <| "dur_hr"  := float)
  |: ("age"        := float)
  |: (maybe <| "vtimestamp" := date)
  |: (maybe <| "visitno" := int)
  |: (maybe <| "googleuri" := string)
  |: ("status"     := string)




