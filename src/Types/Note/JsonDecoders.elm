module Types.Note.JsonDecoders exposing (..)

import Types.Note exposing (..)
import Json.Decode exposing (..)
import Utils.JsonDecoders exposing (..)

import Maybe exposing (Maybe, withDefault)
import String exposing (join,concat)

{--
type alias Note =
  { nid      : Int
  , pid      : Int
  , ra       : String
  , ndate    : Maybe Time
  , dropnote : Bool
  , note     : String
  }
--}

noteDecoder : Decoder Note
noteDecoder = succeed Note
  |: ("nid"      := int)
  |: ("pid"      := int)
  |: ("ra"       := string)
  |: (maybe <| "ndate" := date)
  |: ("dropnote" := bool)
  |: ("note"     := string)


