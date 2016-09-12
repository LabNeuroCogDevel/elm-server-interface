module Types.Note.JsonDecoders exposing (..)

import Types.Note exposing (..)
import Utils.JsonDecoders exposing (..)
import Utils.JsonEncoders exposing (..)
import Maybe exposing (..)
import Json.Encode exposing (..)
import Json.Decode exposing (..)

import Maybe exposing (Maybe, withDefault)
import String exposing (join,concat)

decode : Decoder Note
decode = succeed Note
  |: ("nid"      := int)
  |: ("pid"      := int)
  |: ("ra"       := string)
  |: (maybe <| "ndate" := date)
  |: ("dropnote" := bool)
  |: ("note"     := string)

encode : Encoder Note
encode note = 
  object 
    [ ("pid"      , int note.pid)
    , ("ra"       , string note.ra)
    , ("ndate"    , withDefault null <| map dateEncoder <| note.ndate)
    , ("dropnote" , bool note.dropnote)
    , ("note"     , string note.note)
    ]
