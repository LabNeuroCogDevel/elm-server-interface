module Types.Note.JsonEncoders exposing (..)

import Types.Note exposing (..)
import Utils.JsonEncoders exposing (..)

import Maybe exposing (..)
import Json.Encode exposing (..)


noteEncoder : Encoder Note
noteEncoder note = 
  object 
    [ ("pid"      , int note.pid)
    , ("ra"       , string note.ra)
    , ("ndate"    , withDefault null <| map dateEncoder <| note.ndate)
    , ("dropnote" , bool note.dropnote)
    , ("note"     , string note.note)
    ]

