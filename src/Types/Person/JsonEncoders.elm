module Types.Person.JsonEncoders exposing (..)

import Types.Person exposing (..)
import Types.Person.Sex exposing (..)
import Types.Person.Hand exposing (..)

import Utils.JsonEncoders exposing (..)

import Maybe exposing (..)
import Json.Encode exposing (..)



personEncoder : Encoder Person
personEncoder p =
  object 
    [ ("fname", string p.fname )
    , ("lname", string p.lname )
    , ("dob", dateEncoder p.dob )
    , ("sex", sexEncoder p.sex )
    , ("hand", handEncoder p.hand )
    , ("adddate", withDefault null <| map dateEncoder <| p.adddate )
    , ("source", withDefault null <| map string <| p.source )
    ]


