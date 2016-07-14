module Types.Person.JsonEncoders exposing (..)

import Types.Person exposing (..)
import Utils.JsonEncoders exposing (..)

import Maybe exposing (..)
import Json.Encode exposing (..)


personEncoder : Encoder Person
personEncoder p =
  object 
    [ ("fname", withDefault null <| map string <| p.fname )
    , ("lname", withDefault null <| map string <| p.lname )
    , ("dob", withDefault null <| map string <| p.dob )
    , ("sex", withDefault null <| map string <| p.sex )
    , ("hand", withDefault null <| map string <| p.hand )
    , ("adddate", withDefault null <| map string <| p.adddate )
    , ("source", withDefault null <| map string <| p.source )
    ]


