module Types.Person.JsonDecoders exposing (..)

import Json.Decode exposing (..)
import Json.Encode exposing (..)
import Utils.JsonDecoders exposing (..)
import Utils.JsonEncoders exposing (..)
import Types.Person exposing (..)
import Types.Person.Sex exposing (..)
import Types.Person.Hand exposing (..)

import Maybe exposing (..)
import String exposing (join,concat)



decode : Decoder Person
decode  = succeed Person
  |: ( "pid"        :=  int    )
  |: ( "fname" := string )
  |: ( "lname" := string )
  |: ( "dob"   :=  date )
  |: ( "sex"   :=  sex )
  |: ( "hand"  :=  hand )
  |: ( maybe <| "adddate"    :=  date )
  |: ( maybe <| "source"     :=  string )
  |: ( map (withDefault 0.0) <| maybe <| "curage"       :=  float )
  |: ( maybe <| "lastvisit"  :=  date )
  |: ( map (withDefault 0) <| maybe <| "numvisits"  :=  int )
  |: ( map (withDefault 0) <| maybe <| "nstudies"   :=  int )
  |: ( map (withDefault 0) <| maybe <| "ndrops"     :=  int )
  |: ( map (withDefault []) <| maybe <| "ids"        :=  stringList )
  |: ( map (withDefault []) <| maybe <| "studies"        :=  stringList )
  |: ( map (withDefault []) <| maybe <| "visittypes"        :=  stringList )
  |: ( maybe <| "maxdrop"    := string )


encode : Encoder Person
encode p =
  object 
    [ ("fname", string p.fname )
    , ("lname", string p.lname )
    , ("dob", dateEncoder p.dob )
    , ("sex", sexEncoder p.sex )
    , ("hand", handEncoder p.hand )
    , ("adddate", withDefault null <| map dateEncoder <| p.adddate )
    , ("source", withDefault null <| map string <| p.source )
    ]


