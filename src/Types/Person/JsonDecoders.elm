module Types.Person.JsonDecoders exposing (..)

import Json.Decode exposing (..)
import Utils.JsonDecoders exposing (..)
import Types.Person exposing (..)
import Types.Person.Sex exposing (..)
import Types.Person.Hand exposing (..)

import Maybe exposing (Maybe, withDefault)
import String exposing (join,concat)



memberDecoderLarge : Decoder Person
memberDecoderLarge  = succeed Person
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
  |: succeed Nothing
  |: succeed Nothing

