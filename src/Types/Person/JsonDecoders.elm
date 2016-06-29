module Types.Person.JsonDecoders exposing (..)

import Json.Decode exposing (..)
import Utils.JsonDecoders exposing (..)

import Maybe exposing (Maybe, withDefault)
import Types.Person exposing (Person)
import String exposing (join)

{--

type alias Person =
  { pid : Pid
  , fullname : Maybe String
  , dob : Maybe String
  , sex : Maybe String
  , hand : Maybe String
  , adddate : Maybe String
  , source : Maybe String
  , curage : Float
  , lastvisit : Maybe String
  , numvisits : Int
  , nstudies : Int
  , ndrops : Int
  , ids : List String
  , studies : List String
  , visittypes : List String
  , maxdrop : Maybe String
  }

--}

memberDecoderLarge : Decoder Person
memberDecoderLarge  = succeed Person
  |: ( "pid"        :=  int    )
  |: ( maybe <| "fullname" := string )
{--
  |: ( maybe <| ("fname"   :=  string )
               `andThen`
               (\fname -> ("lname" := string)
                          `andThen`
                          (\lname -> succeed
                                     <| join " " [fname,lname])))
--}
  |: ( maybe <| "dob"        :=  string )
  |: ( maybe <| "sex"        :=  string )
  |: ( maybe <| "hand"       :=  string )
  |: ( maybe <| "adddate"    :=  string )
  |: ( maybe <| "source"     :=  string )
  |: ( map (withDefault 0.0) <| maybe <| "curage"       :=  float )
  |: ( maybe <| "lastvisit"  :=  string )
  |: ( map (withDefault 0) <| maybe <| "numvisits"  :=  int )
  |: ( map (withDefault 0) <| maybe <| "nstudies"   :=  int )
  |: ( map (withDefault 0) <| maybe <| "ndrops"     :=  int )
  |: ( map (withDefault []) <| maybe <| "ids"        :=  stringList )
  |: ( map (withDefault []) <| maybe <| "studies"        :=  stringList )
  |: ( map (withDefault []) <| maybe <| "visittypes"        :=  stringList )
  |: ( maybe <| "maxdrop"    := string )

