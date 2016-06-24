module Types.Person.JsonDecoders exposing (..)

import Json.Decode exposing (..)
import Utils.JsonDecoders exposing (..)

import Maybe exposing (Maybe, withDefault)
import Types.Person exposing (Person)
import String exposing (join)


memberDecoderLarge : Decoder Person
memberDecoderLarge  = succeed Person
 |: ( "pid"        :=  int    )
 |: ( maybe <| ("fname"   :=  string )
               `andThen`
               (\fname -> ("lname" := string)
                          `andThen`
                          (\lname -> succeed
                                     <| join " " [fname,lname])))
 |: ( maybe <| "dob"        :=  string )
 |: ( maybe <| "sex"        :=  string )
 |: ( maybe <| "hand"       :=  string )
 |: ( map (withDefault []) <| maybe <| "ids"        :=  stringList )

