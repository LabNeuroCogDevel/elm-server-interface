module JsonDecoders exposing (..)

import Json.Decode exposing (..)
import Maybe exposing (Maybe, withDefault)
import Person exposing (Person)
import String exposing (join)

apply : Decoder (a -> b) -> Decoder a -> Decoder b
apply decF decA = decF `andThen` \f -> map f decA

(|:) = apply

maybeNull : Decoder a -> Decoder (Maybe a)
maybeNull dec = oneOf [ null Nothing, map Just dec ]

stringList : Decoder (List String)
stringList = oneOf [ null [], list string ]

stringNull : Decoder (Maybe String)
stringNull = maybeNull string

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

