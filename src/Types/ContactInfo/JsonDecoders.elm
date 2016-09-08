module Types.ContactInfo.JsonDecoders exposing (..)


import Json.Decode exposing (..)
import Utils.JsonDecoders exposing (..)
import Types.ContactInfo exposing (..)

import Maybe exposing (Maybe, withDefault)
import String exposing (join)

{--
 -- These are the types we want to build
type alias Contact = 
  { cid : Cid
  , contactType : String
  , content : String
  , notGood : Bool
  , notes : String
  }

type alias ContactInfo =
  { belongsTo : Int -- PID of person this info belongs to
  , relation : String
  , name : String
  , lastContact : Maybe String
  , contacts : List Contact
  }
--}

contactDecoder : Decoder Contact
contactDecoder = succeed Contact
  |: ( "cid"    := int )
  |: ( "ctype"  := string )
  |: ( "cvalue" := string )
  |: ( "nogood" := bool )
  |: ( "notes"  := map (withDefault "") stringNull )

contactInfoDecoder : Decoder ContactInfo
contactInfoDecoder = succeed ContactInfo
  |: ( "pid"         := int )
  |: ( "relation"    := string )
  |: ( "who"        := string )
  |: ( "lastcontact" := stringNull )
  |: ( "contacts"    := list contactDecoder )

ciListDecoder : Decoder (List ContactInfo)
ciListDecoder = list contactInfoDecoder
