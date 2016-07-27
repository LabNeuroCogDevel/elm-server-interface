module Types.ContactInfo.JsonEncoders exposing (..)


import Json.Encode exposing (..)
import Utils.JsonEncoders exposing (..)
import Types.ContactInfo exposing (..)

import Maybe exposing (Maybe, withDefault)
import String exposing (join)

import List as L


contactEncoder : Int -> String -> String -> Encoder Contact
contactEncoder pid relation who con =
  object
    [ ("cid",int con.cid)
    , ("pid",int pid)
    , ("ctype",string con.contactType)
    , ("cvalue",string con.content)
    , ("relation",string relation)
    , ("who",string who)
    , ("nogood",bool con.notGood)
    , ("note",string con.notes)
    ]


contactInfoEncoder : Encoder ContactInfo
contactInfoEncoder ci = 
  list
    <| L.map
        (contactEncoder ci.belongsTo ci.relation ci.name)
        ci.contacts

