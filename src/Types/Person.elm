module Types.Person exposing (..)

import Types.Person.Sex exposing (..)
import Types.Person.Hand exposing (..)

import Date exposing (Date)

import Types.Person.Sex as S
import Types.Person.Hand as H

type alias Pid = Int

{--
{ "pid":1                                      --Int
, "fullname":"Fake Person"                     --Maybe String          | Maybe String/String
, "dob":"1956-08-29"                           --Maybe String          | Date/String/Maybe String/Maybe Date
, "sex":"M"                                    --Maybe String          | String/Maybe String
, "hand":"L"                                   --Maybe String          | String/Maybe String
, "adddate":null                               --Maybe String          | Maybe Date/Maybe String
, "source":null                                --Maybe String
, "curage":59.8329911019849                    --Float                 | Float?
, "lastvisit":"2016-03-30T00:00:00"            --Maybe String          | Maybe DateTime/DateTime/String
, "numvisits":2                                --Int
, "nstudies":2                                 --Int
, "ndrops":0                                   --Int
, "ids":["9"]                                  --List String
, "studies":["MEGEmo", "SlotReward"]           --List String
, "visittypes":["Behavioral", "MEG"]           --List String
, "maxdrop":null                               --Maybe String
}
--}


type alias PersonFData = 
  { fname : String
  , lname : String
  , dob : Date
  , sex : Sex
  , hand : Hand
  , adddate : Maybe Date
  , source : Maybe String
  }

type alias Person =
  { pid : Pid
  , fname : String
  , lname : String
  , dob : Date
  , sex : Sex
  , hand : Hand
  , adddate : Maybe Date
  , source : Maybe String
  , curage : Float
  , lastvisit : Maybe Date
  , numvisits : Int
  , nstudies : Int
  , ndrops : Int
  , ids : List String
  , studies : List String
  , visittypes : List String
  , maxdrop : Maybe String
  }

new : Person
new = 
  { pid = 0
  , fname = ""
  , lname = ""
  , dob = Date.fromTime 0.0
  , sex = S.Unknown
  , hand = H.Unknown
  , adddate = Nothing
  , source = Nothing
  , curage = 0.0
  , lastvisit = Nothing
  , numvisits = 0
  , nstudies = 0
  , ndrops = 0
  , ids = []
  , studies = []
  , visittypes = []
  , maxdrop = Nothing
  }

modifyPerson : Person -> String -> String -> Date -> Sex -> Hand -> Maybe Date -> Maybe String -> Person
modifyPerson person fname lname dob sex hand adddate source =
  { person
  | fname = fname
  , lname = lname
  , dob = dob
  , sex = sex
  , hand = hand
  , adddate = adddate
  , source = source
  }


