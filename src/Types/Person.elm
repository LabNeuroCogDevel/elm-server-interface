module Types.Person exposing (..)

import Date exposing (Date)

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


type alias Person =
  { pid : Pid
  , fname : Maybe String
  , lname : Maybe String
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

new : Person
new = 
  { pid = 0
  , fname = Nothing
  , lname = Nothing
  , dob = Nothing
  , sex = Nothing
  , hand = Nothing
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

modifyPerson : Person -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Person
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


