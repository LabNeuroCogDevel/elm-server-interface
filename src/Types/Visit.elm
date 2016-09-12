module Types.Visit exposing (..)

import ISO8601 exposing (Time)


type alias Visit =
  { vid       : Int          --
  , pid       : Int          --
  , vtype     : String       --
  , score     : Maybe Float  --
  , durHr     : Maybe Float  --
  , age       : Float        --
  , timestamp : Maybe Time   --
  , visitno   : Maybe Int    --
  , googleuri : Maybe String --
  , status    : String       --
  }

newVisit : Int -> Visit
newVisit n = 
  { vid = 0
  , pid = n
  , vtype = ""
  , score = Nothing
  , durHr = Nothing
  , age = 0.0
  , timestamp = Nothing
  , visitno = Nothing
  , googleuri = Nothing
  , status = ""
  }
