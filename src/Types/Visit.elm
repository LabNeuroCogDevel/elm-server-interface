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

