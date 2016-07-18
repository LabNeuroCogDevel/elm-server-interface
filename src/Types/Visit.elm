module Types.Visit exposing (..)



type alias Visit =
  { vid       : Int
  , pid       : Int
  , vtype     : String
  , score     : Maybe Float
  -- , durHr     : ?
  , age       : Float
  , timestamp : Maybe Time
  , visitno   : Maybe Int
  , googleuri : Maybe String
  , status    : String
  }

