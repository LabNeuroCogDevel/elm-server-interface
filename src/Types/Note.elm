module Types.Note exposing (..)

type alias Note =
  { nid : Int
  , pid : Int
  , ra : String
  , ndate : Maybe Time
  , dropnote : Bool
  , note : String
  }

