module Types.Task exposing (..)


type alias Task =
  { name : String  -- "task"
  , desc : String  -- "tdesc"
  , measures : List String -- "measures"
  , files : List String -- "files", TODO type unclear
  , settings : List String -- "settings", TODO type unclear
  , modes : List String -- "modes"
  }

