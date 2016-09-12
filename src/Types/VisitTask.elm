module Types.VisitTask exposing (..)


type alias VisitTask =
  { vtid : Int                     -- "vtid"
  , vid : Int                      -- "vid"
  , task : String                  -- "task"
  , measures : List (String,Float) -- "measures"
  , files : List String            -- "files"
  }
