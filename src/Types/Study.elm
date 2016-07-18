module Types.Study exposing (..)


type alias Study =
  { name : String             -- "study"
  , grantName : String        -- "grantname"
  , cohorts : List String     -- "cohorts"
  , visitTypes : List String  -- "visit_types"
  }

