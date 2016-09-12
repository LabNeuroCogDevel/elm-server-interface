module Pages.Visits.Model exposing (..)

import Nav.Operations exposing (Operation)
import Types.Visit exposing (Visit)


type Msg 
  = CrudOp Operation
  | Error String
  | ReceiveVisits (List Visit)
  | NoOp

type alias Model =
  { visits : List Visit
  , error : String
  }
