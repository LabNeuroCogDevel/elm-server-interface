module Pages.Page.Model exposing (..)

import Nav.Operations exposing (Operation)
import Types.Study exposing (Study)


type Msg 
  = CrudOp Operation
  | ReceiveStudies (List Study)
  | NoOp

type alias Model =
  { studies : List Study
  }
