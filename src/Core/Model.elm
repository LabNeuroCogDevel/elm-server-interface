module Core.Model exposing (..)

import Pages.People.Model as People
import Nav.Routes exposing (Route,Query)
import Nav.Pages exposing (Page)
import Nav.RQ exposing (RQ)
import Hop.Types exposing (Location)

type alias Model =
  { routeQuery : RQ
  , peopleModel : People.Model
  , errorMsg : String
  }

type Msg
  = NoOp
  | NavigateTo (Maybe Route) (Maybe Query)
  | PeopleMsg People.Msg

