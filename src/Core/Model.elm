module Core.Model exposing (..)

import Pages.People.Model as People
import Pages.Studies.Model as Studies
import Pages.Visits.Model as Visits
import Pages.Login.Model as Login
import Nav.Routes exposing (Route)
import Nav.Queries exposing (Query)
import Nav.RQ exposing (RQ)
import Hop.Types exposing (Location)

type alias Model =
  { routeQuery : RQ
  , peopleModel : People.Model
  , studiesModel : Studies.Model
  , visitsModel : Visits.Model
  , loginModel : Login.Model
  , errorMsg : String
  }

type Msg
  = NoOp
  | NavigateTo (Maybe Route) (Maybe Query)
  | PeopleMsg People.Msg
  | StudiesMsg Studies.Msg
  | VisitsMsg Visits.Msg
  | LoginMsg Login.Msg

