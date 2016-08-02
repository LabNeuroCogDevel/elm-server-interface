module Components.Visits.Update exposing (..)

import Components.Visits.Model exposing (..)
import Nav.RQ exposing (..)

import Nav.Operations exposing (Operation (..))

import Pages.Visits.HttpCmds as HttpCmds


init : RQ -> (Model, Cmd Msg)
init rq =
  ( { visits = []
    , error = ""
    }
  , Cmd.none
  )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  CrudOp operation ->
    case operation of
      All ->
        ( model
        , HttpCmds.fetchVisits
        )

      New ->
        ( model
        , Cmd.none
        )

      Cancel ->
        ( model
        , Cmd.none
        )

      View id -> 
        ( model
        , Cmd.none
        )

      Edit id ->
        ( model
        , Cmd.none
        )

      Delete id ->
        ( model
        , Cmd.none
        )

  ReceiveVisits visits ->
    ( { model 
      | visits = visits
      }
    , Cmd.none
    )

  Error string ->
    ( { model 
      | error = string
      }
    , Cmd.none
    )

  NoOp ->
    ( model
    , Cmd.none
    )



urlUpdate : RQ -> Model -> (Model, Cmd Msg)
urlUpdate rq model = 
  ( model
  , Cmd.none
  )

