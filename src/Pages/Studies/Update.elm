module Pages.Studies.Update exposing (..)

import Pages.Studies.Model exposing (..)
import Nav.RQ exposing (..)

import Nav.Operations exposing (Operation (..))

import Pages.Studies.HttpCmds as HttpCmds


init : RQ -> (Model, Cmd Msg)
init rq =
  ( { studies = []
    }
  , Cmd.none
  )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  CrudOp operation ->
    case operation of
      All ->
        ( model
        , HttpCmds.fetchStudies
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

  ReceiveStudies studies ->
    ( { model 
      | studies = studies
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

