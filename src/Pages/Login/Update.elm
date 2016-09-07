module Pages.Login.Update exposing (..)

import Types.Login       exposing (..)
import Pages.Login.HttpCmds    as HC
import Pages.Login.Model exposing (..)
import Nav.RQ exposing (..)

import Nav.Operations exposing (Operation (..))

import Pages.Login.HttpCmds as HttpCmds


init : RQ -> (Model, Cmd Msg)
init rq =
  ( { cred = newCred , error = "" } 
  , Cmd.none
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  CrudOp operation ->
    case operation of
      All -> ( model , HC.fetchAuthToken model.cred )

      Cancel    -> ( model , Cmd.none)
      View id   -> ( model , Cmd.none)
      Edit id   -> ( model , Cmd.none)
      Delete id -> ( model , Cmd.none)
      Nav.Operations.New       -> ( model , Cmd.none)




  Error errstring ->
      ( { model | error = errstring} -- , cred = newCred 
      , Cmd.none
      )
   
  ReceiveLogin cred ->
      ( { model | cred = cred, error = "" }
      , Cmd.none
      )

  NoOp -> ( model , Cmd.none)
   


urlUpdate : RQ -> Model -> (Model, Cmd Msg)
urlUpdate rq model = 
  ( model
  , Cmd.none
  )

