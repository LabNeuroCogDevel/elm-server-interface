module Core.Update exposing (..)

import Core.Model exposing (..)
import Platform.Cmd exposing (..)
import Utils exposing (..)
import Types.Either exposing (..)

import Hop exposing (makeUrl)
import Nav.Routes exposing (Route, routeToPath, routerConfig)
import Nav.Operations exposing (Operation (..))
import Nav.RQ exposing (RQ, getQueryRQ)
import Components.Search.Model exposing (parseSearch, parseOrder)
import Pages.People.Search exposing (peopleKeyInfo)

import Navigation
import Result

import String as S
import Nav.Routes as R
import Hop.Types as H
import Core.HttpCmds as C
import Dict as D
import Maybe as M
import Utils.Navigation as UNav
import Pages.People.HttpCmds as PC
import Pages.People as People
import Pages.Studies as Studies
import Pages.Visits as Visits
import Pages.People.Model as PM
import Pages.Studies.Model as SM
import Pages.Visits.Model as VM

import Pages.Login as Login
import Pages.Login.Model as LM



init : RQ -> (Model, Cmd Msg)
init rq = 
  let
    (pm, pcmds) = People.init rq
    (sm, scmds) = Studies.init rq
    (vm, vcmds) = Visits.init rq
    (lm, lcmds) = Login.init rq

    (model, morecmds) = update (getMessage rq)
      { routeQuery = rq
      , peopleModel = pm
      , studiesModel = sm
      , visitsModel = vm
      , loginModel = lm
      , errorMsg = ""
      }
    cmd = batch [ Cmd.map PeopleMsg pcmds
                , Cmd.map StudiesMsg scmds
                , Cmd.map VisitsMsg vcmds
                , morecmds
                , Cmd.map PeopleMsg 
                    <| PC.getPeople 
                         (parseSearch peopleKeyInfo
                           <| M.withDefault ""
                           <| D.get "search"
                           <| getQueryRQ rq
                         )
                         (parseOrder peopleKeyInfo
                           <| M.withDefault ""
                           <| D.get "order"
                           <| getQueryRQ rq
                         )
                         25
                    <| M.withDefault 1 
                    <| (D.get "page" (getQueryRQ rq))
                       `M.andThen`
                       (Result.toMaybe << S.toInt)
                ]
  in 
    (model,cmd)



update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    NoOp ->
      (model, Cmd.none)

    NavigateTo route query -> 
      let
        rq = model.routeQuery
        command = UNav.navigateTo
                    model.routeQuery
                    route
                    query
      in
        (model,command)

    PeopleMsg mg ->
      let
        (newModel, cmds) = People.update mg model.peopleModel
      in
        ( { model
          | peopleModel = newModel
          }
        , Cmd.map PeopleMsg cmds
        )

    StudiesMsg mg ->
      let
        (newModel, cmds) = Studies.update mg model.studiesModel
      in
        ( { model
          | studiesModel = newModel
          }
        , Cmd.map StudiesMsg cmds
        )

    VisitsMsg mg ->
      let
        (newModel, cmds) = Visits.update mg model.visitsModel
      in
        ( { model
          | visitsModel = newModel
          }
        , Cmd.map VisitsMsg cmds
        )

    LoginMsg mg ->
      let
        (newModel, cmds) = Login.update mg model.loginModel
      in
        ( { model
          | loginModel = newModel
          }
        , Cmd.map LoginMsg cmds
        )


getMessage : RQ -> Msg
getMessage route = 
  case route of
    R.NotFound ->
      NoOp
    R.Root ->
      NavigateTo (Just (R.defaultPeople All)) (Just (D.singleton "page" "1"))
    R.People _ operation ->
      PeopleMsg <| case operation of
        New -> PM.NoOp
        View n -> PM.ViewPerson n
        Edit n -> PM.EditPerson n
        Delete _ -> PM.NoOp
        All -> PM.NoOp
        Cancel -> PM.CancelEdit

    R.Studies operation ->
      StudiesMsg <| SM.CrudOp operation

    R.Visits operation ->
      VisitsMsg <| VM.CrudOp operation

    R.Login operation ->
      LoginMsg <| LM.CrudOp operation
        
        


urlUpdate : RQ -> Model -> ( Model, Cmd Msg )
urlUpdate rq model =
  let
    (newPeopleModel, pCmd) = People.urlUpdate rq model.peopleModel
    (newStudiesModel, sCmd) = Studies.urlUpdate rq model.studiesModel
    -- login does not update on url change
    -- does visit?

    (newModel, cmd') = 
      update 
        (getMessage rq)
        { model
        | routeQuery = rq
        , peopleModel = newPeopleModel
        , studiesModel = newStudiesModel
        , errorMsg = ""
        }
  in
    ( newModel
    , batch
        [ Cmd.map PeopleMsg pCmd
        , Cmd.map StudiesMsg sCmd
        , cmd'
        ]
    )

