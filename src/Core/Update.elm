module Core.Update exposing (..)

import Core.Model exposing (..)
import Platform.Cmd exposing (..)
import Utils exposing (..)
import Types.Either exposing (..)

import Hop exposing (makeUrl)
import Nav.Routes exposing (Route, routeToPath, routerConfig)
import Nav.Operations exposing (Operation (..))
import Nav.RQ exposing (RQ, getQueryRQ)
import Components.Search.Model exposing (parseSearch)

import Navigation
import Result

import String as S
import Nav.Routes as R
import Hop.Types as H
import Core.HttpCmds as C
import Dict as D
import Maybe as M
import Pages.People.HttpCmds as PC
import Pages.People as People
import Pages.People.Model as PM



init : RQ -> (Model, Cmd Msg)
init rq = 
  let
    (pm, cmds) = People.init rq
    (model, morecmds) = update (getMessage rq)
      { routeQuery = rq
      , peopleModel = pm
      , errorMsg = ""
      }
    cmd = batch [ Cmd.map PeopleMsg cmds
                , morecmds
                , Cmd.map PeopleMsg 
                    <| PC.getPeople 
                         (parseSearch
                           <| M.withDefault ""
                           <| D.get "search"
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
        command = Utils.navigateTo
                    model.routeQuery
                    route
                    query
      in
        (model,command)

    PeopleMsg mg ->
      let
        (newModel, cmds) = People.update mg model.peopleModel
      in ({ model | peopleModel = newModel }, Cmd.map PeopleMsg cmds)


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
        


urlUpdate : RQ -> Model -> ( Model, Cmd Msg )
urlUpdate rq model =
  let
    (newPeopleModel, pCmd) = People.urlUpdate rq model.peopleModel
    (newModel, cmd') = 
      update 
        (getMessage rq)
        { model
        | routeQuery = rq
        , peopleModel = newPeopleModel
        , errorMsg = ""
        {--
            (toString rq)
            ++
            "\nmodel: "
            ++
            (toString model)
            ++
            "\npm: "
            ++
            (toString newPeopleModel)
            ++
            "\n"
            ++
            (toString pCmd)
        --}

        }
  in (newModel, batch [ Cmd.map PeopleMsg pCmd, cmd' ])

