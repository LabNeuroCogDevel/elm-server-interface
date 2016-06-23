module Pages.People.Update exposing (..)

import Pages.People.Model exposing (..)
import Types.Either exposing (..)

import Form exposing (Form)
import Nav.RQ exposing (RQ, getQueryRQ)

import List 
import String
import Result

import Nav.Routes as R
import List as L
import Dict as D
import Maybe as M
import Pages.People.Model as P

import Pages.People.HttpCmds as HttpCmds
import Types.Person as Person
import Utils as Utils

init : RQ -> (Model, Cmd Msg)
init rq = 
  (initModel rq, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of 

    NoOp ->
      (model, Cmd.none)

    FormMsg formMsg ->
      ({ model | form = Form.update formMsg model.form}, Cmd.none)

    EditFormMsg formMsg ->
      ({ model | editForm = Form.update formMsg model.editForm}, Cmd.none)
    
    SubmitPerson person ->
      ({ model 
       | form = Form.update (Form.Reset <| personFields Person.new) model.form
       , people = person :: model.people
       , id = model.id + 1
       }
      , Cmd.none)

    EditPerson pid ->
      let 
        person = List.head <| List.filter (((==) pid) << .pid) model.people
      in 
        case person of
          Just p -> 
            ({model | editpid = Just pid
                    , editForm = buildEditForm p
                    }
            , Cmd.none)
          Nothing ->
            ( model
            , Utils.navigateTo
                model.routeQuery
                (Just (R.People R.All))
                (Just <| getQueryRQ model.routeQuery)
            )

    CancelEdit ->
      ( {model | editpid = Nothing}
      , Utils.navigateTo
          model.routeQuery
          (Just (R.People R.All))
          (Just <| getQueryRQ model.routeQuery)
      )

    NavigateTo route query ->
      ( model
      , Utils.navigateTo
          model.routeQuery
          route
          query
      )

    SavePerson person ->
      ({model | people = List.map 
                           (\p -> if p.pid == person.pid then person else p)
                           model.people
              , editpid = Nothing}
      , Utils.navigateTo
          model.routeQuery
          (Just (R.People R.All))
          (Just <| getQueryRQ model.routeQuery)
      )
      --(R.View person.pid)))

    ChangePeopleList pList pging -> 
      let
        nm = {model | people = pList, paging = pging}
      in 
        case model.editpid of
          Nothing ->
            ( nm
            , Cmd.none
            )
          Just p ->
            ( { nm | editpid = M.map (.pid)
                       <| L.head
                       <| L.filter (((==) p) << (.pid))
                       <| pList }
            , Cmd.none
            )

    RQChanged rq -> 
      ({ model | routeQuery = rq }, Cmd.none)


urlUpdate : RQ -> Model -> (Model, Cmd Msg)
urlUpdate rq model = 
  let
    newModel = { model | routeQuery = rq }
    maybePageNum = (D.get "page" <| getQueryRQ rq)
                   `M.andThen`
                   (Result.toMaybe << String.toInt)
  in
    case maybePageNum of
      Just n ->
        ( newModel
        , if n /= model.paging.curPage
          then
            HttpCmds.getPeople 25 n
          else
            Cmd.none
        )

      Nothing ->
        (newModel, Cmd.none)



