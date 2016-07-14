module Pages.People.Update exposing (..)

import Pages.People.Model exposing (..)
import Types.Either exposing (..)
import Nav.Operations exposing (..)
import Nav.Routes exposing (..)

import Form exposing (Form)
import Nav.RQ exposing (RQ, getQueryParam)--, getQueryRQ)
import Pages.People.HttpCmds exposing (updateNavFromModel)
import Pages.People.Search exposing (peopleKeyInfo)

import List 
import String
import Result

import Nav.Routes as R
import List as L
import Dict as D
import Maybe as M
import Utils.Maybe as UM
import Pages.People.Model as P

import Pages.People.HttpCmds as HttpCmds
import Types.Person as Person
import Utils as Utils
import Components.Search.Model exposing (..)
import Components.Contacts.HttpCmds as ContHttp

init : RQ -> (Model, Cmd Msg)
init rq = 
  (initModel rq, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of 

    NoOp ->
      (model, Cmd.none)

    FormMsg formMsg ->
      let 
        newForm = Form.update formMsg model.form
        fnameField = Form.getFieldAsString "fname" newForm
        lnameField = Form.getFieldAsString "lname" newForm
        fValM = fnameField.value
        lValM = lnameField.value
        newfVal =
          UM.test ((/=) model.fnameFilter)
            <| M.withDefault "" fValM
        newlVal =
          UM.test ((/=) model.lnameFilter)
            <| M.withDefault "" lValM
        newModel = { model | form = newForm }
      in
        if newfVal == Nothing && newlVal == Nothing
        then 
          ( newModel, Cmd.none )
        else
          let
            fVal = M.withDefault newModel.fnameFilter newfVal 
            lVal = M.withDefault newModel.lnameFilter newlVal
            m = { newModel | fnameFilter = fVal, lnameFilter = lVal }
          in
            ( m
            , HttpCmds.runSearch (buildSearch m) (buildOrdering m)
            )


    EditFormMsg formMsg ->
      ({ model | editForm = Form.update formMsg model.editForm}, Cmd.none)
    
    SubmitPerson person ->
      ({ model 
       | form = Form.update (Form.Reset <| personFields Person.new) model.form
       , people = model.people
       , id = model.id + 1
       , fnameFilter = ""
       , lnameFilter = ""
       }
      , HttpCmds.insertPerson person
      )

    SubmittedPerson person ->
      ( model
      , Utils.navigateTo
          model.routeQuery
          (Just (defaultPeople All))
          Nothing
      )

    SearchStringChanged str ->
      ( { model
        | searchString = str
        }
      , Cmd.none
      )

    PeopleSearch ->
      ( model
      , updateNavFromModel model
      )

    OrdStringChanged ordstr ->
      ( { model
        | ordString = ordstr
        }
      , Cmd.none
      )

    OrdEnter ->
      ( model
      , updateNavFromModel model
      )

    ViewPerson pid ->
      let 
        person = List.head <| List.filter (((==) pid) << .pid) model.people
      in 
        case person of
          Just p -> 
            if model.activepid == Just pid
            then
              ( { model | activepid = Nothing }
              , Utils.navigateTo
                  model.routeQuery
                  (Just (defaultPeople All))
                  Nothing
              )
            else
              ( { model | activepid = Just pid
                        , editpid = Nothing
                        , contactInfo = Nothing
                        }
              , ContHttp.getCICmd (always NoOp) ContactInfo pid
              )
          Nothing ->
            ( model
            , Utils.navigateTo
                model.routeQuery
                (Just (defaultPeople All))
                Nothing
                --(Just <| getQueryRQ model.routeQuery)
            )
    
    ContactInfo info ->
      ( { model | contactInfo = Just info }
      , Cmd.none
      )

    -- TODO implement crud ops
    CrudOp op ->
      (model,Cmd.none)

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
                (Just (defaultPeople All))
                Nothing --(Just <| getQueryRQ model.routeQuery)
            )

    CancelEdit ->
      ( {model | editpid = Nothing
               , activepid = Nothing 
               }
      , Utils.navigateTo
          model.routeQuery
          (Just (defaultPeople All))
          Nothing --(Just <| getQueryRQ model.routeQuery)
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
          (Just (defaultPeople All))
          Nothing --(Just <| getQueryRQ model.routeQuery)
      )

      --(View person.pid)))

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

    ChangeSorting key ->
      let
        ss = Pages.People.Model.getSortStatus key model
        ord = parseOrder peopleKeyInfo model.ordString
        nm =
          { model 
          | ordString = case ss of
              Ascending ->
                orderingToString peopleKeyInfo <| (Desc key) :: ord
              Descending ->
                orderingToString peopleKeyInfo <| L.filter (((/=) key) << getKey) ord
              Unsorted ->
                orderingToString peopleKeyInfo <| (Asc key) :: ord
          }

      in
        ( nm
        , updateNavFromModel nm
        ) 

          

    RQChanged rq -> 
      ({ model | routeQuery = rq }, Cmd.none)


urlUpdate : RQ -> Model -> (Model, Cmd Msg)
urlUpdate rq model = 
  let
    nM = { model | routeQuery = rq }
    maybePageNum = (getQueryParam "page" rq)
                   `M.andThen`
                   (Result.toMaybe << String.toInt)
    maybeSearchStr = getQueryParam "search" rq
    searchStr = M.withDefault model.searchString maybeSearchStr

    maybeOrdStr = getQueryParam "order" rq
    ordStr = M.withDefault model.ordString maybeOrdStr

    newModel = { nM | searchString = searchStr, ordString = ordStr }

    cmd = 
      case maybePageNum of
        Just n ->
          --if n /= model.paging.curPage
          --then
          HttpCmds.getPeople (buildSearch newModel) (buildOrdering newModel) 25 n
          --else
          --  Cmd.none

        Nothing ->
          Cmd.none
  in
    (newModel,cmd)



