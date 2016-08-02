module Pages.People.Update exposing (..)

import Pages.People.Model exposing (..)
import Types.Either exposing (..)
import Nav.Operations exposing (..)
import Nav.Routes exposing (..)

import Form exposing (Form)
import Nav.RQ exposing (RQ, getQueryParam)--, getQueryRQ)
import Pages.People.HttpCmds exposing (updateNavFromModel)
import Pages.People.Search exposing (peopleKeyInfo,PeopleKey (..))

import List 
import String
import Result

import Nav.Routes as R
import List as L
import Dict as D
import Maybe as M
import Utils.Maybe as UM
import Utils.Navigation as UNav
import Pages.People.Model as P

import Pages.People.HttpCmds as HttpCmds
import Types.Person as Person
import Pages.People.HttpCmds as PC
import Utils as Utils
import Utils.Http.Handlers as Crud
--import Components.Search.Model exposing (..)
import Components.Search.Model as Search
import Components.Search.Update as SearchU
import Components.Contacts.HttpCmds as ContHttp
import Components.Visits.HttpCmds as VistHttp

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
        fVal = M.map (\val -> (FName, Search.ILike val)) fValM
        lVal = M.map (\val -> (LName, Search.ILike val)) lValM
        newModel =
          { model
          | form = newForm
          , searchModel = 
              Search.addSearches
                [ ("fname", fVal)
                , ("lname", lVal)
                ]
                model.searchModel
          , activepid = Nothing
          }
      in
        ( newModel
        , HttpCmds.runSearch (buildSearch newModel) (buildOrder newModel)
        )

    SearchMsg msg ->
      let
        sModel = SearchU.update model.searchModel msg
        newModel =
          { model 
          | searchModel = sModel
          , activepid = if Search.isSubmitMsg msg then Nothing else model.activepid
          }
      in
        ( newModel
        , if Search.isSubmitMsg msg
          then
            Cmd.batch
              [ HttpCmds.runSearch (buildSearch newModel) (buildOrder newModel)
              , HttpCmds.updateNavFromModel newModel
              ]
          else
            Cmd.none--HttpCmds.updateNavFromModel newModel
        )


    EditFormMsg formMsg ->
      ({ model | editForm = Form.update formMsg model.editForm}, Cmd.none)


    NewContactFormMsg msg ->
      ( { model
        | contactForm = Form.update msg model.contactForm
        }
      , Cmd.none
      )


{--
    NewVisitFromMsg msg ->
      ( { model
        | visitForm = Form.update msg model.visitForm
        }
      , Cmd.none
      )
--}

    
    SubmitPerson person ->
      ({ model 
       | form = Form.update (Form.Reset <| personFields Person.new) model.form
       , people = model.people
       , searchModel = Search.clearAdditionalSearches model.searchModel
       }
      , Crud.create PC.person person
      )


    SubmittedPerson person ->
      ( model
      , UNav.navigateTo
          model.routeQuery
          (Just (defaultPeople All))
          Nothing
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
              , UNav.navigateTo
                  model.routeQuery
                  (Just (defaultPeople All))
                  Nothing
              )
            else
              ( { model | activepid = Just pid
                        , editpid = Nothing
                        }
              , if p.contacts == Nothing
                then
                  Cmd.batch
                    [ ContHttp.getCICmd (always NoOp) (ContactInfo pid) pid
                    , VistHttp.getVisitsCmd (always NoOp) (ReceiveVisits pid) pid
                    ]
                else
                  Cmd.none
              )
          Nothing ->
            ( model
            , UNav.navigateTo
                model.routeQuery
                (Just (defaultPeople All))
                Nothing
                --(Just <| getQueryRQ model.routeQuery)
            )
    
    ReceiveVisits id visits ->
          ( updatePerson id (Person.addVisits visits) model
          , Cmd.none
          )

    ContactInfo id info ->
          ( updatePerson id (Person.addContacts info) model
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
            , UNav.navigateTo
                model.routeQuery
                (Just (defaultPeople All))
                Nothing --(Just <| getQueryRQ model.routeQuery)
            )

    CancelEdit ->
      ( {model | editpid = Nothing
               , activepid = Nothing 
               }
      , UNav.navigateTo
          model.routeQuery
          (Just (defaultPeople All))
          Nothing --(Just <| getQueryRQ model.routeQuery)
      )

    NavigateTo route query ->
      ( model
      , UNav.navigateTo
          model.routeQuery
          route
          query
      )

    SavePerson person ->
      ({model | people = List.map 
                           (\p -> if p.pid == person.pid then person else p)
                           model.people
              , editpid = Nothing}
      , Crud.update PC.person person
      )

    SavedPerson person ->
      ( model
      , UNav.navigateTo
          model.routeQuery
          (Just (defaultPeople All))
          Nothing
      )

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
    nM = { model | routeQuery = rq }
    maybePageNum = (getQueryParam "page" rq)
                   `M.andThen`
                   (Result.toMaybe << String.toInt)
    maybeSearchStr = getQueryParam "search" rq
    searchStr = M.withDefault (searchString model) maybeSearchStr

    maybeOrdStr = getQueryParam "order" rq
    ordStr = M.withDefault (ordString model) maybeOrdStr

    newModel =
      updateSearchString searchStr
        <| updateOrdString ordStr
        <| nM

    cmd = 
      case maybePageNum of
        Just n ->
          if n /= model.paging.curPage || searchStr /= (searchString model) || ordStr /= (ordString model)
          then
            HttpCmds.getPeople (buildSearch newModel) (buildOrder newModel) 25 n
          else
            Cmd.none

        Nothing ->
          Cmd.none
  in
    (newModel,cmd)



