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
          }
      in
        ( newModel
        , HttpCmds.runSearch (buildSearch newModel) (buildOrder newModel)
        )
      {--
        if newfVal == Nothing && newlVal == Nothing
        then 
          ( newModel, Cmd.none )
        else
          let
            fVal = M.withDefault newModel.fnameFilter newfVal 
            lVal = M.withDefault newModel.lnameFilter newlVal

            
            m = { newModel | fnameFilter = fVal, lnameFilter = lVal }
          in
      --}

    -- TODO doesn't actually run a search ... kinda does... oy it's a little weird
    SearchMsg msg ->
      let
        sModel = SearchU.update model.searchModel msg
        newModel =
          { model 
          | searchModel = sModel
          }
      in
        ( newModel
        , if Search.isSubmitMsg msg
          then
            HttpCmds.updateNavFromModel newModel
            --HttpCmds.runSearch (buildSearch newModel) (buildOrder newModel)
          else
            Cmd.none--HttpCmds.updateNavFromModel newModel
        )


    EditFormMsg formMsg ->
      ({ model | editForm = Form.update formMsg model.editForm}, Cmd.none)
    
    SubmitPerson person ->
      ({ model 
       | form = Form.update (Form.Reset <| personFields Person.new) model.form
       , people = model.people
       , id = model.id + 1
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

{--
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

--}
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
                        , contactInfo = Nothing
                        }
              , ContHttp.getCICmd (always NoOp) ContactInfo pid
              )
          Nothing ->
            ( model
            , UNav.navigateTo
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

{--
    ChangeSorting key ->
      update 
        model
        (SearchMsg
          <| Search.ChangeSorting key)

        --( nm
        --, updateNavFromModel nm
        --) 
--}
          

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
          --if n /= model.paging.curPage
          --then
          HttpCmds.getPeople (buildSearch newModel) (buildOrder newModel) 25 n
          --else
          --  Cmd.none

        Nothing ->
          Cmd.none
  in
    (newModel,cmd)



