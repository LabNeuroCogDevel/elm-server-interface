module Pages.People.View exposing (..)

import Types.Person exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Pages.People.Model exposing (..)
import View.Bootstrap exposing (..)
import Nav.Routes exposing (..)
import Nav.RQ exposing (..)
import Nav.Operations exposing (..)
import Pages.People.Search exposing (..)

import Maybe exposing (withDefault)
import Form exposing (Form)
import ElmEscapeHtml exposing (unescape)
import View.Pagination exposing (makePaginator)
import Components.Search.Model exposing (SortStatus (..))

import String
import Maybe
import Form

import List as L
import String as S
import Utils.List as UL

import Html.App as Html
import Html.Attributes as Atts
import Json.Decode as Json
import Components.Contacts.View as ContView
import Components.Search.View as SrchView



view : Model -> Html Msg
view model = vtemp model

vtemp model = 
  let
    rq = model.routeQuery
    pg = model.paging
    page = pg.curPage
    maxPage = pg.totalPages
    firstItem = pg.firstItem
    lastItem = pg.lastItem
    maxItem = pg.numItems
  in
    div
      []
      [ mkNPForm model model.form
      , mkEdForm model model.editForm
      , h1 []
          [ text <|
            "Page " ++ (toString page) ++" of "++(toString maxPage) 
              ++ " showing items " ++ (toString firstItem) ++ "-"
              ++ toString (lastItem) ++ " of " ++ (toString maxItem)
              ++ "." ]
      , h2 [] [ text model.pagingErr ]
      , makePaginator (updateRoute rq (defaultPeople All)) pg
      , SrchView.viewSearch model.searchString SearchStringChanged PeopleSearch model.ordString OrdStringChanged OrdEnter
      , table [ class "table table-striped" ]
          [ thead [ class "thead-inverse no-select" ]
              [ tr []
                  [ th 
                      [ classList 
                          [("table-desc", getSortStatus Id model == Descending)
                          ,("table-asc", getSortStatus Id model == Ascending)
                          ]
                      , onClick <| ChangeSorting Id
                      ]
                      [ text "PID" ]
                  , th
                      [ classList 
                          [("table-desc", getSortStatus Name model == Descending)
                          ,("table-asc", getSortStatus Name model == Ascending)
                          ]
                      , onClick <| ChangeSorting Name
                      ] 
                      [ text "Name" ]
                  , th 
                      [ classList 
                          [("table-desc", getSortStatus DOB model == Descending)
                          ,("table-asc", getSortStatus DOB model == Ascending)
                          ]
                      , onClick <| ChangeSorting DOB
                      ]
                      [ text "Age (DOB)" ]
                  , th
                      [ classList 
                          [("table-desc", getSortStatus Sex model == Descending)
                          ,("table-asc", getSortStatus Sex model == Ascending)
                          ]
                      , onClick <| ChangeSorting Sex
                      ]
                      [ text "Sex" ]
                  , th 
                      [ classList 
                          [("table-desc", getSortStatus Hand model == Descending)
                          ,("table-asc", getSortStatus Hand model == Ascending)
                          ]
                      , onClick <| ChangeSorting Hand
                      ]
                      [ text "Hand" ]
                  , th 
                      [ classList 
                          --[("table-danger", getSortStatus PID model == Descending)
                          --,("table-success", getSortStatus PID model == Ascending)
                          [
                          ]
                      ]
                      [ text "Add Date" ]
                  , th 
                      [ classList 
                          --[("table-danger", getSortStatus PID model == Descending)
                          --,("table-success", getSortStatus PID model == Ascending)
                          [
                          ]
                      ]
                      [ text "Source" ]
                  ]
              ]
          , tbody [] <| L.concat ([newPersonForm model.form] :: List.map (viewEditPerson model) model.people)
          ]

      {--
      , case model.people of 
          [] -> text ""
          xs -> p [ class "alert alert-success" ] [ text (toString xs) ]
      --}
      -- DEBUG
      , div [ class "alert alert-success" ]
          [ ul []
              [ li []
                  [ text <| "id: "++(toString model.id) ]
              , li []
                  [ text <| "editpid: "++(toString model.editpid) ]
              , li []
                  [ text <| "paging: "++(toString model.paging) ]
              , li []
                  [ text <| "routeQuery: "++(toString model.routeQuery) ]
              , li []
                  [ text <| "activepid: "++(toString model.activepid) ]
              , li []
                  [ text <| "contactInfo: "++(toString model.contactInfo) ]
              , li []
                  [ text <| "search: "++(toString <| buildSearch model) ]
              , li []
                  [ text <| "ordString: "++(toString <| buildOrdering model) ]
                  {--
              , li []
                  [ text "editpid: "++(toString model.editpid) ]
              , li []
                  [ text "editpid: "++(toString model.editpid) ]
                  --}
              ]
          ]
      ]


viewEditPerson : Model -> Person -> List (Html Msg)
viewEditPerson m p = 
  let
    default = viewPerson m p 
  in 
    case m.editpid of 
      Nothing ->
        default

      Just id -> if id == p.pid
        then
          [ editPersonForm m.editForm ]
        else
          default


isActive : Model -> Person -> Bool
isActive m p = m.activepid == Just p.pid


viewPerson : Model -> Person -> List (Html Msg)
viewPerson model person = 
  [ tr 
      [ onDoubleClick
          <| NavigateTo
              (Just (defaultPeople (Edit person.pid)))
              Nothing
      , onClick
          <| NavigateTo
              (Just (defaultPeople (View person.pid)))
              Nothing
      , classList
          [("table-success", isActive model person)
          ]
      ] --EditPerson person.pid ]
      [ td []
          [ text <| toString person.pid ]
      , td []
          [ text <| withDefault "N/A" person.fullname ]
          --if person.fullname == "" then "N/A" else person.fullname ]
      , td []
          [ text <| S.concat [ toString <| floor person.curage
                             , " ("
                             , withDefault "N/A" person.dob 
                             , ")"
                             ]
          ]
      , td []
          [ text <| withDefault "N/A" person.sex ]
      , td []
          [ text <| withDefault "N/A" person.hand ]
      , td []
          [ text <| withDefault "N/A" person.adddate ]
      , td []
          [ text <| withDefault "N/A" person.source ]
      ]
  ]
  ++
  ( if isActive model person
    then
      case model.contactInfo of
        Nothing -> 
          []
        Just info ->
          [ tr
              [ style [("background","#a0a0a0")] ]
              [ td
                  [ colspan (4) ]
                  [ ContView.viewCIs info ]
              , td
                  [ colspan (nCols-4) ]
                  [ viewPersonRest person ]
              ]
          ]
    else
      []
  )
  
viewPersonRest : Person -> Html msg
viewPersonRest person = 
  table [ class "table table-striped" ]
    [ thead [ class "thead-inverse" ]
        [ tr []
            [ th [] [ text <| S.concat [ "Summary" ] ]
            , th [] [ text <| S.concat [ "Visit types" ] ]
            , th [] [ text <| S.concat [ "Studies" ] ]
            , th [] [ text <| S.concat [ "Ids" ] ]
            ]
        ]
    , tbody []
        <| L.map (tr [] << L.map (td [] << UL.singleton << text))
        <| UL.transpose ""
            [ [ S.concat [ "Last Visit: ", withDefault "N/A" person.lastvisit ]
              , S.concat [ "Total Visits: ", toString person.numvisits ]
              , S.concat [ "Total Studies: ", toString person.nstudies ]
              , S.concat [ "Max Drop: ", withDefault "N/A" person.maxdrop ]
              , S.concat [ "Total Drops: ", toString person.ndrops ]
              ]
            , person.visittypes
            , person.studies
            , person.ids
            ]
    ]

nCols : Int
nCols = 7


nFields : Int
nFields = 8


newPersonForm : Form CustomError Person -> Html Msg
newPersonForm = formRow 1 "Submit" "Reset" "new-person" 
  <| \msg ->
      case msg of 
        m ->
          FormMsg m


editPersonForm : Form CustomError Person -> Html Msg
editPersonForm = formRow 2 "Save" "Cancel" "edit-person" EditFormMsg


formRow : Int -> String -> String -> String -> (Form.Msg -> Msg) -> Form CustomError Person -> Html Msg
formRow formn submit cancel formid wrap frm = 
  let 
--    pid = Form.getFieldAsString "pid" frm
    name = Form.getFieldAsString "fullname" frm
    dob = Form.getFieldAsString "dob" frm
    sex = Form.getFieldAsString "sex" frm
    hand = Form.getFieldAsString "hand" frm
    adddate = Form.getFieldAsString "adddate" frm
    source = Form.getFieldAsString "source" frm
  in
    tr [ class "form-group" ] <|
      (td [] 
        [ div [ class "" ]
          [ input
              [ type' "submit"
              , attribute "form" formid
              , class "btn btn-primary"
              , value submit
              , tabindex <| formn * nFields + 7
              ]
              [ ]
          , text " "
          , input
              [ type' "reset"
              , attribute "form" formid
              , class "btn btn-danger"
              , value cancel
              , tabindex <| formn * nFields + 8
              ]
              [ ]
          ]
        ])
      ::
      (List.map ((Html.map wrap) << (formTCell formid))
       [ {-- "PID" :- pid
       ,--}
         ("Full Name",  name, formn * nFields + 1, 10, 1)
       , ("Date of Birth", dob, formn * nFields + 2, 10, 1)
       , ("Sex", sex, formn * nFields + 3, 5, 1)
       , ("Hand", hand, formn * nFields + 4, 5, 1)
       , ("Add Date", adddate, formn * nFields + 5, 10, 1)
       , ("Source", source, formn * nFields + 6, 10, 1)
       ])


resetMsg : Msg
resetMsg = FormMsg <| Form.Reset []


cancelMsg : Msg
cancelMsg = NavigateTo (Just (defaultPeople Cancel)) Nothing


newPersonMsg : Model -> Person -> Msg
newPersonMsg model person = SubmitPerson { person | pid = model.id }


savePersonMsg : Model -> Person -> Msg
savePersonMsg model person = SavePerson person


mkNPForm = makeForm "new-person" resetMsg newPersonMsg


mkEdForm = makeForm "edit-person" cancelMsg savePersonMsg


makeForm : String -> Msg -> (Model -> Person -> Msg) -> Model -> Form CustomError Person -> Html Msg
makeForm name cancel success model frm = 
  Html.form
    [ id name
    , on "reset" <| Json.succeed <| cancel
    , onSubmit <| case Form.getOutput frm of 
        Just person ->
          success model person
        
        Nothing ->
          FormMsg Form.Submit
    ]
    []

