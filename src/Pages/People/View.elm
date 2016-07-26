module Pages.People.View exposing (..)

import Types.Person exposing (..)
import Types.Person.Sex exposing (..)
import Types.Person.Hand exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import View.Bootstrap exposing (..)
import Pages.People.Model exposing (..)
import Nav.Routes exposing (..)
import Nav.RQ exposing (..)
import Nav.Operations exposing (..)
import Pages.People.Search exposing (..)
import Utils.Date exposing (..)

import Maybe exposing (withDefault)
import Form exposing (Form,FieldState)
import ElmEscapeHtml exposing (unescape)
import View.Pagination exposing (makePaginator)
import Components.Search.Model exposing (SortStatus (..))

import String
import Maybe
import Form

import Maybe as M
import List as L
import String as S
import Utils.List as UL

import Html.App as Html
import Html.Attributes as Atts
import Json.Decode as Json
import Form.Input as Input
import Components.Contacts.View as ContView
import Components.Search.View as Search
import Components.Search.Model as SearchM
import Types.Person.Sex as Sex
import Types.Person.Hand as Hand



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
      , Html.map SearchMsg <| Search.view model.searchModel
      , table [ class "table table-striped" ]
          [ thead [ class "thead-inverse no-select" ]
              [ tr [] <|
                  L.map (makeThCell model)
                    [ Id
                    , FName
                    , LName
                    , DOB
                    , Sex
                    , Hand
                    , AddDate
                    , Source
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
                  [ text <| "ordString: "++(toString <| buildOrder model) ]
              , li []
                  [ text <| "searchModel: "++(toString <| model.searchModel) ]
                  {--
              , li []
                  [ text "editpid: "++(toString model.editpid) ]
              , li []
                  [ text "editpid: "++(toString model.editpid) ]
                  --}
              ]
          ]
      ]

makeThCell : Model -> PeopleKey -> Html Msg
makeThCell model key = 
  th []
    [ div
        [ classList 
            [ ("table-desc", getSortStatus key model == Descending)
            , ("table-asc", getSortStatus key model == Ascending)
            , ("btn", True)
            --, ("h5", True)
            ]
        , onClick <| SearchMsg <| SearchM.ChangeSorting key
        ]
        [ strong []
            [ text (fst <| peopleKeyInfo.prettyKeyNames key)
            ]
        , span []
            [ text <| unescape "&nbsp;"
            ]
        , i 
            [ classList 
                [("fa", True)
                ,("fa-chevron-down", getSortStatus key model == Descending)
                ,("fa-chevron-up", getSortStatus key model == Ascending)
                ]
            ] []
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
          [ text <| person.fname ]
      , td []
          [ text <| person.lname ]
      , td []
          [ text <| S.concat [ toString <| floor person.curage
                             , " ("
                             , dateToString person.dob 
                             , ")"
                             ]
          ]
      , td []
          [ text <| prettySexToString person.sex ]
      , td []
          [ text <| prettyHandToString person.hand ]
      , td []
          [ text <| withDefault "N/A" <| M.map dateToString person.adddate ]
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
            [ [ S.concat [ "Last Visit: ", withDefault "N/A" <| M.map dateToString person.lastvisit ]
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
nCols = 8


nFields : Int
nFields = 9


newPersonForm : Form CustomError Person -> Html Msg
newPersonForm = formRow 1 "Submit" "Reset" "new-person" 
  <| \msg ->
      case msg of 
        m ->
          FormMsg m


editPersonForm : Form CustomError Person -> Html Msg
editPersonForm = formRow 2 "Save" "Cancel" "edit-person" EditFormMsg

formTCell : String -> (String, FieldState CustomError String, Int, Int, Int,Maybe (Input.Input CustomError String)) -> Html Form.Msg
formTCell formid (label', state, i, l, nc,inp) =
  td
    [ class <| "form-group " ++ (errorClass state.liveError)
    , colspan nc
    ]
    [ withDefault Input.textInput inp state
                            [ class <| "form-control "++(errorClass state.liveError)
                            , placeholder label'
                            , attribute "form" formid
                            , tabindex i
                            , style
                                [("min-width",(toString l)++"em")]
                            ]
    ]

formRow : Int -> String -> String -> String -> (Form.Msg -> Msg) -> Form CustomError Person -> Html Msg
formRow formn submit cancel formid wrap frm = 
  let 
--    pid = Form.getFieldAsString "pid" frm
    fname = Form.getFieldAsString "fname" frm
    lname = Form.getFieldAsString "lname" frm
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
       [ ("First Name", fname, formn * nFields + 1, 10, 1,Nothing)
       , ("Last Name", lname, formn * nFields + 1, 10, 1,Nothing)
       , ("Date of Birth (yyyy-mm-dd)", dob, formn * nFields + 2, 10, 1,Nothing)
       , ("Sex", sex, formn * nFields + 3, 8, 1
         , Just 
           <| Input.selectInput
           <| List.map
               (\x -> (sexToString x, prettySexToString x))
               [ Male, Female, Sex.Unknown, Other ]
         )
       , ("Hand", hand, formn * nFields + 4, 8, 1
         , Just 
           <| Input.selectInput
           <| List.map
               (\x -> (handToString x, prettyHandToString x))
               [ Left, Right, Hand.Unknown, Ambi ]
         )
       , ("Add Date (yyyy-mm-dd)", adddate, formn * nFields + 5, 10, 1,Nothing)
       , ("Source", source, formn * nFields + 6, 10, 1,Nothing)
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

