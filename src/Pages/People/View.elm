module Pages.People.View exposing (..)

import Html                     exposing (..)
import Html.Attributes          exposing (..)
import Html.Events              exposing (..)
import Nav.Operations           exposing (..)
import Nav.RQ                   exposing (..)
import Nav.Routes               exposing (..)
import Pages.People.Model       exposing (..)
import Pages.People.Search      exposing (..)
import Types.Person             exposing (..)
import Types.Person.Hand        exposing (..)
import Types.Person.Sex         exposing (..)
import Utils.Date               exposing (..)
import View.Bootstrap           exposing (..)
import View.TabPane             exposing (..)

import Components.Search.Model  exposing (SortStatus (..))
import ElmEscapeHtml            exposing (unescape)
import Form                     exposing (Form,FieldState)
import Maybe                    exposing (withDefault)
import View.Pagination          exposing (makePaginator)
import Utils                    exposing (with)

import Form
import Maybe
import String

import List                     as       L
import Maybe                    as       M
import String                   as       S
import Utils.List               as       UL

import Components.Contacts.View as       ContView
import Components.Search.Model  as       SearchM
import Components.Search.View   as       Search
import Components.Visits.View   as       VistView
import Form.Input               as       Input
import Html.App                 as       Html
import Html.Attributes          as       Atts
import Json.Decode              as       Json
import Pages.People.Modals      as       Modals
import Types.Person.Hand        as       Hand
import Types.Person.Sex         as       Sex
import View.Modal               as       Modal



view : Model -> Html Msg
view model = vtemp model

vtemp model = 
  let
    rq        = model.routeQuery
    pg        = model.paging
    page      = pg.curPage
    maxPage   = pg.totalPages
    firstItem = pg.firstItem
    lastItem  = pg.lastItem
    maxItem   = pg.numItems
  in
    div
      []
      [ mkNPForm model model.form
      , mkEdForm model model.editForm
      , h2 [] [ text model.pagingErr ]

      -- search bars
      , Html.map SearchMsg <| Search.view model.searchModel

      -- header: search results
      , table [ class "table table-striped tnoborder" ]
          [ thead [ class "thead-inverse no-select" ]
              [ tr [] <|
                  L.map (makeThCell model)
                    [ LunaID
                    , FName
                    , LName
                    , DOB
                    , Sex
                    , Hand
                    , AddDate
                    , Source
                    ]
              ]

          -- MEAT: search results
          , tbody [] <| L.concat ([newPersonForm model.form] :: List.map (viewEditPerson model) model.people)
          ]

      , makePaginator (updateRoute rq (defaultPeople All)) pg

      -- page no. and search counts
      , h3 []
          [ text <|
            -- "Page " ++ (toString page) ++" of "++(toString maxPage) 
              "showing items " ++ (toString firstItem) ++ "-"
              ++ toString (lastItem) ++ "/" ++ (toString maxItem)
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
                  [ text <| "editpid: "++(toString model.editpid) ]
              , li []
                  [ text <| "paging: "++(toString model.paging) ]
              , li []
                  [ text <| "routeQuery: "++(toString model.routeQuery) ]
              , li []
                  [ text <| "activepid: "++(toString model.activepid) ]
              , li []
                  [ text <| "activePerson: "++(toString <| L.head <| L.filter (\person -> Just person.pid == model.activepid) model.people) ]
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

      -- modals -- here but hidden
      , Modal.buildModal Modals.newVisitModal model
      , Modal.buildModal Modals.newContactModal model
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
            [ text <| unescape "&nbsp;&nbsp;"
            ]
        , i 
            [ classList 
                [("fa fa-lg", True)
                ,("fa-sort-desc", getSortStatus key model == Descending)
                ,("fa-sort", getSortStatus key model == Unsorted)
                ,("fa-sort-asc", getSortStatus key model == Ascending)
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
          ,("tselectable", True)
          ]
      ] --EditPerson person.pid ]
      [ td []
          -- [ text <| toString person.pid ]
          [ text <| toString <| withDefault 0 person.lunaid ]
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
      [ tr []
          [ td 
              [ style [("background","#c0c4c0")]
              , colspan nCols
              ]
              [ makeTabDiv
                  ( L.map makeTabInfoTuple
                      [ ( "tabsummary"
                        , "Summary"
                        , \_ -> []
                        , \(model,person) -> 
                            viewPersonRest person
                        )
                      , ( "tabcontacts"
                        , "Contacts"
                        , \_ -> []
                        , \(model,person) ->
                            div
                              []
                              [ withDefault
                                  (div [] [])
                                  (M.map 
                                    (ContView.viewCIs <| \ci ->
                                      [ with Modal.buttonAtts 
                                          ( \_ ->
                                            [ onClick (NewContactFor person ci)
                                            ]
                                          )
                                          Modal.buildModalButton Modals.newContactModal model
                                      ]
                                    )
                                    person.contacts
                                  )
                              ]
                        )
                      , ( "tabvisits"
                        , "Visits"
                        , \_ -> []
                        , \(model,person) ->
                            div
                              []
                              [ Modal.buildModalButton Modals.newVisitModal model
                              , withDefault
                                  (div [] [])
                                  (M.map VistView.viewVisitList person.visits)
                              ]
                        )
                      ]
                  )
                  "tabsummary"
                  (model,person)
              ]
          ]
      ]
    else
      []
  )
  
-- how to view multiple ids: list of divs
viewID : EnrollID -> Html Msg
viewID id =
 div []
   [ span [] [text id.id]
   , span [] [text id.etype]
   , span [] [text (dateToString id.edate)]
   ]
 
viewIDs : List EnrollID -> Html  Msg
viewIDs ids = div [] (L.map viewID ids)

-- stop gap so we can skip worrying about applying above
viewIDstring : List EnrollID -> List String
viewIDstring ids = L.map (\id -> S.concat [id.etype," ", id.id]) ids

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
            , viewIDstring person.ids
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
        [ div [ class "nobr" ]
          [ button
              [ type' "submit"
              , attribute "form" formid
              , class "btn btn-primary"
              --, value "" --submit
              , tabindex <| formn * nFields + 7
              ]
              [ i [ class "fa fa-plus" ] []
              ]
          , span []
              [ text <| unescape "&nbsp;&nbsp;"--&nbsp;"
              ]
          , button
              [ type' "reset"
              , attribute "form" formid
              , class "btn btn-danger"
              --, value cancel
              , tabindex <| formn * nFields + 8
              ]
              [ i [ class "fa fa-times" ] []
              ]
          ]
        ])
      ::
      ( List.map ((Html.map wrap) << (formTCell formid))
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
        ]
      )


resetMsg : Msg
resetMsg = FormMsg <| Form.Reset []


cancelMsg : Msg
cancelMsg = NavigateTo (Just (defaultPeople Cancel)) Nothing


newPersonMsg : Model -> Person -> Msg
newPersonMsg model person = SubmitPerson person


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
