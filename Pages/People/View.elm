module Pages.People.View exposing (..)

import Person exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Pages.People.Model exposing (..)
import View.Bootstrap exposing (..)
import Nav.Routes exposing (..)
import Nav.RQ exposing (..)

import Maybe exposing (withDefault)
import Form exposing (Form)
import ElmEscapeHtml exposing (unescape)
import View.Pagination exposing (makePaginator)

import String
import Maybe

import Html.App as Html
import Html.Attributes as Atts
import Json.Decode as Json



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
      , makePaginator (updateRoute rq (People All)) pg
      , table [ class "table table-striped" ]
          [ thead []
              [ tr []
                  [ th [] [ text "PID" ]
                  , th [] [ text "Name" ]
                  , th [] [ text "DOB" ]
                  , th [] [ text "Sex" ]
                  , th [] [ text "Hand" ]
                  , th [] [ text "IDs" ]
                  ]
              ]
          , tbody [] (newPersonForm model.form :: List.map (viewEditPerson model) model.people)
          ]

      {--
      , case model.people of 
          [] -> text ""
          xs -> p [ class "alert alert-success" ] [ text (toString xs) ]
      --}
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
                  {--
              , li []
                  [ text "editpid: "++(toString model.editpid) ]
              , li []
                  [ text "editpid: "++(toString model.editpid) ]
                  --}
              ]
          ]
      ]

viewEditPerson : Model -> Person -> Html Msg
viewEditPerson m p = case m.editpid of 
  Nothing ->
    viewPerson p

  Just id -> if id == p.pid
    then
      editPersonForm m.editForm
    else
      viewPerson p

viewPerson : Person -> Html Msg
viewPerson person = 
  tr 
    [ onDoubleClick
        <| NavigateTo
             (Just (People (Edit person.pid)))
             Nothing
    ] --EditPerson person.pid ]
    [ td []
        [ text <| toString person.pid ]
    , td []
        [ text <| withDefault "N/A" person.fullname ]
        --if person.fullname == "" then "N/A" else person.fullname ]

    , td []
        [ text <| withDefault "N/A" person.dob ]
    , td []
        [ text <| withDefault "N/A" person.sex ]
    , td []
        [ text <| withDefault "N/A" person.hand ]
    , td []
        [ text <| if person.ids == [] then "N/A" else String.join " " person.ids ]
    ]

nFields : Int
nFields = 7

newPersonForm : Form CustomError Person -> Html Msg
newPersonForm = formRow 1 "Submit" "Reset" "new-person" FormMsg

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
    ids = Form.getFieldAsString "ids" frm
  in
    tr [ class "form-group" ] <|
      (td [] 
        [ div [ class "" ]
          [ input
              [ type' "submit"
              , attribute "form" formid
              , class "btn btn-primary"
              , value submit
              , tabindex <| formn * nFields + 6
              ]
              [ ]
          , text " "
          , input
              [ type' "reset"
              , attribute "form" formid
              , class "btn btn-danger"
              , value cancel
              , tabindex <| formn * nFields + 7
              ]
              [ ]
          ]
        ])
      ::
      (List.map ((Html.map wrap) << (formTCell formid))
       [ {-- "PID" :- pid
       ,--}
         ("Full Name",  name, formn * nFields + 1)
       , ("Date of Birth", dob, formn * nFields + 2)
       , ("Sex", sex, formn * nFields + 3)
       , ("Hand", hand, formn * nFields + 4)
       , ("IDs", ids, formn * nFields + 5)
       ])

resetMsg : Msg
resetMsg = FormMsg <| Form.Reset []

cancelMsg : Msg
cancelMsg = NavigateTo (Just (People Cancel)) Nothing

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

{--
formView : Form CustomError Person -> Html Msg
formView frm = 
  let 
    --pid = Form.getFieldAsString "pid" frm
    name = Form.getFieldAsString "fullname" frm
    dob = Form.getFieldAsString "dob" frm
    sex = Form.getFieldAsString "sex" frm
    hand = Form.getFieldAsString "hand" frm
    ids = Form.getFieldAsString "ids" frm
  in
    Html.form
      [ id "new-person"
      , on "reset" <| Json.succeed <| FormMsg <| Form.Reset []
      , onSubmit <| case Form.getOutput frm of 
          Just person ->
            SubmitPerson person
          
          Nothing ->
            FormMsg Form.Submit
      ]
      [ div [ style [("width","600px")
                    ,("margin", "50px auto")]
            , class "form-horizontal" ]
        <|
        {--
        [ legend [] [ text "Person Form" ] ]
        ++
        (List.map (Html.map FormMsg)
         [ textGroup "PID" pid
         , textGroup "Full Name" name
         , textGroup "Date of Birth" dob
         , textGroup "Sex" sex
         , textGroup "Hand" hand
         , textGroup "IDs" ids
         ]) 
        ++
        --}
            [ formActions
                [ input
                    [ type' "submit"
                    --, attribute "form" "new-person"
                    , class "btn btn-primary"
                    ]
                    [ text "Submit" ]
                , text " "
                , input
                    [ type' "reset"
                    --, attribute "form" "new-person"
                    , class "btn btn-danger"
                    ]
                    [ text "Reset" ]
                ]
            ]
      ]
      --} --}
