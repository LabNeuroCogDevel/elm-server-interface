module Pages.People.Modals exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import View.Modal exposing (..)
import View.Bootstrap exposing (..)
import Pages.People.Model exposing (..)

import Form exposing (FieldState)
import Form.Field exposing (Field)

import Form

import Html.App as Html
import Form.Input as Input
import Form.Field as Field

{--
<div class="form-group row">
  <label for="inputPassword3" class="col-sm-2 col-form-label">Password</label>
  <div class="col-sm-10">
    <input type="password" class="form-control" id="inputPassword3" placeholder="Password">
  </div>
</div>
--}

makeFormGroupDiv : String -> String -> String -> String -> (String -> Field) -> FieldState e String -> Html Form.Msg
makeFormGroupDiv fLabel fType fPlaceholder fId wrapper fValue =
  div
    [ class "form-group row"
    ]
    [ label
        [ for fId
        , class "col-sm-2 col-form-label"
        ]
        [ text fLabel
        ]
    , div
        [ class "col-sm-10"
        ]
        [ Input.baseInput
            fType
            wrapper
            fValue
            [ class "form-control"
            , id fId
            , placeholder fPlaceholder
            ]
        ]
    ]
            



newContactModal : ModalInfo Model Msg
newContactModal model = 
  let
    frm = model.contactForm
    cType = Form.getFieldAsString "cType" frm
    contents = Form.getFieldAsString "contents" frm
    notes = Form.getFieldAsString "notes" frm
  in
  { id = "newContactModal"
  , title = "New Contact"
  , titleId = Nothing
  , label = "Create a new contact"
  , body =
      div
        [ classList 
            [ ("container-fluid", True)
            ]
        ]
        [ Html.map NewContactFormMsg <| Html.form []
            [ makeFormGroupDiv "Contact Type" "text" "Contact Type" "contactType" Field.Text cType
            , makeFormGroupDiv "Contents" "text" "Contents" "contents" Field.Text contents
            , makeFormGroupDiv "Notes" "textarea" "Notes" "notes" Field.Text notes
            ]
        ]
  , footerButtons = [] --model -> List (Html msg)
  , buttonContent =
      { attributes    = [ ]
      , buttonContent = [ text "New Contact" ]
      }
    -- modal -> Html msg
  }


newVisitModal : ModalInfo Model Msg
newVisitModal model = 
  { id = "newVisitModal"
  , title = "New Visit"
  , titleId = Nothing
  , label = "Create a new visit"
  , body = h4 [] [ text "MODALLBODYAHAHAHA" ]
  , footerButtons = [] --model -> List (Html msg)
  , buttonContent =
      { attributes =
          [
          ]
      , buttonContent = 
          [ text "New Visit"
          ]
      }
    -- modal -> Html msg
  }
