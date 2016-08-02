module Pages.People.Modals exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import View.Modal exposing (..)
import Pages.People.Model exposing (..)



newContactModal : ModalInfo Model Msg
newContactModal model = 
  { id = "newContactModal"
  , title = "New Contact"
  , titleId = Nothing
  , label = "Create a new contact"
  , body = h4 [] [ text "MODALLBODYAHAHAHA" ]
  , footerButtons = [] --model -> List (Html msg)
  , buttonContent = text "New Contact"
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
  , buttonContent = text "New Visit"
    -- modal -> Html msg
  }
