module ModalTest exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import View.Modal exposing (..)

import Html.App as App

type alias Model = ()

type Msg = NoOp

info : ModalInfo Model Msg
info _ = 
  { id = "modalie"
  , title = "Wheeeee Modallll"
  , titleId = "modalTitle"
  , label = "This is a modal"
  , body = h4 [] [ text "MODALLBODYAHAHAHA" ]
  , footerButtons = [] --model -> List (Html msg)
  , buttonContent = text "OPEN MY MODALL!!!!!111!!!1!"
    -- modal -> Html msg
  }

view : Model -> Html Msg
view model =
  div []
    [ buildModalButton info model
    , buildModal info model
    ]

update : Msg -> Model -> Model --, Cmd Msg)
update msg model = case msg of
  _ -> (model)

main = App.beginnerProgram
  { model = ()
  , view = view
  , update = update
  }

