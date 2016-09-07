module Pages.Login.View exposing (..)

import Types.Login exposing (Cred)
import Pages.Login.Model exposing (Model,Msg)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

viewLogin : Cred -> Html Msg
viewLogin cred = 
  div 
    []
    [text "hi there"]

view : Model -> Html Msg
view model =
  div []
    [ h4 [] [ text <| model.error ]
    , h4 [] [text "this is the login view for model"]
    , viewLogin model.cred
    ]

