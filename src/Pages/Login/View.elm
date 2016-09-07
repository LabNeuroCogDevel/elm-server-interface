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
    [input [ onInput Pages.Login.Model.IdUp, placeholder "UPMC id" ] []
    ,input [ type' "password", onInput Pages.Login.Model.PassUp] []
    ,button [onClick Pages.Login.Model.SetAuth] [text "authenticate"] 
    ]
    -- ,text (toString cred.isvalid)
    -- ,text cred.authtoken ]

view : Model -> Html Msg
view model =
  div []
    [ h4 [] [ text <| model.error ]
    , h4 [] [text "Login"]
    , viewLogin model.cred
    ]

