module Components.Search.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Components.Search.Model exposing (..)
import Utils.Html exposing (..)


viewSearch : String -> (String -> msg) -> msg -> Html msg
viewSearch txt onin onch =
  input
    [ type' "text"
    , onInput onin
    , onChange (always onch)
    , placeholder "Press enter to Search"
    , size 40
    , value txt
    ] []

