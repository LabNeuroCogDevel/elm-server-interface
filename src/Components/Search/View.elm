module Components.Search.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Components.Search.Model exposing (..)
import Utils.Html exposing (..)


viewSearch : String -> (String -> msg) -> msg -> String -> (String -> msg) -> msg -> Html msg
viewSearch txt onin onch ord ordin ordch =
  span []
    [ input
        [ type' "text"
        , onInput onin
        , onChange (always onch)
        , placeholder "Press Enter to Search"
        , size 40
        , value txt
        ] []
    , input
        [ type' "text"
        , onInput ordin
        , onChange (always ordch)
        , placeholder "Order by"
        , size 20
        , value ord
        ] []
    ]

