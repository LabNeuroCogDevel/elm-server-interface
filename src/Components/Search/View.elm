module Components.Search.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Components.Search.Model exposing (..)
import Utils.Html exposing (..)



viewSearch : String -> (String -> msg) -> msg -> String -> (String -> msg) -> msg -> Html msg
viewSearch txt onin onch ord ordin ordch =
  div [class "search_bar"] 
    [ span [class "tooltip_container"]
      [ input
          [ type' "text"
          , onInput onin
          , onChange (always onch)
          , placeholder "Press Enter to Search"
          , size 40
          , value txt
          ] []
        , span [class "tooltip_text"] [text "e.g: age >=15; age < 30"]
       ]
   , span [class "tooltip_container"]
       [ input
            [ type' "text"
            , onInput ordin
            , onChange (always ordch)
            , placeholder "Order by"
            , size 20
            , value ord
            ] []
       , span [class "tooltip_text"] [text "e.g: age"]
       ]
    
  ]

view : SearchModel k -> Html (SearchMsg k)
view model = viewSearch (searchString model) SearchStringChanged SearchEnter (orderString model) OrdStringChanged OrdEnter

