module View.TabPane exposing (TabInfo, makeTabInfo, makeTabInfoTuple, makeTabDiv)


import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

import List as L


role : String -> Attribute msg
role = attribute "role" 

dataToggle : String -> Attribute msg
dataToggle = attribute "data-toggle" 

type alias TabInfo model msg = 
  { hrefId : String
  , title : String
  , linkAtts : model -> List (Attribute msg)
  , tabView : model -> Html msg
  }

makeTabInfo : String -> String -> (model -> List (Attribute msg)) -> (model -> Html msg) -> TabInfo model msg
makeTabInfo hrefId title linkAtts tabView =
  { hrefId = hrefId
  , title = title
  , linkAtts = linkAtts
  , tabView = tabView
  }


makeTabInfoTuple : (String, String, model -> List (Attribute msg), model -> Html msg) -> TabInfo model msg
makeTabInfoTuple (hrefId, title, linkAtts, tabView) = 
  makeTabInfo hrefId title linkAtts tabView



makeNavItem : String -> TabInfo model msg -> model -> Html msg
makeNavItem defaultTab ti mod =
  li [ class "nav-item" ]
    [ a
        ( [ classList 
              [ ("nav-link",True)
              , ("active", ti.hrefId == defaultTab)
              ]
          , href <| "#"++ti.hrefId
          , role "tab"
          , dataToggle "tab"
          ]
          ++
          (ti.linkAtts mod)
        )
        [ text ti.title
        ]
    ]

makeTabPane : String -> TabInfo model msg -> model -> Html msg
makeTabPane defaultTab ti mod =
  div
    [ classList
        [ ("tab-pane",True)
        , ("fade", True)
        , ("in", defaultTab == ti.hrefId)
        , ("active", defaultTab == ti.hrefId)
        ]
    , role "tabpanel"
    , id ti.hrefId
    ]
    [ ti.tabView mod
    ]

makeTabDiv : List (TabInfo model msg) -> String -> model -> Html msg
makeTabDiv list defaultTab mod =
  div
    [ class "container" ]
    [ ul
        [ class "nav nav-tabs"
        , attribute "role" "tablist"
        ]
        <| L.map (((|>) mod) << makeNavItem defaultTab) list
    , div
        [ class "tab-content clearfix"
        ]
        <| L.map (((|>) mod) << makeTabPane defaultTab) list
    ]
  








