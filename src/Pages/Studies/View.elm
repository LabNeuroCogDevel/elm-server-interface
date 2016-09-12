module Pages.Studies.View exposing (..)

import Pages.Studies.Model exposing (..)
import Types.Study exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import String as S
import List as L

viewStudy : Study -> Html Msg
viewStudy study = 
  div 
    []
    <|
    [ h6 []
        [ strong [] [ text study.name ]
        , text <| S.concat [ " -- ", study.grantName ]
        ]
    ]
    ++
    [ text <| S.concat [ "Visit Types: ", S.join ", " study.visitTypes ]
    ]
    ++
    [ br [] []
    ]
    ++
    [ text <| S.concat [ "Cohorts: ", S.join ", " study.cohorts ]
    ]

view : Model -> Html Msg
view model =
  div []
    <| L.intersperse (hr [] [])
    <| L.map viewStudy model.studies
