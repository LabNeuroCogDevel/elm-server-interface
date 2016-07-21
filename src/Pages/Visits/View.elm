module Pages.Visits.View exposing (..)

import Pages.Visits.Model exposing (..)
import Types.Visit exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import ISO8601 as T
import String as S
import List as L
import Maybe as M

viewVisit : Visit -> Html Msg
viewVisit visit = 
  div 
    []
    <|
    [ h6 []
        [ strong [] [ text visit.vtype ]
        , text <| (\x -> S.concat [ " --- ", x ]) <| visit.status
        , text <| M.withDefault "" <| M.map (\x -> S.concat [ " -- ", toString x ]) visit.score
        ]
    , strong [] [ text "Duration " ]
    , text <| M.withDefault "n/a" <| M.map toString visit.durHr
    , br [] []
    , strong [] [ text "Timestamp " ]
    , text <| M.withDefault "n/a" <| M.map T.toString visit.timestamp
    , br [] []
    , strong [] [ text "Age " ]
    , text <| toString visit.age
    , br [] []
    , a
        [ href <| M.withDefault "#/visits/all" visit.googleuri
        ]
        [ text "Google Calendar"
        ]
    ]
    {--
    ++
    [ text <| S.concat [ "Visit Types: ", S.join ", " visit.visitTypes ]
    ]
    ++
    [ br [] []
    ]
    ++
    [ text <| S.concat [ "Cohorts: ", S.join ", " visit.cohorts ]
    ]
    --}

view : Model -> Html Msg
view model =
  div []
    [ --text <| S.concat ["Visits: ", toString model.visits]
    -- ,
    h4 []
        [ text <| model.error
        ]
    , div []
        <| L.intersperse (hr [] [])
        <| L.map viewVisit model.visits
    ]

