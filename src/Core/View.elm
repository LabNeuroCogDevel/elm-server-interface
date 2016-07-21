module Core.View exposing (..)

import Nav.RQ exposing (..)
import Nav.Routes exposing (..)
import Core.Model exposing (..)
import Types.Either exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Platform.Cmd exposing (Cmd)

import Pages.People as People
import Pages.Studies as Studies
import Pages.Visits as Visits
import Pages.NotFound as NotFound
import Html.App as Html
import Html.Attributes as Atts


handlePeopleMessage : People.Msg -> Msg
handlePeopleMessage m = PeopleMsg m

handleStudiesMessage : Studies.Msg -> Msg
handleStudiesMessage m = StudiesMsg m

handleVisitsMessage : Visits.Msg -> Msg
handleVisitsMessage m = VisitsMsg m

view : Model -> Html Msg
view model =
  div []
    [ text <| toString model.routeQuery
    {--
    , div [ class "alert alert-danger" ]
        [ text model.errorMsg ]
    --}
    , case getRouteRQ <| model.routeQuery of
        Root ->
          Html.map handlePeopleMessage <|
            People.view model.peopleModel
        
        People _ _ ->
          Html.map handlePeopleMessage <|
            People.view model.peopleModel

        Studies _ ->
          Html.map handleStudiesMessage <|
            Studies.view model.studiesModel

        Visits _ ->
          Html.map handleVisitsMessage <|
            Visits.view model.visitsModel

        NotFound ->
          NotFound.view NavigateTo ()
    ]
    


