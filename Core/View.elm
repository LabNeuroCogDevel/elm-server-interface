module Core.View exposing (..)

import Nav.Pages exposing (..)
import Nav.Routes exposing (..)
import Core.Model exposing (..)
import Either exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Platform.Cmd exposing (Cmd)

import Pages.People as People
import Pages.NotFound as NotFound
import Html.App as Html
import Html.Attributes as Atts


handlePeopleMessage : People.Msg -> Msg
handlePeopleMessage m = PeopleMsg m

view : Model -> Html Msg
view model =
  div []
    [ text <| toString model.routeQuery
    {--
    , div [ class "alert alert-danger" ]
        [ text model.errorMsg ]
    --}
    , case model.routeQuery.page of
        RootPage ->
          Html.map handlePeopleMessage <|
            People.view model.peopleModel
        
        PeoplePage _ ->
          Html.map handlePeopleMessage <|
            People.view model.peopleModel

        NotFoundPage ->
          NotFound.view NavigateTo ()
    ]
    


