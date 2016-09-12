module Pages.Visits exposing (..)

import Pages.Visits.View as V
import Pages.Visits.Model as M
import Pages.Visits.Update as U

import Html exposing (Html)
import Nav.RQ exposing (RQ)

type alias Msg = M.Msg

type alias Model = M.Model

view : Model -> Html Msg
view = V.view

init : RQ -> (Model, Cmd Msg)
init = U.init

update : Msg -> Model -> (Model, Cmd Msg)
update = U.update

urlUpdate : RQ -> Model -> (Model, Cmd Msg)
urlUpdate = U.urlUpdate
