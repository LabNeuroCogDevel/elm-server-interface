module Pages.People exposing (..)

import Types.Either exposing (..)

import Pages.People.View as V
import Pages.People.Update as U
import Pages.People.Model as M

import Nav.Routes exposing (Route)
import Nav.RQ exposing (RQ)
import Platform.Cmd exposing (Cmd)
import Html exposing (Html)


-- hmm
view : M.Model -> Html M.Msg
view = V.view

update : M.Msg -> M.Model -> (M.Model, Cmd M.Msg)
update = U.update

init : RQ -> (M.Model, Cmd M.Msg)
init = U.init

type alias Model = M.Model

type alias Msg = M.Msg

urlUpdate : RQ -> M.Model -> (M.Model, Cmd M.Msg)
urlUpdate = U.urlUpdate
