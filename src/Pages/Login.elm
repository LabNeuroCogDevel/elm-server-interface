-- This is a generic template jason created 
-- WF20160808 copy of Visit

module Pages.Login exposing (..)

import Types.Either exposing (..)

import Pages.Login.View as V
import Pages.Login.Update as U
import Pages.Login.Model as M

import Nav.Routes exposing (Route)
import Nav.RQ exposing (RQ)
import Platform.Cmd exposing (Cmd)
import Html exposing (Html)


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


