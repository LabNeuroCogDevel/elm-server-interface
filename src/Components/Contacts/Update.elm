module Components.Contacts.Update exposing (..)

import Maybe exposing (..)
import Types.ContactInfo exposing (..)
import Types.ContactInfo.JsonDecoders exposing (..)
import Components.Contacts.Model exposing (..)

import Result exposing (toMaybe)
import String exposing (toInt)
import Components.Contacts.HttpCmds exposing (getCICmd)


-- turn an contanct int into a cmd msg
-- use HttpCmds.elm getCICmd 
ciCmd : Int -> Cmd Msg
ciCmd = getCICmd Error ContactStuff


init : (Model, Cmd Msg)
init = (("",Nothing), ciCmd 5)


update : Msg -> Model -> (Model, Cmd Msg)
update msg (debug,model) = case msg of
  NoOp ->
    ((debug ++ " noop",model), Cmd.none)

  Error str ->
    ((debug ++ " err: " ++ str,model), Cmd.none)

  NumberChange numStr ->
    let
      n = withDefault 5
        <| Result.toMaybe
        <| toInt numStr
    in
      ((debug ++ " nc",model), ciCmd n)
        
  ContactStuff cs ->
    ((debug ++ " cs",Just cs), Cmd.none)
