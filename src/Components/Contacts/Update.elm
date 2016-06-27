module Components.Contacts.Update exposing (..)

import Maybe exposing (..)
import Types.ContactInfo exposing (..)
import Types.ContactInfo.JsonDecoders exposing (..)
import Components.Contacts.Model exposing (..)

import Result exposing (toMaybe)
import String exposing (toInt)
import Http exposing (get)
import Task exposing (perform)




testUrl : String
testUrl = "http://localhost:3003/contacts_view?pid=eq."


ciCmd : Int -> Cmd Msg
ciCmd n = 
  perform (Error << toString) ContactStuff
    <| get ciListDecoder (testUrl ++ toString n)


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

