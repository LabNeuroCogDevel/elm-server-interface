module ContactDecoderTest exposing (..)

import Http exposing (..)
import Task exposing (..)
import Maybe exposing (..)
import Html exposing (..)
import Html.App exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Types.ContactInfo exposing (..)
import Types.ContactInfo.JsonDecoders exposing (..)

import Result exposing (toMaybe)
import String exposing (toInt)

testUrl : String
testUrl = "http://localhost:3003/contacts_view?pid=eq."


type alias Model = (String,Maybe (List ContactInfo))

type Msg
  = NoOp
  | Error String
  | NumberChange String
  | ContactStuff (List ContactInfo)

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

view : Model -> Html Msg
view model =
  div []
    [ input
        [ type' "text"
        --, value "5"
        , onInput NumberChange
        ] []
    , text <| toString model
    ]

main
  = program 
      { init = init
      , update = update
      , subscriptions = always Sub.none
      , view = view
      }


