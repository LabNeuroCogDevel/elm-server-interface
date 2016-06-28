module Utils.Html exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode exposing (..)

import Json.Decode as JD


onChange : (String -> msg) -> Attribute msg
onChange handler = on "change" (map handler <| at ["target", "value"] string)
