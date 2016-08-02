module View.Bootstrap exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes as Atts


row : List (Html msg) -> Html msg
row content =
  div [ class "row" ] content

col' : Int -> List (Html msg) -> Html msg
col' i content =
  div [ class ("col-xs-" ++ toString i) ] content

role : String -> Attribute msg
role = attribute "role" 

dataToggle : String -> Attribute msg
dataToggle = attribute "data-toggle" 

data : String -> String -> Attribute msg
data typ value = attribute ("data-"++typ) value

aria : String -> String -> Attribute msg
aria typ value = attribute ("aria-"++typ) value


errorClass : Maybe error -> String
errorClass maybeError =
  Maybe.map (\_ -> "has-error") maybeError |> Maybe.withDefault ""


{--
errorMessage : Maybe (Error CustomError) -> Html msg
errorMessage maybeError =
  case maybeError of
    Just error ->
      p
        [ class "help-block" ]
        [ text (toString error) ]
    Nothing ->
      span
        [ class "help-block" ]
        [ text " " ] -- &#8199;
--}
