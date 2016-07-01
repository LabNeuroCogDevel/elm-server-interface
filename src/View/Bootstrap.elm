module View.Bootstrap exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes as Atts
import Form exposing (Form, FieldState)
import Form.Input as Input
import Form.Error exposing (Error)
import Pages.People.Model exposing (..)


row : List (Html msg) -> Html msg
row content =
  div [ class "row" ] content

col' : Int -> List (Html msg) -> Html msg
col' i content =
  div [ class ("col-xs-" ++ toString i) ] content


type alias GroupBuilder a = String -> FieldState CustomError a -> Html Form.Msg


formGroup : String -> Maybe (Error CustomError) -> List (Html Form.Msg) -> Html Form.Msg
formGroup label' maybeError inputs =
  div
    [ class ("row form-group " ++ (errorClass maybeError)) ]
    [ col' 3
        [ label [ class "control-label" ] [ text label' ] ]
    , col' 5
         inputs
    , col' 4
        [ errorMessage maybeError ]
    ]

formTCell : String -> (String, FieldState CustomError String, Int, Int, Int) -> Html Form.Msg
formTCell formid (label', state, i, l, nc) =
  td
    [ class <| "form-group " ++ (errorClass state.liveError)
    , colspan nc
    ]
    [ Input.textInput state [ class <| "form-control "++(errorClass state.liveError)
                            , placeholder label'
                            , attribute "form" formid
                            , tabindex i
                            , style
                                [("min-width",(toString l)++"em")]
                            ]
    ]


formActions : List (Html msg) -> Html msg
formActions content =
  row
    [ div [ class "col-xs-offset-3 col-xs-9" ] content ]


textGroup : GroupBuilder String
textGroup label' state =
  formGroup label' state.liveError
    [ Input.textInput state [ class "form-control" ] ]


textAreaGroup : GroupBuilder String
textAreaGroup label' state =
  formGroup label' state.liveError
    [ Input.textArea state [ class "form-control" ] ]


checkboxGroup : GroupBuilder Bool
checkboxGroup label' state =
  formGroup "" state.liveError
    [ div
        [ class "checkbox" ]
        [ label []
            [ Input.checkboxInput state []
            , text label'
            ]
        ]
    ]


selectGroup : List (String, String) -> GroupBuilder String
selectGroup options label' state =
  formGroup label' state.liveError
    [ Input.selectInput options state [ class "form-control" ] ]


radioGroup : List (String, String) -> GroupBuilder String
radioGroup options label' state =
  let
    item (v, l) =
      label
        [ class "radio-inline" ]
        [ Input.radioInput state.path state [ value v ]
        , text l
        ]
  in
    formGroup label' state.liveError
      (List.map item options)


errorClass : Maybe error -> String
errorClass maybeError =
  Maybe.map (\_ -> "has-error") maybeError |> Maybe.withDefault ""


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
