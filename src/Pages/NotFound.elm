module Pages.NotFound exposing (..)

import Nav.Routes exposing (..)
import Nav.Queries exposing (..)
import Platform.Cmd exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


--init : ((), Cmd Msg)
--init = ((), Cmd.none)

view : (Maybe Route -> Maybe Query -> msg) -> () -> Html msg
view navTo _ = 
  div []
    [ h1 []
        [ text "Error page not found!"
        ]
    , input [ type' "button"
            , class "btn btn-default"
            , onClick <| navTo (Just Root) Nothing
            , value "Go Home"
            ] []
    ]


