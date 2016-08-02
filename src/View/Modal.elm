module View.Modal exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import View.Bootstrap exposing (..)
import ElmEscapeHtml exposing (unescape)

import List as L

type alias StaticModalInfo msg = 
  { id : String
  , title : String
  , titleId : Maybe String
  , label : String
  , body : Html msg
  , footerButtons : List (Html msg)
  , buttonContent : Html msg
  }

type alias ModalInfo model msg = model -> StaticModalInfo msg

buildModalButton : ModalInfo model msg -> model -> Html msg
buildModalButton info model = showModalButton <| info model

showModalButton : StaticModalInfo msg -> Html msg
showModalButton info =
  button
    [ type' "button"
    , classList
        [ ("btn", True)
        , ("btn-primary", True)
        ]
    , data "toggle" "modal"
    , data "target" <| "#" ++ info.id
    ]
    [ info.buttonContent
    ]


buildModal : ModalInfo model msg -> model -> Html msg
buildModal info model = showModal <| info model

showModal : StaticModalInfo msg -> Html msg
showModal info =
  div
    [ classList
        [ ("modal", True)
        , ("fade", True)
        ]
    , id info.id
    , tabindex -1
    , role "dialog"
    , aria "labelledby" info.label
    , aria "hidden" "true"
    ]
    [ div
        [ classList
            [ ("modal-dialog", True)
            ]
        , role "document"
        ]
        [ div 
            [ classList
                [ ("modal-content", True)
                ]
            ]
            [ div
                [ classList
                    [ ("modal-header", True)
                    ]
                ]
                [ button
                    [ type' "button"
                    , class "close"
                    , data "dismiss" "modal"
                    , aria "label" "Close"
                    ]
                    [ span
                        [ aria "hidden" "true"
                        ]
                        [ text <| unescape "&times;"
                        ]
                    ]
                , h4
                    ( [ classList
                          [ ("modal-title", True)
                          ]
                      ]
                      ++
                      ( case info.titleId of
                          Nothing ->
                            []
                          Just tId ->
                            [ id tId
                            ]
                      )
                    )
                    [ text info.title
                    ]
                ]
            , div
                [ classList
                    [ ("modal-body", True)
                    ]
                ]
                [ info.body
                ]
            , div
                [ classList
                    [ ("modal-footer", True)
                    ]
                ]
                ( [ button
                      [ type' "button"
                      , classList
                          [ ("btn", True)
                          , ("btn-secondary", True)
                          ]
                      , data "dismiss" "modal"
                      ]
                      [ text "Close"
                      ]
                  ]
                  ++
                  ( info.footerButtons )
                )
                {--
                , button
                    [ type' "button"
                    , classList
                        [ ("btn", True)
                        , ("btn-primary", True)
                        ]
                    ]
                    [ text "Save changes"
                    ]
                ]--}
            ]
        ]
    ]


