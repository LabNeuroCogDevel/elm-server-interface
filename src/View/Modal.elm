module View.Modal exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import View.Bootstrap exposing (..)
import ElmEscapeHtml exposing (unescape)

import List as L

import Utils

type alias ModalButtonInfo msg =
  { attributes    : List (Attribute msg) -- the list of attributes for the button
  , buttonContent : List (Html msg)      -- the button's inner html
  }

--:'<,'>!sed -E 's/\s*([{,:]|--)\s*/\/\1\//g' | column -t -s '/' -o ' '

type alias StaticModalInfo msg = 
  { id            : String              -- the modal's id
  , title         : String              -- the title of the modal
  , titleId       : Maybe String        -- optional id for the modal's title tag
  , label         : String              -- label for accessibility
  , body          : Html msg            -- modal body
  , footerButtons : List (Html msg)     -- buttons to add to footer in addition to cancel (possibly other html)
  , buttonContent : ModalButtonInfo msg -- content for button to open modal
  }

{-- Static button attribute modifier
 -- use like:
 --   with buttonAttsS (atts) showModalButton (staticmodinfo)
 --}
buttonAttsS : List (Attribute msg) -> StaticModalInfo msg -> StaticModalInfo msg
buttonAttsS list modinfo =
  let
    defBC = modinfo.buttonContent
  in 
    { modinfo
    | buttonContent =
        { defBC
        | attributes = list
        }
    }



-- dynamic modal info that depends on the model
type alias ModalInfo model msg = model -> StaticModalInfo msg


-- set the attributes for the button, use like buttonAttsS above
buttonAtts : (model -> List (Attribute msg)) -> ModalInfo model msg -> ModalInfo model msg
buttonAtts = Utils.lift2F buttonAttsS



{-- since a ModalInfo is just a function that takes a model and produces
 -- static modal info, directly feed that into showModalButton
 --}
buildModalButton : ModalInfo model msg -> model -> Html msg
buildModalButton info = showModalButton << info



-- does what it says on the can, show a static modal
-- use buildModalButton to build a modal that depends on
-- the current state
showModalButton : StaticModalInfo msg -> Html msg
showModalButton info =
  button
    ( [ type' "button"
      , classList
          [ ("btn", True)
          , ("btn-primary", True)
          ]
      , data "toggle" "modal"
      , data "target" <| "#" ++ info.id
      ]
      ++ 
      info.buttonContent.attributes
    )
    info.buttonContent.buttonContent


-- for buildModal, showModal, see the button equivalents above
buildModal : ModalInfo model msg -> model -> Html msg
buildModal info = showModal << info


-- for buildModal, showModal, see the button equivalents above
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


