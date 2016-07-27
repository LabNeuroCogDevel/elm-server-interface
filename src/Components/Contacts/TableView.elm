module Components.Contacts.TableView exposing (..)

import Maybe exposing (..)
import Html exposing (..)
import Html.App exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Types.ContactInfo exposing (..)
import Components.Contacts.Model exposing (..)
import Components.Contacts.View exposing (splitMail,contactString,dropUnlessEmpty,showPhone)

import Char exposing (isDigit)
import ElmEscapeHtml exposing (unescape)

import Regex as R
import String as S
import List as L
import Utils.List as U

viewContacts : List Contact -> Html msg
viewContacts contacts =
  let
    (mails,rest) =
      L.partition 
        ( (  R.contains
          <| R.caseInsensitive 
          <| R.regex
          <| "email"
          )
        <<
          (.contactType)
        )
        contacts
    (phones,rest2) = 
      L.partition 
        ( (  R.contains
          <| R.caseInsensitive 
          <| R.regex
          <| "phone"
          )
        <<
          (.contactType)
        )
        rest
    mailStrings = L.concatMap (splitMail (dropUnlessEmpty 5) identity) mails
    phoneStrings = L.map (contactString (dropUnlessEmpty 5) showPhone) phones
    restStrings = L.map (contactString identity identity) rest2
    rows = U.transpose "" [mailStrings,phoneStrings,restStrings]
  in
    table [ class "table table-striped" ]
      [ thead [ class "thead-inverse" ]
          [ tr [] 
              <| L.map ((th [ ]) << U.singleton << text) ["Emails","Phone Numbers","Other"]
          ]
      , tbody [ ]
          <| L.map ((tr []) << (L.map ((td []) << U.singleton << text))) rows
      ]



viewCI : ContactInfo -> Html msg
viewCI ci = 
  div []
    [ h4 [] [ text <| ci.relation ++ ": " ++ ci.name ]
    , viewContacts ci.contacts
    ]


viewCIs : List ContactInfo -> Html msg
viewCIs model =
  div []
    <| L.map viewCI
    <| let
         (subj, rest) = L.partition (((==) "Subject") << .relation) model
       in
         subj ++ rest


view : Model -> Html Msg
view (debug, model) =
  div []
    [ input
        [ type' "text"
        --, value "5"
        , onInput NumberChange
        ] []
    , br [] []
    , text debug
    , br [] []
    , viewCIs <| withDefault [] model
    ]

