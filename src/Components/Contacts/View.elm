module Components.Contacts.View exposing (..)

import Maybe exposing (..)
import Html exposing (..)
import Html.App exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Types.ContactInfo exposing (..)
import Components.Contacts.Model exposing (..)

import Char exposing (isDigit)
import ElmEscapeHtml exposing (unescape)
import Utils exposing (bindF)

import Regex as R
import String as S
import List as L
import Utils.List as U


dropUnlessEmpty : Int -> String -> String
dropUnlessEmpty n str = 
  if n >= (S.length str)
  then
    str
  else
    S.dropRight n str
    
showPhone : String -> String
showPhone num =
  let
    digs = S.filter isDigit num
  in 
    if (S.length digs == 10)
    then
      "("
      ++
      (S.left 3 digs)
      ++
      ")"
      ++
      (unescape "&nbsp;")
      ++
      (S.slice 3 -4 digs)
      ++
      "-"
      ++
      (S.right 4 digs)
    else
      num


contactString : (String -> String) -> (String -> String) -> Contact -> String
contactString handleType handleValue contact = 
  (handleType contact.contactType) ++ ": " ++ (handleValue contact.content)
    ++ " (" ++ (if contact.notGood then "invalid" else "valid")
    ++")"

splitMail : (String -> String) -> (String -> String) -> Contact -> List String
splitMail ht hv contact = 
  let
    conts = R.split (R.All) (R.regex "\\s*;\\s*") contact.content
  in
    L.map
      (\str ->
        contactString ht hv { contact | content = str }
      )
      conts

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
    table [ class "table" ]
      [ thead [ class "" ]
          [ tr [] 
              <| L.map ((th [ ]) << U.singleton << text) ["Emails","Phone Numbers","Other"]
          ]
      , tbody [ ]
          <| L.map ((tr []) << (L.map ((td []) << U.singleton << text))) rows
      ]


viewCI : List (Html msg) -> ContactInfo -> Html msg
viewCI customHtml ci = 
  div []
    <|
    [ h4 [] [ text <| ci.relation ++ ": " ++ ci.name ]
    , hr [] []
    ]
    ++ customHtml ++
    [ viewContacts ci.contacts
    ]


viewCIs : (ContactInfo -> List (Html msg)) -> List ContactInfo -> Html msg
viewCIs customHtml model =
  div []
    <| L.intersperse (hr [] [])
    <|
    ( L.map (bindF customHtml viewCI)
      <| let
           (subj, rest) = L.partition (((==) "Subject") << .relation) model
         in
           subj ++ rest
    )


view : Model -> Html Msg
view (debug, model) =
  div []
    [ viewCIs (always []) <| withDefault [] model
    ]
