module Components.Contacts.View exposing (..)

-- import Http exposing (..)
-- import Task exposing (..)
import Maybe exposing (..)
import Html exposing (..)
import Html.App exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Types.ContactInfo exposing (..)
import Components.Contacts.Model exposing (..)
-- import Types.ContactInfo.JsonDecoders exposing (..)

-- import Result exposing (toMaybe)
-- import String exposing (toInt)

import Regex as R
import String as S
import List as L
import Utils as U

contactString : Int -> Contact -> String
contactString n contact = 
  let
    cType = contact.contactType
    ct =
      if n == (S.length cType)
      then
        cType
      else
        S.dropRight n cType
  in
    
  ct ++ ": " ++ contact.content
    ++ " (" ++ (if contact.notGood then "invalid" else "valid")
    ++")"

splitMail : Int -> Contact -> List String
splitMail n contact = 
  let
    conts = R.split (R.All) (R.regex "\\s*;\\s*") contact.content
  in
    L.map
      (\str ->
        contactString n { contact | content = str }
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
    mailStrings = L.concatMap (splitMail 5) mails
    phoneStrings = L.map (contactString 5) phones
    restStrings = L.map (contactString 0) rest2
    rows = U.transpose "" [mailStrings,phoneStrings,restStrings]
  in
    table [] <| L.map ((tr []) << (L.map ((td []) << U.singleton << text))) (["Emails","Phone Numbers","Other"]::rows)



viewCI : ContactInfo -> Html msg
viewCI ci = 
  div []
    [ text <| ci.relation ++ ": " ++ ci.name
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

