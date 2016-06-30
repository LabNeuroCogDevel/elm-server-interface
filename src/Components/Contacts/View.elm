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
import Char exposing (isDigit)

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
      ") "
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

