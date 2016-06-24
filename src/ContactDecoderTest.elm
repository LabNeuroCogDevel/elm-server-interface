module ContactDecoderTest exposing (..)

import Http exposing (..)
import Task exposing (..)
import Maybe exposing (..)
import Html exposing (..)
import Html.App exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Types.ContactInfo exposing (..)
import Types.ContactInfo.JsonDecoders exposing (..)

import Result exposing (toMaybe)
import String exposing (toInt)

import Regex as R
import String as S
import List as L

testUrl : String
testUrl = "http://localhost:3003/contacts_view?pid=eq."

singleton : a -> List a 
singleton x = [x]

type alias Model = (String,Maybe (List ContactInfo))

type Msg
  = NoOp
  | Error String
  | NumberChange String
  | ContactStuff (List ContactInfo)

ciCmd : Int -> Cmd Msg
ciCmd n = 
  perform (Error << toString) ContactStuff
    <| get ciListDecoder (testUrl ++ toString n)

init : (Model, Cmd Msg)
init = (("",Nothing), ciCmd 5)

update : Msg -> Model -> (Model, Cmd Msg)
update msg (debug,model) = case msg of
  NoOp ->
    ((debug ++ " noop",model), Cmd.none)

  Error str ->
    ((debug ++ " err: " ++ str,model), Cmd.none)

  NumberChange numStr ->
    let
      n = withDefault 5
        <| Result.toMaybe
        <| toInt numStr
    in
      ((debug ++ " nc",model), ciCmd n)
        
  ContactStuff cs ->
    ((debug ++ " cs",Just cs), Cmd.none)


viewContact : Contact -> Html msg
viewContact contact = 
  div [] <|
    [ span []
        [ text <| contact.contactType ++ ": " ++ contact.content
            ++ " (" ++ (if contact.notGood then "invalid" else "valid")
            ++")"
        ]
    ]
    ++
    (if contact.notes == ""
    then
      []
    else 
      [ p []
          [ text <| "Notes: " ++ contact.notes ]
      ]
    )

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


transpose : a -> (List (List a)) -> (List (List a))
transpose def cols =
  if L.all L.isEmpty cols
  then
    []
  else
    (L.map (withDefault def << L.head) cols) :: (transpose def <| L.map (withDefault [] << L.tail) cols)


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
    rows = transpose "" [mailStrings,phoneStrings,restStrings]
  in
    table [] <| L.map ((tr []) << (L.map ((td []) << singleton << text))) (["Emails","Phone Numbers","Other"]::rows)



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

main
  = program 
      { init = init
      , update = update
      , subscriptions = always Sub.none
      , view = view
      }


