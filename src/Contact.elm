module Main exposing (..)
--module Pages.Contact exposing (..)
import Html.App

-- types
import ISO8601 exposing (Time)
import Date
import Maybe exposing (Maybe, withDefault)

-- decoder
import Json.Decode exposing (..)
import Utils.JsonDecoders exposing (..)
-- import String exposing (join) -- also in view, not used anyway
-- import Maybe exposing (Maybe, withDefault) -- also in types

-- http
import Http exposing (get)
import Task exposing (perform)

--  model
-- import Nav.Operations as NavOp -- not using operations
-- import Form exposing (..) -- used also by update

-- update
import Nav.RQ as NavRQ  -- RQ is short for Route and Query
-- import Cmd.Extra -- causes repl to crash
import Form exposing (Form)
import Form.Validate as FmVl 
--import Nav.Operations exposing (Operation (..))

-- view
import Html exposing (..)
import Html.Events
import Form.Input as Input
import VirtualDom
import String


---------------
-- TYPES
---------------

type alias CID       = Int
type alias PID       = Int
type alias CType     = String
type alias CValue    = String
type alias CRelation = String
type alias CForWhom  = String

-- mash all the contact info together
type alias Contact =
  { cid       : CID    
  , pid       : PID
  , ctype     : CType 
  , cvalue    : CValue
  , relation  : CRelation
  , forwhom   : CForWhom
  , nogood    : Bool
--  , added     : Maybe Time
  }

-- not used b/c form handles initialization
blankContact : Int -> Contact
blankContact n = 
  { cid       = n
  , pid       = 0
  , ctype     = ""
  , cvalue    = ""
  , relation  = ""
  , forwhom   = ""
  , nogood    = False
  --, added     = Nothing
  }

-- sometimes we might have a contact
-- make Nothing the blank contact
maybe2Contact : Maybe Contact -> Contact
maybe2Contact mc = Maybe.withDefault (blankContact 0) mc

-- postgrest gives us a list. just only the first
-- or get default if nothing
unlistContact : List Contact -> Contact
unlistContact lc = List.head lc |> maybe2Contact 

---------------
-- MODEL
---------------

type alias Model = { contact: Form () Contact , error : String,initc: Contact }

type Msg =
    
    -- add error to the model
    Error String
    
  -- elm-simple-form message
  | FormMsg Form.Msg

    -- recieve http get of contact
  | SetContact (List Contact)
  | UpdateID   CID

    -- will recurse from FetchError to Error
  | FetchError Http.Error 

    -- do nothing
  | NoOp

--------------
-- View
-------------

fm2str : (Form a b )-> String -> String
fm2str f s =
  case .value (Form.getFieldAsString s  f) of
    Nothing -> ""
    Just c  -> c

-- model.contact is
-- {cid, ctype, cvalue, relation, forwhom, nogood, added, note} 
-- model.error is a string
view : Model -> Html Msg
view model =
 let
   f : String -> String
   f  c   = fm2str model.contact c

   -- what the form model looks like.
   -- out = Form.getOutput model.contact |> Result.toMaybe |> Maybe.withDefault (blankContact 0) 
   
   cidint = String.toInt (fm2str model.contact "cid") |> Result.toMaybe |> Maybe.withDefault 0 

 in
   div []
    [ h2 [] [text "Contacts:"]
    , h3 [] [text (f "cid"), text (f "pid") ]
    -- , h3 [] [text out.cid]
    , h3 [] [text (toString model.initc.cid), text (toString model.initc.pid), text model.initc.ctype, text model.initc.relation ]
    , button [Html.Events.onClick  (UpdateID cidint) ] [text "click"] 
    , Html.App.map FormMsg (viewForm model.contact)
    , h1 [] [text model.error]
    ]
 

-- display the contact form
-- should not have cid or pid aviable for editing
viewForm: Form () Contact -> Html Form.Msg
viewForm form =
 let
  -- given a stringkey [label prop] [input prop]: make a label input pair
  labelInputPair: String -> 
      List (VirtualDom.Property Form.Msg) -> 
      List (VirtualDom.Property Form.Msg) -> 
      List (VirtualDom.Node Form.Msg) 
  labelInputPair k lab_prop in_prop  = 
    [ tr []
     [ td [] [label lab_prop [text k]  ] 
     , td [] [Input.textInput (Form.getFieldAsString k form ) in_prop ] 
     ]
    ]
 in
  table [] 
   (  (labelInputPair "cid"    [] []) --[Html.Events.onBlur updateOnId  ] )
   ++ (labelInputPair "pid"    [] [] )
   ++ (labelInputPair "ctype"  [] [] )
   ++ (labelInputPair "cvalue" [] [] )
   ++ (labelInputPair "relation" [] [] )
   ++ (labelInputPair "who" [] [] )
   )
  
 
---------------
-- DECODER 
---------------


contactDecoder : Decoder Contact
contactDecoder = succeed Contact
  |: ( "cid"      := int )
  |: ( "pid"      := int )
  |: ( "ctype"    := string )
  |: ( "cvalue"   := string )
  |: ( "relation" := string )
  |: ( "who"      := string )
  |: ( "nogood"      := Json.Decode.bool )
  -- |: (maybe <| "added" := date)

-- postgrest always returns a list. 
contactListDecoder : Decoder (List Contact)
contactListDecoder = nullOrList contactDecoder

---------------
-- HTTP
---------------

-- where we can get contact info
-- we want to excluded "added" ... because I haven' figure out how to add time or date to form validation
-- we'll just append id to the end of url
contactURL : String
contactURL = "/db/contact?select=cid,pid,ctype,cvalue,relation,who,nogood&cid=eq."

getCmd : CID -> Cmd Msg
getCmd n =
  Task.perform FetchError SetContact (Http.get contactListDecoder (contactURL++(toString n)))

-----------
-- UPDATE
-------------

validate : FmVl.Validation () Contact
validate = 
--  FmVl.form2  Contact (FmVl.get "cid" FmVl.int) (FmVl.get "ctype" FmVl.string )
 FmVl.succeed Contact
  `FmVl.apply` (FmVl.get "cid"      FmVl.int    )
  `FmVl.apply` (FmVl.get "pid"      FmVl.int    )
  `FmVl.apply` (FmVl.get "ctype"    FmVl.string )
  `FmVl.apply` (FmVl.get "cvalue"   FmVl.string )
  `FmVl.apply` (FmVl.get "relation" FmVl.string )
  `FmVl.apply` (FmVl.get "who"      FmVl.string )
  `FmVl.apply` (FmVl.get "nogood"   FmVl.bool   )
  -- `FmVl.apply` (FmVl.maybe <|(FmVl.get "added"    FmVl.date ))
   

-- init : NavRQ.RQ -> (Model, Cmd Msg)
-- init rq = ( { contact = blankContact 0, error = "" } , Cmd.none)
init : (Model, Cmd Msg)
init = ( { contact = Form.initial [] validate , error = "", initc = blankContact 0 } , Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  -- change contact info (on click do this)
  UpdateID   n   -> ( model, getCmd n)
  --                     (on success of http do this)
  SetContact c   -> ( { model | initc = (unlistContact  c)    } , Cmd.none)

  -- add error to model
  Error string   -> ( { model | error = string } , Cmd.none)

  -- go to Error from FetchError
  -- N.B. cause repl to crash
  -- FetchError err -> ( model                      , Cmd.Extra.message (Error (toString err)) )
  -- or just do it
  FetchError err -> ( { model | error = (toString err)},Cmd.none)

  -- updating the form updates the model
  FormMsg fm -> ({model | contact = Form.update fm model.contact},Cmd.none) 

  -- do nothing
  NoOp ->           ( model                      , Cmd.none)



urlUpdate : NavRQ.RQ -> Model -> (Model, Cmd Msg)
urlUpdate rq model = ( model , Cmd.none)


--------------
-- MAIN
--------------

main : Program Never
main = Html.App.program
  { init = init
  , view = view
  , update = update
  , subscriptions = (always Sub.none)
  }
