module Main exposing (..)
--module Pages.Contact exposing (..)
-- main
import Html.App
import Navigation
-- import Nav.RQ as NavRQ  -- RQ is short for Route and Query (imported in update)

-- types
import ISO8601 exposing (Time)
import Date
import Maybe exposing (Maybe, withDefault)

-- decoder
import Json.Decode exposing (..)
import Utils.JsonDecoders exposing (..)
import Json.Encode 
-- import Utils.JsonEncoders as UEncode -- does date stuff
-- import String exposing (join) -- also in view, not used anyway
-- import Maybe exposing (Maybe, withDefault) -- also in types


-- http
import Http exposing (get)
import Task exposing (perform)
-- import Dict -- not needed

--  model
-- import Nav.Operations as NavOp -- not using operations
-- import Form exposing (..) -- used also by update

-- update
import Nav.RQ as NavRQ  -- RQ is short for Route and Query
import Cmd.Extra -- causes repl to crash
import Form exposing (Form)
import Form.Validate as FmVl 
import String
--import Nav.Operations exposing (Operation (..))

-- view
import Html exposing (..)
import Html.Events
import Form.Input as Input
import Form.Field as Field
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


--
-- handling types
--

-- sometimes we might have a contact
-- make Nothing the blank contact
maybe2Contact : Maybe Contact -> Contact
maybe2Contact mc = Maybe.withDefault (blankContact 0) mc

-- postgrest gives us a list but we wanted just one value
-- just only the first
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
  | ContactToForm
  | ChangeContact   CID
  | Refresh 
  -- run updateCmd
  | Update

    -- will recurse from FetchError to Error
  | FetchError Http.Error 

    -- do nothing
  | NoOp

--------------
-- View
-------------

-- form to string
-- give me a form and the element/id you want out of it
-- i'll give you a string
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

   -- convient: specify the form in model for fm2str so we dont have to keep rewritting it
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
    , button [Html.Events.onClick  (ChangeContact cidint) ] [text "click"] 
    , button [Html.Events.onClick  Update                 ] [text "update"] 
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
   ++ (labelInputPair "forwhom" [] [] )
   )
  
 
---------------
-- JSON: DECODER + ENCODER
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


contactEncoder : Contact -> String
contactEncoder c =
  Json.Encode.encode 0 (Json.Encode.object
    [ -- ("cid"     , Json.Encode.int    c.cid)
      ("pid"     , Json.Encode.int    c.pid)
    , ("ctype"   , Json.Encode.string c.ctype)
    , ("cvalue"  , Json.Encode.string c.cvalue)
    , ("relation", Json.Encode.string c.relation)
    , ("who"     , Json.Encode.string c.forwhom)
    , ("nogood"  , Json.Encode.bool   c.nogood)
    ])


---------------
-- HTTP
---------------
-- AuthToken is just a string ... maybe
-- We need it for the authorization header (postgrest jwt)
type alias AuthToken = Maybe String
authTokenToHeader : AuthToken -> List (String, String)
authTokenToHeader token =
   case token of
      Just token' -> [("Authorization", "Bearer "++token')]
      Nothing -> []

-- main component of rewrite Http.post and Http.get to accept authorization token (jwt)
authorizedSend : String -> String -> AuthToken-> List (String,String)->Http.Body -> (Decoder value) ->
                 Platform.Task Http.Error value
authorizedSend url verb token headers body dec =
  let 
    req = { url = url
          , verb = verb
          , headers =  headers ++ (authTokenToHeader token) 
          , body = body
          }
  in
    Http.fromJson dec (Http.send Http.defaultSettings req)

-- send and do nothing on success
-- we need this for patch (update) b/c server can return nothing and succeed
-- also need to pull promoteError out of http package (not epxosed)
{-promoteError : Http.RawError -> Decoder a -- Platform.Task Http.Error a
promoteError res =
    case res of
        Http.RawTimeout      -> Http.Timeout
        Http.RawNetworkError -> Http.NetworkError
        _ -> if 200 <= res.status && res.status < 300 then
               res.value
             else
               fail (Http.BadResponse res.status res.statusText)
-}
promoteError : Http.RawError -> Http.Error
promoteError rawError =
  case rawError of
      Http.RawTimeout      -> Http.Timeout
      Http.RawNetworkError -> Http.NetworkError

handleResponse :  Http.Response -> Task.Task Http.Error String 
handleResponse response =
  if 200 <= response.status && response.status < 300 then
    case response.value of
        Http.Text str -> Task.succeed str 
        _ -> Task.fail (Http.UnexpectedPayload "Response body is a blob, expecting a string.")
  else 
    Task.fail (Http.BadResponse response.status response.statusText)


authorizedSendNoRecieve : String -> String -> AuthToken-> List (String,String)->Http.Body -> 
                 Platform.Task Http.Error String
authorizedSendNoRecieve url verb token headers body =
  let 
    req = { url = url
          , verb = verb
          , headers =  headers ++ (authTokenToHeader token) 
          , body = body
          }
  in
    Task.mapError promoteError (Http.send Http.defaultSettings req) `Task.andThen` handleResponse

-- rewrite Http.get (take decoder and url) to included authkey (maybe string)
authorizedGet : (Decoder value) -> String -> AuthToken -> Platform.Task Http.Error value
authorizedGet dec url token =
  authorizedSend url "GET" token [] Http.empty dec

-- rewrite Http.post
authorizedPost : (Decoder value) -> String -> Http.Body -> AuthToken -> Platform.Task Http.Error value
authorizedPost dec url body token =
  authorizedSend url "POST" token [] body dec

-- where we can get contact info
contactURL : String
contactURL = "/db/contact"

-- what do we want to fetch when searching
getContactURL : String
getContactURL = contactURL++"?select=cid,pid,ctype,cvalue,relation,who,nogood&cid=eq."
-- we want to excluded "added" b/c I haven't figure out how to add time or date to form validation
-- we'll just append id to the end of url

-- using a contact id and maybe an authentication token
-- populate contact (or error) (messages to update)
getCmd : CID -> AuthToken -> Cmd Msg
getCmd n token =
  let
    url = (getContactURL++(toString n) )
    get = (authorizedGet contactListDecoder url token)
  in
    Task.perform FetchError SetContact get
                 
    --(Http.get contactListDecoder (getContactURL++(toString n)))

-- if we are creating a new contact we use post
createCmd : Model -> AuthToken -> Cmd Msg
createCmd model token  =
  let
    body = contactEncoder model.initc 
    header=[("Prefer", "return=representation")]
    req = authorizedSend contactURL "POST" token header (Http.string body) contactListDecoder 
    -- req = authorizedSend contactURL "PATCH" token [] body dec
  in
    -- fetch cid again after update
    Task.perform FetchError SetContact req

-- if we are creating a new contact we use post
updateCmd : Model -> AuthToken -> Cmd Msg
updateCmd model token  =
  let
    url  = contactURL ++ "?cid=eq." ++ (toString model.initc.cid)
    body = contactEncoder (form2contact model.contact)
    req  = authorizedSendNoRecieve url "PATCH" token [("Content-Type", "application/json")] (Http.string body) 
  in
    -- fetch cid again after update
    Task.perform FetchError (\x->Refresh) req


-------------
-- UPDATE
-------------

-- b/c we are using the form instead of the record directly
-- we need a default form data structure 
fields = [(.cid >> toString,     "cid")
         ,(.pid >> toString,     "pid")
         ,(.ctype,   "ctype")
         ,(.cvalue,  "cvalue")
         ,(.relation,"relation")
         ,(.forwhom, "forwhom")
         ,(.nogood >>toString,  "nogood")]

-- make tuplet from a single entry in field 
-- e.g. ("cvalue",  Field.Text c.cvalue)
field2form : Contact -> (Contact->String,String) -> (String,Field.Field)
field2form c e = ( (snd e), Field.Text ((fst e) c) ) 

-- use field2from to work on the whole record
record2form : Contact -> List (String,Field.Field)
record2form c = List.map (\e -> field2form c e)  fields

{-
  -- could write above like this
  -- this is cleaner and easier to understand, but repeatative

   record2form : Contact -> List (String,Field.Field)
   record2form c = 
    [ ("cid",     Field.Text (toString c.cid))
    , ("pid",     Field.Text (toString c.pid))
    , ("cvalue",  Field.Text c.cvalue)
    , ("relation",Field.Text c.relation)
    , ("forwhom", Field.Text c.forwhom)
    , ("nogood",  Field.Text (toString c.nogood))
    ]
-}

-- default for numbers
-- handle maybe and toInt
maybe0 : Maybe String -> Int
maybe0 n = 
 case n of 
  Nothing -> 0 
  Just v -> 
    case (String.toInt v ) of
      Err msg -> 0
      Ok  i -> i

maybeFalse : Maybe Bool -> Bool
maybeFalse b =
 case b of 
   Nothing -> False
   Just bv -> bv

-- when we send the data we want to take it out of the form
-- and put it back into the data
form2contact : (Form a b) -> Contact
form2contact f =
  { cid      = Form.getFieldAsString "cid"      f |> .value |> maybe0
  , pid      = Form.getFieldAsString "pid"      f |> .value |> maybe0
  , cvalue   = Form.getFieldAsString "cvalue"   f |> .value |> Maybe.withDefault "" 
  , ctype    = Form.getFieldAsString "ctype"    f |> .value |> Maybe.withDefault "" 
  , relation = Form.getFieldAsString "relation" f |> .value |> Maybe.withDefault ""  
  , forwhom  = Form.getFieldAsString "forwhom"  f |> .value |> Maybe.withDefault ""  
  , nogood   = Form.getFieldAsBool   "nogood"   f |> .value |> maybeFalse
  }



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
init = ( { contact = Form.initial (record2form (blankContact 0)) validate , error = "", initc = blankContact 0 } , Cmd.none)

-- reset the form to be whatever is in "initc"
updateFormModel : Model -> Model
updateFormModel model =
 {model | contact = Form.initial (record2form model.initc) validate}

-- the logic switchboard
update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  -- change contact info (on click do this or on route parse)
  ChangeContact n-> ( model, getCmd n Nothing)
  --                     (on success of http do initc changes, and then form changes)
  SetContact c   -> ( { model | initc = (unlistContact  c)    } 
                    , Cmd.Extra.message ContactToForm )

  --                 put initc into contact
  ContactToForm  -> (  updateFormModel model, Cmd.none)

  -- did we do something? update values to make sure change took
  -- will run SetContact which will run ContactToForm
  Refresh        -> ( model, getCmd model.initc.cid Nothing )
  Update         -> ( model, updateCmd model Nothing)

  -- add error to model
  Error string   -> ( { model | error = string } , Cmd.none)

  -- go to Error from FetchError
  -- N.B. cause repl to crash
  -- FetchError err -> ( model, Cmd.Extra.message (Error (toString err)) )
  -- or just do it
  FetchError err -> ( { model | error = (toString err) }
                    , Cmd.none)

  -- updating the form updates the model -- handled by simple form library
  FormMsg fm ->     ( { model | contact = Form.update fm model.contact }
                    , Cmd.none) 

  -- do nothing
  NoOp ->           (model, Cmd.none)



urlUpdate : NavRQ.RQ -> Model -> (Model, Cmd Msg)
urlUpdate rq model = ( model , Cmd.none)


--------------
-- MAIN super simple
--------------

main : Program Never
main = Html.App.program
  { init = init
  , view = view
  , update = update
  , subscriptions = (always Sub.none)
  }

-- --------------
-- -- MAIN with routes
-- --------------
-- app = Navigation.program NavRQ.urlParser
--   { init = init
--   , update = update
--   , view = view
--   , urlUpdate = urlUpdate
--   , subscriptions = \_ -> Sub.none
--   }
-- main = app
-- 
