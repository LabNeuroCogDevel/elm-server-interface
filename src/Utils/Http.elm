module Utils.Http exposing (..)


import Http exposing (..)
import Json.Decode exposing (..)
import Nav.Paging exposing (..)
import Core.Model exposing (..)
import Utils.JsonEncoders exposing (..)
--import Result exposing (..)

import Task exposing (Task)
import Dict exposing (Dict)

import String
import Regex

import Strng as S
import Task as T
import Result as R
import Dict as D
import List as L

type Tag
  = Create
  | Read
  | Update
  | Delete

type alias HttpResult a = (a, Dict String String)

type alias CRUDInfo a = 
  { baseUrl : String
  , getId : a -> Int
  , idField : String
  , decode : Decoder a
  , encode : Encoder a
  , headers : Tag -> Dict String String
  }


type alias SuccessHandler a r = HttpResult a -> r
type alias ErrorHandler r = Error -> r

type alias Handler a r = (SuccessHandler a r, ErrorHandler r)


handle : Handler a r -> Cmd (Result Error HttpResult a) -> Cmd r
handle (s, f) =
  Cmd.map
    (\res -> case res of
      Err e ->
        f e
      Ok ret ->
        s ret
    )


headers : List (String, String) -> CrudInfo a -> CrudInfo a
headers heads info = 
  { info
  | headers = D.union (D.fromList heads) << info.headers
  }

--
create : CRUDInfo a -> a -> Cmd (Result Error HttpResult a)
create info val =
  let 
    headDict = info.headers Create
    default =
      D.fromList
        [ ("Prefer", "return=representation")
        , ("Content-Type", "application/json")
        ]
    heads = D.toList <| D.union headDict default
  in 
    postWithHeaders info.encode info.decode info.url heads val


--
read : CRUDInfo a -> Dict String String -> Cmd (Result Error (HttpResult (List a)))
read info query =
  getWithHeaders (list info.decode) (url info.url (D.fromList query)) (info.headers Read)

--
update : CRUDInfo a -> a -> Cmd (Result Error HttpResult a)
update info val =
  let 
    headDict = info.headers Update
    default =
      D.fromList
        [ ("Prefer", "return=representation")
        , ("Content-Type", "application/json")
        ]
    heads = D.toList <| D.union headDict default
    q = D.fromList [ info.idField, S.concat [ "eq", ".", toString <| info.getId val ] ]
    uurl = url info.url q
  in 
    patchWithHeaders info.encode info.decode uurl heads val
  

-- TODO figure out return of DELETE command postgrest
delete : CRUDInfo a -> a -> Cmd (Result Error (HttpResult ()))
delete info val =
  deleteById info (info.headers Delete) <| info.getId val


deleteById : CRUDInfo a -> Int -> Cmd (Result Error (HttpResult ()))
deleteById info id =
  let
    q = D.fromList [ info.idField, S.concat [ "eq", ".", toString <| id ] ]
    uurl = url info.url q
  in
    deleteWithHeaders uurl (info.headers Delete)


deleteNR : CRUDInfo a -> a -> Cmd (Result Error (Dict String String))
delete info val =
  deleteByIdNR info (info.headers Delete) <| info.getId val


deleteByIdNR : CRUDInfo a -> Int -> Cmd (Result Error (Dict String String))
deleteByIdNR info id =
  let
    q = D.fromList [ info.idField, S.concat [ "eq", ".", toString <| id ] ]
    uurl = url info.url q
  in
    deleteWithHeadersNR uurl (info.headers Delete)


defaultJsonSets : Settings
defaultJsonSets = { defaultSettings 
                  | desiredResponseType = Just "application/json"
                  }

sendForJson : Settings -> Request -> Decoder a -> Task Error a
sendForJson s r d = fromJson d <| send s r

sendForJsonHeaders : Settings -> Request -> Decoder a -> Task Error (HttpResult a)
sendForJsonHeaders s r d = fromJsonHeaders d <| send s r

defaultSendForJson : Request -> Decoder a -> Task Error a
defaultSendForJson = sendForJson defaultJsonSets

defaultSendForJsonHeaders : Request -> Decoder a -> Task Error (HttpResult a)
defaultSendForJsonHeaders = sendForJsonHeaders defaultJsonSets

sendCommand : Settings -> Request -> Decoder a -> (Error -> b) -> (HttpResult a -> b) -> Cmd b))
sendCommand s r d err encapsulate =
  T.perform err encapsulate <| sendForJsonHeaders s r d

defaultSend : Request -> Decoder a -> Cmd (Result Error (HttpResult a))
defaultSend r d = sendCommand defaultJsonSets r d Err Ok

makeGet : String -> List (String, String) -> Request
makeGet url headers =
  { verb = "GET"
  , headers = headers
  , url = url
  , body = empty
  }

makePost : String -> String -> List (String,String) -> Request
makePost url body headers = 
  { verb = "POST"
  , headers = headers
  , url = url
  , body = Http.string body
  }

makePatch : String -> String -> List (String,String) -> Request
makePatch url body headers = 
  { verb = "PATCH"
  , headers = headers
  , url = url
  , body = Http.string body
  }

makeDelete : String -> List (String,String) -> Request
makeDelete url headers =
  { verb = "DELETE"
  , headers = headers
  , url = url
  , body = empty
  }

postWithHeaders : Encoder a -> Decoder b -> String -> List (String,String) -> a -> Cmd (Result Error (b,Dict String String))
postWithHeaders enc dec url headers obj =
  defaultSend (makePost url (code enc obj) headers) dec

patchWithHeaders : Encoder a -> Decoder b -> String -> List (String,String) -> a -> Cmd (Result Error (b,Dict String String))
patchWithHeaders enc dec url headers obj =
  defaultSend (makePatch url (code enc obj) headers) dec

getWithHeaders : Decoder a -> String -> List (String,String) -> Cmd (Result Error (HttpResult a))
getWithHeaders decoder url headers =
  defaultSend (makeGet url headers) decoder

deleteWithHeaders : String -> List (String,String) -> Cmd (Result Error ((),Dict String String))
deleteWithHeaders url headers =
    defaultSend (makeDelete url headers) (succeed ())

-- get rid of return value
deleteWithHeadersNR : String -> List (String,String) -> Cmd (Result Error (Dict String String))
deleteWithHeadersNR 
  Cmd.map 
    (\result -> case result of
      Err _ -> 
        result
      Ok (r,heads) ->
        Ok heads
    )
    <| deleteWithHeaders url headers

-- based on the code in evancz's Http library
fromJsonHeaders : Decoder a -> Task RawError Response -> Task Error (HttpResult a)
fromJsonHeaders decoder response =
  let decode str =
    case decodeString decoder str of 
      Ok v -> T.succeed v
      Err msg -> T.fail (UnexpectedPayload msg)
  in
    T.mapError promoteError response
      `T.andThen` handleResponse decode

handleResponse : (String -> Task Error a) -> Response -> Task Error (HttpResult a)
handleResponse task response =
  if 200 <= response.status && response.status < 300 then
    -- No error
    case response.value of
      Text str ->
        (task str)
        `T.andThen`
        (\result ->
          T.succeed (result, response.headers))

      _ ->
        T.fail (UnexpectedPayload "Response body is a blob, expecting a string.")
  else 
    T.fail (BadResponse response.status response.statusText)
    

promoteError : RawError -> Error
promoteError raw =
  case raw of
    RawTimeout -> Timeout
    RawNetworkError -> NetworkError

