module Utils.Http exposing (..)


import Http exposing (..)
import Json.Decode exposing (..)
import Nav.Paging exposing (..)
import Core.Model exposing (..)
import Utils.JsonEncoders exposing (..)
import Utils.Http.Tag exposing (..)
--import Result exposing (..)

import Task exposing (Task)
import Dict exposing (Dict)

import String
import Regex

import Platform.Cmd as Cmd

import Maybe as M
import String as S
import Task as T
import Result as R
import Dict as D
import List as L


type alias HttpResult a = (a, Dict String String)

type alias SearchInfo a = 
  { url : String
  , decode : Decoder a
  }

type alias CrudInfo a = 
  { url : String
  , search : Maybe (SearchInfo a)
  , getId : a -> Int
  , idField : String
  , decode : Decoder a
  , encode : Encoder a
  , headers : Tag -> Dict String String
  , noSearch : Bool
  }

type alias Viewer a = SearchInfo a


type alias SuccessHandler a r = HttpResult a -> r
type alias ErrorHandler r = Error -> r

type alias Handler a r = (SuccessHandler a r, ErrorHandler r)


handle : Handler a r -> Cmd (Result Error (HttpResult a)) -> Cmd r
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
create : CrudInfo a -> a -> Cmd (Result Error (HttpResult a))
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
read : CrudInfo a -> Dict String String -> Cmd (Result Error (HttpResult (List a)))
read info query =
  let 
    nonsearch = getWithHeaders (list info.decode) (url info.url (D.toList query)) (D.toList <| info.headers Read)
  in case info.search of
    Just srchinfo ->
      if info.noSearch
      then
        nonsearch
      else
        getWithHeaders (list srchinfo.decode) (url srchinfo.url (D.toList query)) (D.toList <| info.headers Read)

    Nothing ->
      nonsearch

--
update : CrudInfo a -> a -> Cmd (Result Error (HttpResult a))
update info val =
  let 
    headDict = info.headers Update
    default =
      D.fromList
        [ ("Prefer", "return=representation")
        , ("Content-Type", "application/json")
        ]
    heads = D.toList <| D.union headDict default
    q = [( info.idField, S.concat [ "eq", ".", toString <| info.getId val ] )]
    uurl = url info.url q
  in 
    patchWithHeaders info.encode info.decode uurl heads val
  

-- TODO figure out return of DELETE command postgrest
delete : CrudInfo a -> a -> Cmd (Result Error (HttpResult ()))
delete info val =
  deleteById info <| info.getId val


deleteById : CrudInfo a -> Int -> Cmd (Result Error (HttpResult ()))
deleteById info id =
  let
    q = [( info.idField, S.concat [ "eq", ".", toString <| id ] )]
    uurl = url info.url q
  in
    deleteWithHeaders uurl (D.toList <| info.headers Delete)


deleteNR : CrudInfo a -> a -> Cmd (Result Error (Dict String String))
deleteNR info val =
  deleteByIdNR info <| info.getId val


deleteByIdNR : CrudInfo a -> Int -> Cmd (Result Error (Dict String String))
deleteByIdNR info id =
  let
    q = [( info.idField, S.concat [ "eq", ".", toString <| id ] )]
    uurl = url info.url q
  in
    deleteWithHeadersNR uurl (D.toList <| info.headers Delete)


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

sendCommand : Settings -> Request -> Decoder a -> (Error -> b) -> (HttpResult a -> b) -> Cmd b
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
deleteWithHeadersNR url headers =
  Cmd.map 
    (\result -> case result of
      Err x -> 
        Err x
      Ok (r,heads) ->
        Ok heads
    )
    <| deleteWithHeaders url headers


getPagingInfoFromHeader : Int -> Dict String String -> Maybe PagingInfo
getPagingInfoFromHeader itemsPerPage headers =
  (D.get "Content-Range" headers)
  `M.andThen`
  (\contentRange -> case contentRange of
    "*/0" ->
      Just <| makePagingInfo itemsPerPage 0 0 0
    range -> 
      let
        ints = L.map String.toInt <|
          Regex.split Regex.All
                      (Regex.regex "[-/]")
                      range
      in case ints of
        [ Ok l, Ok u, Ok m ] ->
          Just <| makePagingInfo itemsPerPage l u m
        
        _ ->
          Nothing
  )



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

