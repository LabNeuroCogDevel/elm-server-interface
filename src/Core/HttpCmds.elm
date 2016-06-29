module Core.HttpCmds exposing (..)

import Http exposing (..)
import Types.Person.JsonDecoders exposing (..)
import Json.Decode exposing (..)
import Nav.Paging exposing (..)
import Core.Model exposing (..)
--import Result exposing (..)

import Task exposing (Task)
import Dict exposing (Dict)

import String
import Regex

import Task as T
import Result as R
import Dict as D
import List as L


hostname : String
hostname = "localhost"

portnum : Int
portnum = 3003

urlstring : String
urlstring = "http://localhost:3003/"

personUrl : String
personUrl = urlstring ++ "person_search_view"

makePersonUrl : List (String, String) -> String
makePersonUrl = url personUrl


defaultJsonSets = { defaultSettings 
                  | desiredResponseType = Just "application/json"
                  }

sendForJson : Settings -> Request -> Decoder a -> Task Error a
sendForJson s r d = fromJson d <| send s r

sendForJsonHeaders : Settings -> Request -> Decoder a -> Task Error (a, Dict String String)
sendForJsonHeaders s r d = fromJsonHeaders d <| send s r

defaultSendForJson : Request -> Decoder a -> Task Error a
defaultSendForJson = sendForJson defaultJsonSets

defaultSendForJsonHeaders : Request -> Decoder a -> Task Error (a, Dict String String)
defaultSendForJsonHeaders = sendForJsonHeaders defaultJsonSets

sendCommand : Settings -> Request -> Decoder a -> (Error -> b) -> ((a,Dict String String) -> b) -> Cmd b
sendCommand s r d err encapsulate =
  T.perform err encapsulate <| sendForJsonHeaders s r d

defaultSend : Request -> Decoder a -> Cmd (Result Error (a,Dict String String))
defaultSend r d = sendCommand defaultJsonSets r d Err Ok

makeGet : String -> List (String, String) -> Request
makeGet url headers =
  { verb = "GET"
  , headers = headers
  , url = url
  , body = empty
  }

getWithHeaders : Decoder a -> String -> List (String,String) -> Cmd (Result Error (a,Dict String String))
getWithHeaders decoder url headers =
  defaultSend (makeGet url headers) decoder

-- based on the code in evancz's Http library
fromJsonHeaders : Decoder a -> Task RawError Response -> Task Error (a, Dict String String)
fromJsonHeaders decoder response =
  let decode str =
    case decodeString decoder str of 
      Ok v -> T.succeed v
      Err msg -> T.fail (UnexpectedPayload msg)
  in
    T.mapError promoteError response
      `T.andThen` handleResponse decode

handleResponse : (String -> Task Error a) -> Response -> Task Error (a,Dict String String)
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

