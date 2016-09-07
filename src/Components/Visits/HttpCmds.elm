module Components.Visits.HttpCmds exposing (..)

import Types.Visit exposing (..)
import Types.Visit.JsonDecoders exposing (..)

import Http exposing (get)
import Task exposing (perform)



testUrl : String
testUrl = "/db/visit?pid=eq."


getVisitsCmd : (String -> msg) -> (List Visit -> msg) -> Int -> Cmd msg
getVisitsCmd err vstuff n = 
  perform (err << toString) vstuff
    <| get visitListDecoder (testUrl ++ toString n)

