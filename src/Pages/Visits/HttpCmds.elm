module Pages.Visits.HttpCmds exposing (..)


import Http exposing (..)
import Core.HttpCmds exposing (..)
import Pages.Visits.Model exposing (..)
import Json.Decode exposing (..)
import Types.Visit.JsonDecoders exposing (..)

visitUrl = "http://localhost:3003/visit"

fetchVisits : Cmd Msg
fetchVisits = 
  Cmd.map 
    (\res -> 
        case res of
          Err err ->
            Error <| toString err

          Ok (visits,query) ->
            ReceiveVisits visits
    )
    <| getWithHeaders (list visitDecoder) visitUrl []
    {--
        [ ("Range","0-24")
        , ("Range-Unit", "items")
        ]
    --}

