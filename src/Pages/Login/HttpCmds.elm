module Pages.Login.HttpCmds exposing (..)


import Http exposing (..)
import Utils.Http exposing (..)
import Pages.Login.Model exposing (..)
import Json.Decode exposing (..)
import Types.Login exposing (..)

loginUrl = "/login"

fetchAuthToken : Cred -> Cmd Msg
fetchAuthToken  cred = 
  Cmd.map 
    (\res -> 
        case res of
          Err err ->
            Error <| toString err

          Ok (token,query) ->
            ReceiveLogin {cred | authtoken = token, isvalid = True }
    )
    <| defaultSend { verb = "POST"
                   , headers = [("Content-type", "application/x-www-form-urlencoded")]
                   , url = loginUrl
                   , body = Http.string  ("user=" ++ cred.id ++ "&pass=" ++  cred.pass )
                   } 
                  Json.Decode.string 
