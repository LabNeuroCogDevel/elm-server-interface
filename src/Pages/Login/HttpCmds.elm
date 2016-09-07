module Pages.Login.HttpCmds exposing (..)


import Http exposing (..)
import Utils.Http exposing (..)
import Pages.Login.Model exposing (..)
import Json.Decode exposing (..)
import Types.Login exposing (..)

loginUrl = "http://localhost:3001/login"

fetchAuthToken : Cred -> Cmd Msg
fetchAuthToken  cred = 
  Cmd.map 
    (\res -> 
        case res of
          Err err ->
            Error <| toString err

          Ok (token,query) ->
            ReceiveLogin {cred | authtoken = token}
    )
    <| defaultSend (makePost loginUrl "" [("user",cred.id), ("pass",cred.pass)]) Json.Decode.string --toString
