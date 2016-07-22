module Pages.Studies.HttpCmds exposing (..)


import Http exposing (..)
import Utils.Http exposing (..)
import Pages.Studies.Model exposing (..)
import Json.Decode exposing (..)
import Types.Study.JsonDecoders exposing (..)

studyUrl = "http://localhost:3003/study"

fetchStudies : Cmd Msg
fetchStudies = 
  Cmd.map 
    (\res -> 
        case res of
          Err _ ->
            NoOp

          Ok (studies,query) ->
            ReceiveStudies studies
    )
    <| getWithHeaders (list studyDecoder) studyUrl []

