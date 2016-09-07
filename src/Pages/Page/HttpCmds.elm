module Pages.Page.HttpCmds exposing (..)


import Http exposing (..)
import Core.HttpCmds exposing (..)
import Pages.Page.Model exposing (..)
import Json.Decode exposing (..)
import Types.Study.JsonDecoders exposing (..)

studyUrl = "/db/study"

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

