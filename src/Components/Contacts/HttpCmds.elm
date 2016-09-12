module Components.Contacts.HttpCmds exposing (..)

import Types.ContactInfo exposing (..)
import Types.ContactInfo.JsonDecoders exposing (..)

import Http exposing (get)
import Task exposing (perform)



-- where we can get contact info
testUrl : String
testUrl = "/db/contacts_view?pid=eq."

-- make a command to get contact info
-- consumed by Update.elm in function ciCmd 
getCICmd : (String -> msg) -> (List ContactInfo -> msg) -> Int -> Cmd msg
getCICmd err cstuff n = 
  perform (err << toString) cstuff
    <| get ciListDecoder (testUrl ++ toString n)
