module Pages.Login.Model exposing (..)

import Nav.Operations exposing (Operation)

import Types.Login exposing (..)
import Types.Visit exposing (Visit)


-- messages that will be used by
-- CrudOp in Update.elm
type Msg 
  =  CrudOp Operation
  | Error String
  | ReceiveLogin Cred
  | NoOp
  | PassUp String   -- the following are gross un-abstracted queries
  | IdUp String
  | SetAuth
  


type alias Model =
  { cred: Cred  -- cred.id, cred.authtoken, cred.isvalid
  , error: String
  }
