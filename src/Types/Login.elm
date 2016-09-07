module Types.Login exposing (..)


type alias Cred =
  { id        : String   -- 
  , pass      : String   -- password
  , isvalid   : Bool     --
  , authtoken : String   --
  }

newCred : Cred
newCred  = 
  { id = ""
  , pass = ""
  , isvalid = False
  , authtoken = ""
  }

