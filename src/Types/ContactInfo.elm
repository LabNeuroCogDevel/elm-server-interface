module Types.ContactInfo exposing (..)

type alias Cid = Int

type alias Contact = 
  { cid : Cid
  , contactType : String
  , content : String
  , notGood : Bool
  , notes : String
  }

type alias ContactInfo =
  { belongsTo : Int -- PID of person this info belongs to
  , relation : String
  , name : String
  , lastContact : Maybe String
  , contacts : List Contact
  }

