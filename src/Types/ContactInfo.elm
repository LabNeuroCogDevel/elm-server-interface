module Types.ContactInfo exposing (..)

type alias Cid = Int

type alias Contact = 
  { cid : Cid
  , contactType : String
  , content : String
  , notGood : Bool
  , notes : String
  }


newContact : Contact
newContact = 
  { cid = 0
  , contactType = ""
  , content = ""
  , notGood = False
  , notes = ""
  }


modifyContact : Contact -> String -> String -> String -> Contact
modifyContact c ctype content notes =
  { c
  | contactType = ctype
  , content = content
  , notes = notes
  }


type alias ContactInfo =
  { belongsTo : Int -- PID of person this info belongs to
  , relation : String
  , name : String
  , lastContact : Maybe String
  , contacts : List Contact
  }


newContactInfo : Int -> ContactInfo
newContactInfo n =
  { belongsTo = n
  , relation = ""
  , name = ""
  , lastContact = Nothing
  , contacts = []
  }


modifyContactInfo : ContactInfo -> String -> String -> ContactInfo
modifyContactInfo ci relation name =
  { ci
  | relation = relation
  , name = name
  }
