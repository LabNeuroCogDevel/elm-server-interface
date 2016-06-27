module Components.Contacts.Model exposing (..)


type alias Model = (String,Maybe (List ContactInfo))

type Msg
  = NoOp
  | Error String
  | NumberChange String
  | ContactStuff (List ContactInfo)

