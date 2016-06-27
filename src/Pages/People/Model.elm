module Pages.People.Model exposing (..)


import Nav.Routes exposing (..)
import Form.Validate as Val exposing (..)

import Nav.RQ exposing (RQ)
import Nav.Paging exposing (PagingInfo,makePagingInfo)
import Form exposing (Form)
import String exposing (words)
import Maybe exposing (withDefault)
import Types.Person exposing (Person, Pid, modifyPerson)
import Types.ContactInfo exposing (ContactInfo)

import Maybe

import Form.Field as Field
import Types.Person as Person


type Msg
  = NoOp
  | FormMsg Form.Msg
  | EditFormMsg Form.Msg
  | SubmitPerson Person
  | ViewPerson Pid
  | EditPerson Pid
  | SavePerson Person
  | CancelEdit
  | RQChanged RQ
  | ContactInfo (List ContactInfo)
  | NavigateTo (Maybe Route) (Maybe Query)
  | ChangePeopleList (List Person) PagingInfo


type alias Model = 
  { form : Form CustomError Person
  , editForm : Form CustomError Person
  , people : List Person
  , id : Int
  , editpid : Maybe Int
  , activepid : Maybe Int
  , contactInfo : Maybe (List ContactInfo)
  , paging : PagingInfo
  , pagingErr : String
  , routeQuery : RQ
  }


type CustomError
  = NoError


personFields : Person -> List (String, Field.Field)
personFields p = 
  [ ("fullname", Field.Text <| withDefault "" p.fullname)
  , ("dob", Field.Text <| withDefault "" p.dob)
  , ("sex", Field.Text <| withDefault "" p.sex)
  , ("hand", Field.Text <| withDefault "" p.hand)
  , ("ids", Field.Text <| String.join " " p.ids)
  ]


buildEditForm : Person -> Form CustomError Person
buildEditForm p = Form.initial (personFields p) (validate p)


initModel : RQ -> Model
initModel rq =
  { form = buildEditForm Person.new
  , editForm = buildEditForm Person.new
  , people = []
  , id = 0
  , editpid = Nothing
  , activepid = Nothing
  , contactInfo = Nothing
  , paging = makePagingInfo 25 1 1 1
  , pagingErr = ""
  , routeQuery = rq
  }


genders : List String
genders = [ "M", "F" ]


hands : List String
hands = [ "L", "R", "LR", "U" ]


(|:) = Val.apply

validate : Person -> Validation CustomError Person
validate p = Val.succeed (modifyPerson p)
{--
  |: (get "pid" <| oneOf [ Val.map (always Nothing) emptyString
                         , Val.map Just int
                         ])
--}
  |: (get "fullname" <| oneOf [ Val.map (always Nothing) emptyString
                              , Val.map Just string
                              ])
  |: (get "dob" <| oneOf [ Val.map (always Nothing) emptyString
                         , Val.map Just string
                         ])
  |: (get "sex" <| oneOf [ Val.map (always Nothing) emptyString
                         , Val.map Just <|
                             string `andThen` includedIn genders
                         ])
  |: (get "hand" <| oneOf [ Val.map (always Nothing) emptyString
                          , Val.map Just <|
                              string `andThen` includedIn hands
                          ])
  |: (get "ids" <| oneOf [ Val.map (always []) emptyString, Val.map words string])


