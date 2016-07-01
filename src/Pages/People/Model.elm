module Pages.People.Model exposing (..)


import Nav.Routes exposing (..)
import Form.Validate as Val exposing (..)
import Components.Search.Model exposing (..)

import Nav.RQ exposing (RQ,getQueryParam)
import Nav.Queries exposing (Query)
import Nav.Operations exposing (Operation)
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
  | CrudOp Operation
  | ViewPerson Pid
  | EditPerson Pid
  | SavePerson Person
  | CancelEdit
  | RQChanged RQ
  | SearchStringChanged String
  | PeopleSearch
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
  , nameFilter : String
  , searchString : String
  , paging : PagingInfo
  , pagingErr : String
  , routeQuery : RQ
  }

buildSearch : Model -> Search
buildSearch model = { key = Name, operator = ILike model.nameFilter } :: (parseSearch model.searchString)

type CustomError
  = NoError


personFields : Person -> List (String, Field.Field)
personFields p = 
  [ ("fullname", Field.Text <| withDefault "" p.fullname )
  , ("dob", Field.Text <| withDefault "" p.dob )
  , ("sex", Field.Text <| withDefault "" p.sex )
  , ("hand", Field.Text <| withDefault "" p.hand )
  , ("adddate", Field.Text <| withDefault "" p.adddate )
  , ("source", Field.Text <| withDefault "" p.source )
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
  , nameFilter = ""
  , searchString = withDefault "" (getQueryParam "search" rq)
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
  |: (get "adddate" <| oneOf [ Val.map (always Nothing) emptyString
                             , Val.map Just string
                             ])
  |: (get "source" <| oneOf [ Val.map (always Nothing) emptyString
                            , Val.map Just string
                            ])
  --|: (get "ids" <| oneOf [ Val.map (always []) emptyString, Val.map words string])


