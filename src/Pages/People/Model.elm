module Pages.People.Model exposing (..)


import Nav.Routes exposing (..)
import Form.Validate as Val exposing (..)
import Components.Search.Model exposing (..)
import Pages.People.Search exposing (..)

import Nav.RQ exposing (RQ,getQueryParam)
import Nav.Queries exposing (Query)
import Nav.Operations exposing (Operation)
import Nav.Paging exposing (PagingInfo,makePagingInfo)
import Form exposing (Form)
import String exposing (words)
import Maybe exposing (withDefault)
import Types.Person exposing (Person, Pid, modifyPerson)
import Types.ContactInfo exposing (ContactInfo)
import Utils.Date exposing (dateToString)
import Types.Person.Hand exposing (handToString,hand,Hand)
import Types.Person.Sex exposing (sexToString,sex,Sex)
import Form.Error exposing (Error(CustomError,InvalidFormat))
import Json.Decode exposing (Decoder, decodeString)
import ISO8601 exposing (Time)
import Utils.JsonDecoders exposing (date)


import Maybe

import Maybe as M
import Utils.JsonDecoders as JD

import Form.Field as Field
import Types.Person as Person


type Msg
  = NoOp
  | FormMsg Form.Msg
  | EditFormMsg Form.Msg
  | SubmitPerson Person
  | SubmittedPerson Person
  | CrudOp Operation
  | ViewPerson Pid
  | EditPerson Pid
  | SavePerson Person
  | SavedPerson Person
  | CancelEdit
  | RQChanged RQ
  | SearchStringChanged String
  | PeopleSearch
  | OrdStringChanged String
  | OrdEnter
  | ContactInfo (List ContactInfo)
  | NavigateTo (Maybe Route) (Maybe Query)
  | ChangePeopleList (List Person) PagingInfo
  | ChangeSorting PeopleKey


type alias Model = 
  { form : Form CustomError Person
  , editForm : Form CustomError Person
  , people : List Person
  , id : Int
  , editpid : Maybe Int
  , activepid : Maybe Int
  , contactInfo : Maybe (List ContactInfo)
  , fnameFilter : String
  , lnameFilter : String
  , searchString : String
  , ordString : String
  , paging : PagingInfo
  , pagingErr : String
  , routeQuery : RQ
  }


buildSearch : Model -> Search PeopleKey
buildSearch model = 
  ( if model.fnameFilter /= ""
    then
      [{ key = FName, operator = ILike model.fnameFilter }]
    else
      []
  ) ++
  ( if model.lnameFilter /= ""
    then
      [{ key = LName, operator = ILike model.lnameFilter }]
    else
      []
  ) ++
  (parseSearch peopleKeyInfo model.searchString)


-- get clean search string
searchString : Model -> String
searchString model = model.searchString


buildOrdering : Model -> Ordering PeopleKey
buildOrdering model = 
  parseOrder peopleKeyInfo model.ordString

-- get clean order string
ordString : Model -> String
ordString = orderingToString peopleKeyInfo << buildOrdering

getSortStatus : PeopleKey -> Model -> SortStatus
getSortStatus key = Components.Search.Model.getSortStatus key << buildOrdering


type CustomError
  = NoError


personFields : Person -> List (String, Field.Field)
personFields p = 
  [ ("fname", Field.Text  p.fname )
  , ("lname", Field.Text p.lname )
  , ("dob", Field.Text <| dateToString p.dob )
  , ("sex", Field.Text <| sexToString p.sex )
  , ("hand", Field.Text <| handToString p.hand )
  , ("adddate", Field.Text <| withDefault "" <| M.map dateToString p.adddate )
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
  , fnameFilter = ""
  , lnameFilter = ""
  , searchString = withDefault "" (getQueryParam "search" rq)
  , ordString = withDefault "" (getQueryParam "order" rq)
  , paging = makePagingInfo 25 1 1 1
  , pagingErr = ""
  , routeQuery = rq
  }


genders : List String
genders = [ "M", "F", "U", "O", "u", "o", "m", "f" ]


hands : List String
hands = [ "L", "R", "U", "A", "l", "r", "u", "a" ]

jsonValidate : Decoder a -> Validation CustomError a
jsonValidate dec field = 
  case Field.asString field of
    Just s ->
      case decodeString dec ("\""++s++"\"") of
        Err str ->
          Err InvalidFormat
        Ok val ->
          Ok val
      
    Nothing ->
      Err <| CustomError NoError

sexValidate : Validation CustomError Sex
sexValidate = jsonValidate sex

handValidate : Validation CustomError Hand
handValidate = jsonValidate hand

dateValidate : Validation CustomError Time
dateValidate = jsonValidate JD.date

(|:) = Val.apply

validate : Person -> Validation CustomError Person
validate p = Val.succeed (modifyPerson p)
  |: (get "fname" string)
  |: (get "lname" string)
  |: (get "dob" dateValidate)
  |: (get "sex" sexValidate)
  |: (get "hand" handValidate)
  |: (get "adddate" <| oneOf [ Val.map (always Nothing) emptyString
                             , Val.map Just dateValidate
                             ])
  |: (get "source" <| oneOf [ Val.map (always Nothing) emptyString
                            , Val.map Just string
                            ])
  --|: (get "ids" <| oneOf [ Val.map (always []) emptyString, Val.map words string])


