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
import Types.ContactInfo exposing (ContactInfo,Contact,newContact,modifyContact,modifyContactInfo)
import Types.Visit exposing (Visit,newVisit)
import Utils.Date exposing (dateToString)
import Types.Person.Hand exposing (handToString,hand,Hand)
import Types.Person.Sex exposing (sexToString,sex,Sex)
import Form.Error exposing (Error(CustomError,InvalidFormat))
import Json.Decode exposing (Decoder, decodeString)
import ISO8601 exposing (Time)
import Utils.JsonDecoders exposing (date)


import Maybe

import List as L
import Maybe as M
import Utils.JsonDecoders as JD

import Form.Field as Field
import Types.Person as Person
import Components.Search.Model as Search
import Components.Search.Update as SearchU


type Msg
  = NoOp
  | FormMsg Form.Msg
  | EditFormMsg Form.Msg
  | NewContactFormMsg Form.Msg
  | SearchMsg (SearchMsg PeopleKey)
  | SubmitPerson Person
  | SubmittedPerson Person
  | CrudOp Operation -- currently unused, possible replacement for the ViewPerson messages and whatever
  | ViewPerson Pid
  | EditPerson Pid
  | SavePerson Person
  | SavedPerson Person
  | NewContactFor Person ContactInfo
  | CancelEdit
  | RQChanged RQ
  | ContactInfo Int (List ContactInfo)
  | ReceiveVisits Int (List Visit)
  --| Visits Int (List Visit)
  | NavigateTo (Maybe Route) (Maybe Query)
  | ChangePeopleList (List Person) PagingInfo
  | DebugLog String

--type alias ContModalModel 

type alias Model = 
  { form : Form CustomError Person
  , editForm : Form CustomError Person
  , visitForm : Form CustomError Visit
  , contactForm : Form CustomError Contact
  --, newContactModalModel : 
  , people : List Person
  , editpid : Maybe Int
  , activepid : Maybe Int
  , searchModel : SearchModel PeopleKey
  , paging : PagingInfo
  , pagingErr : String
  , routeQuery : RQ
  }

updatePerson : Int -> (Person -> Person) -> Model -> Model
updatePerson id update model = 
  { model 
  | people = L.map (\person -> if person.pid == id then update person else person) model.people
  }

buildSearch : Model -> Search PeopleKey
buildSearch model = 
  totalSearch model.searchModel


-- get clean search string
searchString : Model -> String
searchString = Search.searchString << (.searchModel)

updateSearchString : String -> Model -> Model
updateSearchString sstr model = 
  { model
  | searchModel = Search.updateSearchString sstr model.searchModel
  }



buildOrder : Model -> Ordering PeopleKey
buildOrder model = 
  order model.searchModel

-- get clean order string
ordString : Model -> String
ordString = Search.orderString << (.searchModel)

updateOrdString : String -> Model -> Model
updateOrdString ostr model = 
  { model
  | searchModel = Search.updateOrderString ostr model.searchModel
  }

getSortStatus : PeopleKey -> Model -> SortStatus
getSortStatus key = Components.Search.Model.getSortStatus key << buildOrder


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

newContactFields : List (String, Field.Field)
newContactFields = 
  [ ("cType", Field.Text "")
  , ("content", Field.Text "")
  , ("notes", Field.Textarea "")
  ]

{--
visitFields : List (String, Field.Field)
visitFields = 
  [ ("vType", Field.Text "")
  , (
  ]
--}

initModel : RQ -> Model
initModel rq =
  let
    searchString = withDefault "" (getQueryParam "search" rq)
    ordString = withDefault "" (getQueryParam "order" rq)
  in
  { form = buildEditForm Person.new
  , editForm = buildEditForm Person.new
  , contactForm =
      Form.initial
        newContactFields
        (validateContact newContact)
  , visitForm = Form.initial [] (Val.succeed (newVisit 0))
  , people = []
  , editpid = Nothing
  , activepid = Nothing
  , searchModel =
      Search.updateOrderString ordString 
        <| Search.updateSearchString searchString
        <| SearchU.init peopleKeyInfo
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


validateContact : Contact -> Validation CustomError Contact
validateContact c = Val.succeed (modifyContact c)
  |: (get "cType" string)
  |: (get "content" string)
  |: (get "notes" string)



validateContactInfo : ContactInfo -> Validation CustomError ContactInfo
validateContactInfo ci = Val.succeed (modifyContactInfo ci)
  |: (get "relation" string)
  |: (get "name" string)
