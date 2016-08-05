module Pages.People.Search exposing
  ( peopleKeyInfo
  , PeopleKey(..)
  )

import Components.Search.Model exposing (..)

type PeopleKey
  = Name
  | FName
  | LName
  | Age
  | Sex
  | Hand
  | Id
  | Ids
  | DOB
  | AddDate
  | Source
  | LunaID

peopleKeyInfo : KeyInfo PeopleKey
peopleKeyInfo = 
  { searchKeys =
      [ Name
      , FName
      , LName
      , Age
      , Sex
      , Hand
      , LunaID
      , Id
      , Ids
      , DOB
      , AddDate
      , Source
      ]
  , sortKeys = 
      [ Name
      , FName
      , LName
      , Age
      , Sex
      , Hand
      , Id
      , LunaID
      , DOB
      , AddDate
      , Source
      ]
  , defaultKey = Name
  , keyNames = keyNames
  , prettyKeyNames = prettyKeyName
  , keyDefault = keyDefault
  }


prettyKeyName : PeopleKey -> (String, Maybe String)
prettyKeyName key = case key of
  Name -> ("Name", Just "Full Name")
  FName -> ("First", Just "First Name")
  LName -> ("Last", Just "Last Name")
  Age -> ("Age", Nothing)
  Sex -> ("Sex", Nothing)
  Hand -> ("Hand", Nothing)
  Id -> ("Id", Nothing)
  LunaID -> ("LunaID", Nothing)
  Ids -> ("Ids", Nothing)
  DOB -> ("Birth", Just "Date of Birth")
  AddDate -> ("Add", Just "Add Date")
  Source -> ("Source", Nothing)

keyNames : PeopleKey -> (String, List String)
keyNames key = case key of
  Name ->
    ("fullname", ["name"])

  FName ->
    ("fname", ["first name","first"])
    
  LName ->
    ("lname", ["last name","last"])

  Age ->
    ("curage", ["age"])

  Sex ->
    ("sex", [])

  Hand ->
    ("hand", [])

  Id ->
    ("pid", ["id"])

  LunaID ->
    ("lunaid", ["lid"])

  Ids ->
    ("ids", [])

  DOB ->
    ("dob", ["born","bdate","birthdate","birth","date of birth"])

  AddDate ->
    ("adddate", ["add date","add"])

  Source ->
    ("source", [])

keyDefault : PeopleKey -> OpTag
keyDefault key = case key of
  Name -> ILikeT
  FName -> ILikeT
  LName -> ILikeT
  Sex -> ILikeT
  Hand -> ILikeT
  Id -> RangeT
  LunaID -> EqT
  Ids -> ContainsT
  Age -> RangeT
  DOB -> RangeT
  AddDate -> RangeT
  Source -> ILikeT


  

