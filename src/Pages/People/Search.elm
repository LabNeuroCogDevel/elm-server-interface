module Pages.People.Search exposing
  ( peopleKeyInfo
  , PeopleKey(..)
  )

import Components.Search.Model exposing (..)

type PeopleKey
  = Name
  | Age
  | Sex
  | Hand
  | Id
  | Ids
  | DOB
  | AddDate
  | Source

peopleKeyInfo : KeyInfo PeopleKey
peopleKeyInfo = 
  { searchKeys =
      [ Name
      , Age
      , Sex
      , Hand
      , Id
      , Ids
      , DOB
      , AddDate
      , Source
      ]
  , sortKeys = 
      [ Name
      , Age
      , Sex
      , Hand
      , Id
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
  Name -> ("Name", Just "Full name")
  Age -> ("Age", Nothing)
  Sex -> ("Sex", Nothing)
  Hand -> ("Hand", Nothing)
  Id -> ("Id", Nothing)
  Ids -> ("Ids", Nothing)
  DOB -> ("Birth", Just "Date of Birth")
  AddDate -> ("Add", Just "Add Date")
  Source -> ("Source", Nothing)

keyNames : PeopleKey -> (String, List String)
keyNames key = case key of
  Name ->
    ("fullname", ["name"])

  Age ->
    ("curage", ["age"])

  Sex ->
    ("sex", [])

  Hand ->
    ("hand", [])

  Id ->
    ("pid", ["id"])

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
  Sex -> ILikeT
  Hand -> ILikeT
  Id -> RangeT
  Ids -> ContainsT
  Age -> RangeT
  DOB -> RangeT
  AddDate -> RangeT
  Source -> ILikeT


  

