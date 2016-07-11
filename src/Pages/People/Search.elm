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

peopleKeyInfo : KeyInfo PeopleKey
peopleKeyInfo = 
  { allKeys = allPeopleKeys
  , defaultKey = Name
  , keyNames = keyNames
  , keyDefault = keyDefault
  }

allPeopleKeys : List PeopleKey
allPeopleKeys =
  [ Ids, Name, Age, Sex, Hand, Id, DOB ]


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
    ("dob", ["born","bdate","birthdate"])

keyDefault : PeopleKey -> OpTag
keyDefault key = case key of
  Name -> ILikeT
  Sex -> ILikeT
  Hand -> ILikeT
  Id -> RangeT
  Ids -> ContainsT
  Age -> RangeT
  DOB -> RangeT


  

