module Types.Person exposing (..)

import Date exposing (Date)

type alias Pid = Int

type alias Person =
  { pid : Pid
  , fullname : Maybe String
  , dob : Maybe String
  , sex : Maybe String
  , hand : Maybe String
  , ids : List String
  }

new : Person
new = 
  { pid = 0
  , fullname = Nothing
  , dob = Nothing
  , sex = Nothing
  , hand = Nothing
  , ids = []
  }

modifyPerson : Person -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> List String -> Person
modifyPerson = Person << (.pid)


