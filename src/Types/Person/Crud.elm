module Types.Person.CRUD exposing (..)

import Utils.Http.Handlers exposing (..)
import Utils.Http exposing (urlstring)

import Types.Person exposing (..)
import Types.Person.Json exposing (..)


personCrudInfo : CrudInfo Person
personCrudInfo = 
  { url = urlstring ++ "person"
  , search =
      Just
        { url = urlstring ++ "person_search_view"
        , decode = decode
        }
  , getId = (.pid)
  , idField = "pid"
  , decode = encode
  , encode = decode
  , headers = always []
  , noSearch = False
  }


